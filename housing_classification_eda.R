# Import required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(corrplot)
library(randomForest)

# Read the data
housing_data <- read.csv("dataset/05_Social_Housing.csv")

# ============= Initial Data Exploration =============
cat("\nDataset Structure:\n")
str(housing_data)

# print the first 10 rows of the dataset
print(head(housing_data, 10))

# ============= Data Preprocessing =============
# Function to convert flag columns to numeric
convert_flag_to_numeric <- function(x) {
  case_when(
    x == "Yes" ~ 1,
    x == "No" ~ 0,
    x == "U" ~ NA_real_,
    x == "n.a." ~ NA_real_
  )
}

# Function to handle NA values in character columns
handle_na <- function(x) {
  ifelse(x == "n.a." | x == "U" | x == "", NA, x)
}

# Preprocess all data first
preprocessed_data <- housing_data %>%
  mutate(
    # Convert target variable to binary
    Status = ifelse(ApplicationListStatusDesc == "ACTIVE", 1, 0),
    Status = as.factor(Status),
    
    # Convert flag columns to numeric
    DisabilityMod = convert_flag_to_numeric(DisabilityModRequestFlag),
    Homelessness = convert_flag_to_numeric(AtRiskOfOrExperiencingHomelessnessFlag),
    Disability = convert_flag_to_numeric(DisabilityApplicationFlag),
    Indigenous = convert_flag_to_numeric(IndigenousApplicationFlag),
    
    # Convert numeric columns
    BedroomsRequired = as.numeric(BedroomsRequired),
    MonthsOnRegister = as.numeric(MonthsOnHousingRegister),
    PeopleCount = as.numeric(PeopleonApplication)
  )

# Handle NA values in character columns and convert to factors
preprocessed_data <- preprocessed_data %>%
  mutate(across(where(is.character), handle_na)) %>%
  mutate(across(where(is.character), as.factor))

# Convert LettingArea1Desc ... LettingArea6Desc to factor
preprocessed_data <- preprocessed_data %>%
  mutate(across(starts_with("LettingArea"), as.factor))

# Print summary of preprocessed data
summary(preprocessed_data)

# Drop columns that:
# 1. we judge are not needed (row_id, ApplicationListCategory, RehousingListDesc, LettingArea1Desc ... LettingArea6Desc, ApplicationReceivedMonth, StateElectorate, HousingServiceCentre, LocalGovtAuthority)
# 2. have already been renamed during the first preprocessing
preprocessed_data <- preprocessed_data %>%
  select(-c(row_id, ApplicationListCategory, RehousingListDesc, 
            DisabilityModRequestFlag, AtRiskOfOrExperiencingHomelessnessFlag, 
            DisabilityApplicationFlag, IndigenousApplicationFlag, MonthsOnHousingRegister, PeopleonApplication, ApplicationListStatusDesc, LettingArea1Desc, LettingArea2Desc, LettingArea3Desc, LettingArea4Desc, LettingArea5Desc, LettingArea6Desc, ApplicationReceivedMonth, StateElectorate, HousingServiceCentre, LocalGovtAuthority))

summary(preprocessed_data)

# Check missing values
cat("\nMissing Values after preprocessing:\n")
colSums(is.na(preprocessed_data))

# Drop LanguagePreference because it is unlikely to be a good predictor and has too many missing values
cat("\nPercentage of missing values in LanguagePreference:\n")
mean(is.na(preprocessed_data$LanguagePreference)) # 96.94327% missing values
preprocessed_data <- preprocessed_data %>%
  select(-LanguagePreference)

# ============= Feature Selection Analysis =============
# 1. Chi-square test for categorical variables
categorical_features <- names(preprocessed_data)[sapply(preprocessed_data, is.factor)]

# exclude Status from the categorical features as it is the target variable
categorical_features <- setdiff(categorical_features, c("Status"))

chi_square_results <- data.frame(
  Feature = character(),
  ChiSquare = numeric(),
  PValue = numeric(),
  stringsAsFactors = FALSE
)

for(feature in categorical_features) {
  chi_test <- chisq.test(table(preprocessed_data[[feature]], 
                              preprocessed_data$Status))
  chi_square_results <- rbind(chi_square_results,
                             data.frame(Feature = feature,
                                      ChiSquare = chi_test$statistic,
                                      PValue = chi_test$p.value))
}

# View warnings from chi-square test
warnings()

# 2. Correlation analysis for numeric variables
numeric_features <- names(preprocessed_data)[sapply(preprocessed_data, is.numeric)]

correlation_matrix <- cor(preprocessed_data[numeric_features], 
                        use = "pairwise.complete.obs")

# View correlation matrix
print(correlation_matrix)

# plot the correlation matrix
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 0.7, tl.offset = 0.5)

# Since PeopleCount is highly correlated with BedroomsRequired, we can drop PeopleCount
preprocessed_data <- preprocessed_data %>%
  select(-PeopleCount)

# 3. Random Forest for feature importance
# Prepare data for random forest - keep factors as factors
rf_data <- preprocessed_data %>%
  select_if(~!all(is.na(.)))  # Remove columns with all NA

# Handle NA values
# First, let's see how many NAs we have in each column
cat("\nNA counts in RF data before handling:\n")
print(colSums(is.na(rf_data)))

# Impute NA values
# For numeric columns: replace NA with median
# For categorical/factor columns: replace NA with mode
impute_na <- function(x) {
  if(is.numeric(x)) {
    replace(x, is.na(x), median(x, na.rm = TRUE))
  } else if(is.factor(x)) {
    # For factor columns, replace with mode
    mode_val <- levels(x)[which.max(table(x))]
    x[is.na(x)] <- mode_val
    x
  } else {
    x  # Return unchanged if neither numeric nor factor
  }
}

rf_data_imputed <- rf_data %>%
  mutate(across(everything(), impute_na))

# Verify no more NAs
cat("\nNA counts after imputation:\n")
print(colSums(is.na(rf_data_imputed)))

# Split features and target
rf_features <- rf_data_imputed %>% select(-Status)
rf_target <- rf_data_imputed$Status

summary(rf_data_imputed)

# Before Random Forest, check cardinality of factor columns
cat("\nNumber of levels in each factor column:\n")
factor_levels <- sapply(rf_features, function(x) if(is.factor(x)) length(levels(x)) else 0)
print(factor_levels[factor_levels > 0])

# Function to handle high cardinality factors
handle_high_cardinality <- function(x, threshold = 52) {
  if(is.factor(x) && length(levels(x)) > threshold) {
    # Get frequency table
    freq_table <- table(x)
    # Keep top categories and group others
    top_categories <- names(sort(freq_table, decreasing = TRUE)[1:threshold])
    x <- as.character(x)
    x[!(x %in% top_categories)] <- "Other"
    return(factor(x))
  }
  return(x)
}

# Apply high cardinality handling
rf_features <- rf_features %>%
  mutate(across(where(is.factor), ~handle_high_cardinality(.)))

# Verify the changes
cat("\nNumber of levels after handling high cardinality:\n")
factor_levels_after <- sapply(rf_features, function(x) if(is.factor(x)) length(levels(x)) else 0)
print(factor_levels_after[factor_levels_after > 0])

# Run Random Forest with modified factors
set.seed(123)
rf_model <- randomForest(x = rf_features, 
                        y = rf_target,
                        importance = TRUE)
importance_scores <- importance(rf_model)

# Print feature importance in descending order
print("\nFeature Importance:")

# use gini importance to measure how much each feature reduces impurity (gini) across all the trees in the forest
# caveat: Gini importance may favor continuous or high-cardinality features (features with many unique values).
print(importance_scores[order(-importance_scores[,"MeanDecreaseGini"]), ])

# ============= Select Final Features =============
# Combine results from all methods
selected_features <- unique(c(
  # Statistically significant categorical features based on chi-square test
  chi_square_results$Feature[chi_square_results$PValue < 0.05],
  
  # Top 10 important features from Random Forest
  head(rownames(importance_scores[order(-importance_scores[,"MeanDecreaseGini"]),]), 10),
  
  # Numeric features with correlation > 0.1 with Status
  names(which(abs(cor(preprocessed_data$Status == 1, 
                     preprocessed_data[sapply(preprocessed_data, is.numeric)],
                     use = "pairwise.complete.obs")) > 0.1))
))

# Print selected features
cat("\nSelected features:\n")
print(selected_features)

# Create final dataset
final_data <- preprocessed_data %>%
  select(Status, all_of(selected_features))

# Save processed data
write.csv(final_data, "dataset/processed_housing_classification.csv", row.names = FALSE)

# drop ApplicationType
final_data <- final_data %>%
  select(-ApplicationType)

# get all possible values of FamilyType
unique(final_data$FamilyType)

# Print summary of final dataset
cat("\nFinal Dataset Structure:\n")
str(final_data)
cat("\nClass Distribution:\n")
table(final_data$Status)

# Function to convert FamilyType into multiple binary columns
convert_family_type <- function(data) {
  data %>%
    mutate(
      # Single (1) vs Couple (0)
      is_single = case_when(
        grepl("^(01|02|03|04|09)", FamilyType) ~ 1,  # Single person or single parent
        TRUE ~ 0
      ),
      
      # Has children (1) vs No children (0)
      has_children = case_when(
        grepl("(Parent|Children)", FamilyType) ~ 1,
        TRUE ~ 0
      ),
      
      # Number of children: 0, 1, 2, 3
      # no need to convert to factor as it is already a numeric column
      children_count = case_when(
        grepl("Parent, 1 Child|Couple, 1 Child", FamilyType) ~ 1,
        grepl("Parent, 2 Children|Couple, 2 Children", FamilyType) ~ 2,
        grepl("Parent, 3 Children|Couple, 3 Children", FamilyType) ~ 3,
        TRUE ~ 0
      ),
      
      # Senior (1) vs Non-senior (0)
      is_senior = case_when(
        grepl("Over 55", FamilyType) ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      # Convert to factors but keep 0/1 as levels
      is_single = factor(is_single, levels = c(0, 1)),
      has_children = factor(has_children, levels = c(0, 1)),
      is_senior = factor(is_senior, levels = c(0, 1))
    ) %>%
    select(-FamilyType)  # Remove original FamilyType column
}

# Apply the conversion
final_data <- final_data %>%
  convert_family_type()

# convert other columns to binary factors
final_data <- final_data %>%
  mutate(across(c(Disability, Homelessness, DisabilityMod, Indigenous), 
                ~factor(., levels = c(0, 1))))

# Print summary of the new features
cat("\nSummary of new family type features:\n")
summary(final_data)

# Print distributions of binary variables
cat("\nDistributions of family type features:\n")
cat("\nSingle vs Couple (1 = Single):\n")
table(final_data$is_single)
cat("\nHas Children (1 = Yes):\n")
table(final_data$has_children)
cat("\nChildren Count (0 = None, 1 = One, 2 = Two, 3 = Three or More):\n")
table(final_data$children_count)
cat("\nSenior Status (1 = Senior):\n")
table(final_data$is_senior)

# undersampling the majority class
final_data <- final_data %>%
  group_by(Status) %>%
  sample_n(size = min(table(final_data$Status)), replace = FALSE) %>%
  ungroup()

# Print summary of the new dataset
cat("\nSummary of the new dataset after undersampling:\n")
summary(final_data)

# Save the final processed data
write.csv(final_data, "dataset/processed_housing_classification.csv", row.names = FALSE)
