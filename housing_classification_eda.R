# Import required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(corrplot)
library(randomForest)
library(smotefamily)  # For SMOTE

# Read the data
housing_data <- read.csv("dataset/05_Social_Housing.csv")

# ============= Initial Data Exploration =============
cat("\nDataset Structure:\n")
str(housing_data)

# print the first 10 rows of the dataset
print(head(housing_data, 10))

summary(housing_data)

# ============= Data Preprocessing Functions =============
# Function to convert flag columns to binary factors
convert_flag_to_factor <- function(x) {
  factor(case_when(
    x == "Yes" | x == "Y" ~ "Yes",
    x == "No" | x == "N" ~ "No",
    x == "U" | x == "n.a." | x == "" ~ NA_character_,
    TRUE ~ NA_character_
  ), levels = c("No", "Yes"))
}

# Function to calculate mode for NA imputation
calculate_mode <- function(x) {
  if (is.factor(x)) {
    # Get the most frequent non-NA value
    freq_table <- table(x, useNA = "no")
    if (length(freq_table) == 0) return(levels(x)[1])  # Default to first level if all NA
    return(names(freq_table)[which.max(freq_table)])
  } else if (is.numeric(x)) {
    ux <- unique(x[!is.na(x)])
    if (length(ux) == 0) return(0)  # Default to 0 if all NA
    ux[which.max(tabulate(match(x, ux)))]
  } else {
    x <- as.character(x)
    ux <- unique(x[!is.na(x)])
    if (length(ux) == 0) return(NA_character_)
    ux[which.max(tabulate(match(x, ux)))]
  }
}

# ============= Initial Data Preprocessing =============
summary(housing_data)

# Drop row_id column as it is not useful for the model
preprocessed_data <- housing_data %>%
  select(-c(row_id))

# Drop columns that we are certain are not useful for the model
preprocessed_data <- preprocessed_data %>%
  select(-c(ApplicationReceivedMonth, LocalGovtAuthority, StateElectorate, HousingServiceCentre))

# First stage of preprocessing
preprocessed_data <- preprocessed_data %>%
  mutate(
    # Convert target variable to binary
    Status = ifelse(ApplicationListStatusDesc == "ACTIVE", 1, 0),
    Status = factor(Status, levels = c(0, 1)),
    
    # Convert flag columns directly to binary factors
    DisabilityMod = convert_flag_to_factor(DisabilityModRequestFlag),
    Homelessness = convert_flag_to_factor(AtRiskOfOrExperiencingHomelessnessFlag),
    Disability = convert_flag_to_factor(DisabilityApplicationFlag),
    Indigenous = convert_flag_to_factor(IndigenousApplicationFlag),
    
    # Convert numeric columns
    BedroomsRequired = as.numeric(BedroomsRequired),
    MonthsOnRegister = as.numeric(MonthsOnHousingRegister),
    PeopleCount = as.numeric(PeopleonApplication),

    # Convert character columns to factors
    ApplicationListStatusDesc = factor(ApplicationListStatusDesc),
    ApplicationType = factor(ApplicationType),
    FamilyType = factor(FamilyType),
    LanguagePreference = factor(LanguagePreference),
    LettingArea1Desc = factor(LettingArea1Desc),
    LettingArea2Desc = factor(LettingArea2Desc),
    LettingArea3Desc = factor(LettingArea3Desc),
    LettingArea4Desc = factor(LettingArea4Desc),
    LettingArea5Desc = factor(LettingArea5Desc),
    LettingArea6Desc = factor(LettingArea6Desc),
    ApplicationListCategory = factor(ApplicationListCategory),
    RehousingListDesc = factor(RehousingListDesc),
    LanguagePreference = factor(LanguagePreference)
  )

# Drop columns that we have already renamed after converting to factors or numeric previously
preprocessed_data <- preprocessed_data %>%
  select(-c( 
            DisabilityModRequestFlag, AtRiskOfOrExperiencingHomelessnessFlag, 
            DisabilityApplicationFlag, IndigenousApplicationFlag, MonthsOnHousingRegister, PeopleonApplication, ApplicationListStatusDesc
            ))

summary(preprocessed_data)

# show number of unique values for each column
print(sapply(preprocessed_data, function(x) length(unique(x))))

# Drop applicaiton list category because the values are not useful for the model
preprocessed_data <- preprocessed_data %>%
  select(-c(ApplicationListCategory))

# Drop RehousingListDesc because there is only 1 unique value
preprocessed_data <- preprocessed_data %>%
  select(-c(RehousingListDesc))


# drop columns with high cardinality
preprocessed_data <- preprocessed_data %>%
  select(-c(LettingArea1Desc, LettingArea2Desc, LettingArea3Desc, LettingArea4Desc, LettingArea5Desc, LettingArea6Desc, ApplicationType, FamilyType))


# remove LanguagePreference because it has too many n.a. values (24452)
preprocessed_data <- preprocessed_data %>%
  select(-c(LanguagePreference))

summary(preprocessed_data)

# ============= Handle Missing Values =============
# Check for missing values
cat("\nMissing values before imputation:\n")
print(colSums(is.na(preprocessed_data)))

# Handle missing values while preserving data types
preprocessed_data <- preprocessed_data %>%
  mutate(
    # Handle flag columns first
    DisabilityMod = {
      mode_val <- calculate_mode(DisabilityMod)
      cat("\nMode value for DisabilityMod:", mode_val, "\n")
      factor(ifelse(is.na(DisabilityMod), mode_val, as.character(DisabilityMod)), 
             levels = c("No", "Yes"))
    },
    
    Homelessness = {
      mode_val <- calculate_mode(Homelessness)
      cat("Mode value for Homelessness:", mode_val, "\n")
      factor(ifelse(is.na(Homelessness), mode_val, as.character(Homelessness)), 
             levels = c("No", "Yes"))
    },
    
    Disability = {
      mode_val <- calculate_mode(Disability)
      cat("Mode value for Disability:", mode_val, "\n")
      factor(ifelse(is.na(Disability), mode_val, as.character(Disability)), 
             levels = c("No", "Yes"))
    },
    
    Indigenous = {
      mode_val <- calculate_mode(Indigenous)
      cat("Mode value for Indigenous:", mode_val, "\n")
      factor(ifelse(is.na(Indigenous), mode_val, as.character(Indigenous)), 
             levels = c("No", "Yes"))
    },
    
    # Handle numeric columns
    across(c(BedroomsRequired, MonthsOnRegister, PeopleCount), ~{
      if(any(is.na(.))) {
        med_val <- median(., na.rm = TRUE)
        ifelse(is.na(.), med_val, .)
      } else .
    })
  )

# Verify missing values are handled
cat("\nMissing values after imputation:\n")
print(colSums(is.na(preprocessed_data)))

summary(preprocessed_data)

# ============= Feature Selection Analysis =============
# Correlation analysis for numeric variables
numeric_features <- names(preprocessed_data)[sapply(preprocessed_data, is.numeric)]
correlation_matrix <- cor(preprocessed_data[numeric_features], use = "pairwise.complete.obs")

# Plot correlation matrix
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.7, tl.offset = 0.5)

# Drop highly correlated features
preprocessed_data <- preprocessed_data %>%
  select(-PeopleCount)  # PeopleCount is highly correlated with BedroomsRequired

# Random Forest for feature importance
set.seed(123)

# Prepare data for random forest
rf_data <- preprocessed_data %>% select(-Status)  # Features only

# Train Random Forest model
rf_model <- randomForest(x = rf_data, 
                        y = preprocessed_data$Status,
                        importance = TRUE)

# Print feature importance
cat("\nFeature Importance (by Gini decrease):\n")
importance_scores <- importance(rf_model)
print(importance_scores[order(importance_scores[,"MeanDecreaseGini"], decreasing = TRUE), ])

# ============= Create Final Dataset =============
# Select top features based on Random Forest importance
selected_features <- rownames(importance_scores)[order(importance_scores[,"MeanDecreaseGini"], decreasing = TRUE)[1:6]]

cat("\nSelected features:\n")
print(selected_features)

# Create final dataset
final_data <- preprocessed_data %>%
  select(Status, all_of(selected_features))

# Print summary of final dataset
cat("\nFinal dataset summary:\n")
summary(final_data)

# ============= Handle Class Imbalance with SMOTE =============
cat("\nImplementing SMOTE...\n")

# Prepare data for SMOTE
X <- final_data %>% 
  select(-Status) %>%
  mutate(across(everything(), ~{
    if (is.factor(.)) {
      # Convert factors to numeric (0/1) based on levels
      as.numeric(.) - 1  # Converts factor levels to 0-based numeric
    } else {
      # For numeric columns, scale to [0,1] range
      (. - min(.)) / (max(.) - min(.))
    }
  })) %>%
  as.matrix()

# Print the structure of X to verify conversion
cat("\nStructure of features matrix X:\n")
str(X)
cat("\nSummary of features matrix X:\n")
print(summary(as.data.frame(X)))

y <- final_data$Status

# Print class distribution before SMOTE
cat("\nClass distribution before SMOTE:\n")
print(table(y))

# Apply SMOTE with error handling
tryCatch({
  # Calculate appropriate dup_size
  n_majority <- max(table(y))
  n_minority <- min(table(y))
  dup_size <- ceiling(n_majority/n_minority)
  
  smote_result <- SMOTE(X = X,
                        target = y,
                        K = 5,
                        dup_size = dup_size)
  
  # Convert SMOTE result back to data frame
  balanced_data <- as.data.frame(smote_result$data)
  colnames(balanced_data)[ncol(balanced_data)] <- "Status"
  colnames(balanced_data)[1:(ncol(balanced_data)-1)] <- colnames(X)
  
  # Convert back to original data types
  final_data_balanced <- balanced_data %>%
    mutate(
      Status = factor(Status, levels = c(0, 1)),
      across(names(final_data)[names(final_data) != "Status"], ~{
        if (is.factor(final_data[[cur_column()]])) {
          # For columns that were originally factors
          factor(ifelse(. >= 0.5, "Yes", "No"), levels = c("No", "Yes"))
        } else {
          # For columns that were originally numeric
          # Rescale back to original range
          orig_col <- final_data[[cur_column()]]
          orig_min <- min(orig_col)
          orig_max <- max(orig_col)
          round(. * (orig_max - orig_min) + orig_min)
        }
      })
    )
  
  cat("\nClass distribution after SMOTE:\n")
  print(table(final_data_balanced$Status))
  
}, error = function(e) {
  cat("\nError in SMOTE process:", conditionMessage(e))
  cat("\nFalling back to simple oversampling...\n")
  
  # Simple oversampling of minority class
  minority_class <- which.min(table(y))
  majority_class <- which.max(table(y))
  minority_indices <- which(y == levels(y)[minority_class])
  majority_indices <- which(y == levels(y)[majority_class])
  
  sampled_minority <- sample(minority_indices, 
                           size = length(majority_indices), 
                           replace = TRUE)
  
  balanced_indices <- c(majority_indices, sampled_minority)
  final_data_balanced <- final_data[balanced_indices, ]
})

# ============= Save Final Datasets =============
# Save both original and balanced datasets
write.csv(final_data, "dataset/processed_housing_classification.csv", row.names = FALSE)
write.csv(final_data_balanced, "dataset/processed_housing_classification_balanced.csv", row.names = FALSE)

cat("\nDatasets saved successfully:")
cat("\n1. Original dataset:", nrow(final_data), "samples")
cat("\n2. Balanced dataset:", nrow(final_data_balanced), "samples\n")