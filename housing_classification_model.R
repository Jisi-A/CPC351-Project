# Import required libraries
library(dplyr)
library(caret)
library(pROC)
library(ggplot2)
library(glmnet)  # For regularized logistic regression

# Create directories if they don't exist
dir.create("plots", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)
dir.create("models", showWarnings = FALSE)

# Read the processed data
data <- read.csv("dataset/processed_housing_classification_balanced.csv")

# drop columns that are not needed
data <- data %>%
  select(-c(is_single, has_children, is_senior, children_count))

# Convert all character columns to factors
data <- data %>%
  mutate(across(where(is.character), as.factor))

# Handle missing values
data <- data %>%
  # For numeric columns: replace NA with median
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  # For factor columns: replace NA with mode
  mutate(across(where(is.factor), ~{
    if(any(is.na(.))) {
      mode_val <- names(sort(table(.), decreasing = TRUE))[1]
      ifelse(is.na(.), mode_val, .)
    } else {
      .
    }
  }))

# Convert Status and binary columns to factors with valid R variable names
# "Public_CH","ATSIH","RIC","Transitional","Affordable","Housing_Types_Count"
data <- data %>%
  mutate(
    Status = factor(Status, levels = c(0, 1), labels = c("Class0", "Class1")),
    across(c( Disability, Homelessness, DisabilityMod, Indigenous, Public_CH, ATSIH, RIC, Transitional, Affordable), 
           ~factor(., levels = c(0, 1), labels = c("No", "Yes")))
  )

# TESTOUT: drop  # "Public_CH","ATSIH","RIC","Transitional","Affordable","Housing_Types_Count"
data <- data %>%
  select(-c(Public_CH, ATSIH, RIC, Transitional, Affordable, Housing_Types_Count))


# Print initial class distribution
cat("\nInitial class distribution:\n")
print(table(data$Status))
cat(sprintf("Class 0: %.2f%%\n", table(data$Status)[1] / nrow(data) * 100))
cat(sprintf("Class 1: %.2f%%\n", table(data$Status)[2] / nrow(data) * 100))

# Set random seed for reproducibility
set.seed(42)  # Using 42 as a standard random seed

# Split the data into training, validation, and test sets (60-20-20 split)
# First split: 60% for training, maintaining class proportions
train_index <- createDataPartition(data$Status, 
                                 p = 0.6, 
                                 list = FALSE, 
                                 times = 1)
train_data <- data[train_index, ]
temp_data <- data[-train_index, ]

# Second split: Split remaining data into validation (50%) and test (50%)
# This maintains the original class proportions in both sets
val_index <- createDataPartition(temp_data$Status, 
                                p = 0.5, 
                                list = FALSE, 
                                times = 1)
val_data <- temp_data[val_index, ]
test_data <- temp_data[-val_index, ]

# Verify class distributions in all splits
cat("\nClass distributions after stratified splitting:\n")

cat("\nTraining set (60%):\n")
train_dist <- table(train_data$Status)
print(train_dist)
cat(sprintf("Class 0: %.2f%%\n", train_dist[1] / nrow(train_data) * 100))
cat(sprintf("Class 1: %.2f%%\n", train_dist[2] / nrow(train_data) * 100))

cat("\nValidation set (20%):\n")
val_dist <- table(val_data$Status)
print(val_dist)
cat(sprintf("Class 0: %.2f%%\n", val_dist[1] / nrow(val_data) * 100))
cat(sprintf("Class 1: %.2f%%\n", val_dist[2] / nrow(val_data) * 100))

cat("\nTest set (20%):\n")
test_dist <- table(test_data$Status)
print(test_dist)
cat(sprintf("Class 0: %.2f%%\n", test_dist[1] / nrow(test_data) * 100))
cat(sprintf("Class 1: %.2f%%\n", test_dist[2] / nrow(test_data) * 100))

# Create model matrices for glmnet
x_train <- model.matrix(Status ~ ., train_data)[,-1]  # Remove intercept
y_train <- ifelse(train_data$Status == "Class1", 1, 0)

x_val <- model.matrix(Status ~ ., val_data)[,-1]  # Remove intercept
y_val <- ifelse(val_data$Status == "Class1", 1, 0)

x_test <- model.matrix(Status ~ ., test_data)[,-1]  # Remove intercept
y_test <- ifelse(test_data$Status == "Class1", 1, 0)

# Calculate observation weights for training data
# obs_weights <- ifelse(y_train == 1, 
#                      class_weights["Class1"], 
#                      class_weights["Class0"])

# Define expanded hyperparameter grid
# Class weights ratios (equivalent to the provided weight dictionary)
# weight_ratios <- list(
#   c(1000, 100), c(1000, 10), c(1000, 1),
#   c(500, 1), c(400, 1), c(300, 1), c(200, 1),
#   c(150, 1), c(100, 1), c(99, 1), c(10, 1),
#   c(0.01, 1), c(0.01, 10), c(0.01, 100),
#   c(0.001, 1), c(0.005, 1), c(1, 1),
#   c(1, 0.1), c(10, 0.1), c(100, 0.1),
#   c(10, 0.01), c(1, 0.01), c(1, 0.001), c(1, 0.005),
#   c(1, 10), c(1, 99), c(1, 100), c(1, 150),
#   c(1, 200), c(1, 300), c(1, 400), c(1, 500),
#   c(1, 1000), c(10, 1000), c(100, 1000)
# )

# Alpha values (0 = ridge/L2, 1 = lasso/L1)
alpha_grid <- c(0, 1)  # Equivalent to L1 and L2 penalties

# Lambda values (equivalent to inverse of C in sklearn)
lambda_grid <- 1 / seq(0.5, 20, 0.5)

# Initialize variables for best model
best_auc <- 0
best_alpha <- NULL
best_lambda <- NULL
best_weights <- NULL
best_model <- NULL

# Perform grid search
cat("\nStarting hyperparameter tuning...\n")
total_combinations <- length(weight_ratios) * length(alpha_grid) * length(lambda_grid)
current_combination <- 0

# for(weight_ratio in weight_ratios) {
  # # Calculate class weights
  # class_weights <- c(weight_ratio[1], weight_ratio[2])
  # class_weights <- class_weights / sum(class_weights)  # Normalize weights
  # names(class_weights) <- c("Class0", "Class1")
  
  # # Calculate observation weights
  # obs_weights <- ifelse(y_train == 1, 
  #                      class_weights["Class1"], 
  #                      class_weights["Class0"])
  
  for(alpha in alpha_grid) {
    # Fit cross-validated model for current alpha
    cv_fit <- cv.glmnet(
      x = x_train,
      y = y_train,
      family = "binomial",
      alpha = alpha,
      lambda = lambda_grid,
      # weights = obs_weights,
      nfolds = 5,
      type.measure = "auc"
    )
    
    # Try each lambda value
    for(lambda in cv_fit$lambda) {
      current_combination <- current_combination + 1
      if(current_combination %% 100 == 0) {
        cat(sprintf("\nTried %d/%d combinations...", current_combination, total_combinations))
      }
      
      # Get predictions on validation set
      val_pred <- predict(cv_fit, newx = x_val, s = lambda, type = "response")
      current_auc <- auc(roc(y_val, val_pred))
      
      # Update best parameters if current model is better
      if(current_auc > best_auc) {
        best_auc <- current_auc
        best_alpha <- alpha
        best_lambda <- lambda
        # best_weights <- class_weights
        best_model <- glmnet(
          x = x_train,
          y = y_train,
          family = "binomial",
          alpha = alpha,
          lambda = lambda,
          # weights = obs_weights
        )
        
        cat(sprintf("\nNew best validation AUC: %.4f with parameters:\n", best_auc))
        cat(sprintf("  alpha: %.2f (%s)\n", best_alpha, ifelse(best_alpha == 0, "ridge/L2", "lasso/L1")))
        cat(sprintf("  lambda: %.6f\n", best_lambda))
        # cat(sprintf("  class weights - Class0: %.4f, Class1: %.4f\n", 
                  #  best_weights["Class0"], best_weights["Class1"]))
      }
    }
  }
# }

# Print final best parameters
cat("\n\nFinal best parameters:\n")
cat(sprintf("alpha: %.2f (%s)\n", best_alpha, ifelse(best_alpha == 0, "ridge/L2", "lasso/L1")))
cat(sprintf("lambda: %.6f\n", best_lambda))
# cat(sprintf("class weights - Class0: %.4f, Class1: %.4f\n", 
           # best_weights["Class0"], best_weights["Class1"]))
cat(sprintf("Validation AUC: %.4f\n", best_auc))

# Make predictions on test set
predictions_prob <- predict(best_model, newx = x_test, type = "response")[,1]

# Find optimal threshold using ROC curve
roc_obj <- roc(y_test, predictions_prob)
optimal_threshold <- coords(roc_obj, "best", ret = "threshold")$threshold
predictions <- factor(ifelse(predictions_prob > optimal_threshold, "Class1", "Class0"), 
                     levels = c("Class0", "Class1"))
test_data$Status <- factor(ifelse(y_test == 1, "Class1", "Class0"), 
                          levels = c("Class0", "Class1"))

# Calculate confusion matrix
conf_matrix <- confusionMatrix(predictions, test_data$Status)
cat("\nConfusion Matrix:\n")
print(conf_matrix)

# Plot ROC curve
png("plots/roc_curve.png", width = 800, height = 600)
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc(roc_obj), 3), ")"))
abline(h = seq(0, 1, 0.2), v = seq(0, 1, 0.2), col = "gray", lty = 2)
points(coords(roc_obj, "best")$specificity, coords(roc_obj, "best")$sensitivity, 
       pch = 19, col = "red")
dev.off()

# Feature importance based on absolute coefficient values
feature_importance <- abs(as.vector(coef(best_model)[-1]))  # Remove intercept
names(feature_importance) <- colnames(x_train)
feature_importance <- sort(feature_importance, decreasing = TRUE)

# Plot feature importance
importance_df <- data.frame(
  Feature = names(feature_importance),
  Importance = feature_importance
)

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Feature Importance in Logistic Regression",
    subtitle = paste("Alpha =", round(best_alpha, 2), ", Lambda =", format(best_lambda, scientific = TRUE)),
    x = "Features",
    y = "Absolute Coefficient Value"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
ggsave("plots/feature_importance.png", width = 10, height = 8)

# Save model performance metrics
performance <- data.frame(
  Metric = c(
    "Accuracy", 
    "Balanced Accuracy",
    "Sensitivity (True Positive Rate)", 
    "Specificity (True Negative Rate)",
    "Precision (Positive Predictive Value)",
    "Negative Predictive Value",
    "F1 Score",
    "AUC",
    "Optimal Threshold",
    "Best Alpha",
    "Best Lambda",
    "Validation AUC"
  ),
  Value = c(
    conf_matrix$overall["Accuracy"],
    conf_matrix$byClass["Balanced Accuracy"],
    conf_matrix$byClass["Sensitivity"],
    conf_matrix$byClass["Specificity"],
    conf_matrix$byClass["Pos Pred Value"],
    conf_matrix$byClass["Neg Pred Value"],
    conf_matrix$byClass["F1"],
    auc(roc_obj),
    optimal_threshold,
    best_alpha,
    best_lambda,
    best_auc
  )
)

# Add description column
performance$Description <- c(
  "Overall prediction accuracy",
  "Average of sensitivity and specificity",
  "Proportion of actual positive cases correctly identified",
  "Proportion of actual negative cases correctly identified",
  "Proportion of predicted positive cases that are actually positive",
  "Proportion of predicted negative cases that are actually negative",
  "Harmonic mean of precision and sensitivity",
  "Area Under the ROC Curve (model discrimination ability)",
  "Optimal probability threshold for classification",
  "Best mixing parameter between L1 and L2 regularization",
  "Best regularization strength parameter",
  "Best validation set AUC score"
)

# Format numeric values
performance$Value <- round(performance$Value, 4)

# Save performance metrics
write.csv(performance, "results/model_performance.csv", row.names = FALSE)

# Save the model
saveRDS(best_model, "models/weighted_logistic_regression_model.rds") 