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
data <- read.csv("dataset/processed_housing_classification_balanced.csv", stringsAsFactors = TRUE)

summary(data)

# Convert Status to factor if not already
data$Status <- factor(data$Status, levels = c(0, 1))

# Convert all character columns to factors
data <- data %>%
  mutate(across(where(is.character), as.factor))

summary(data)

# Print initial class distribution
cat("\nInitial class distribution:\n")
print(table(data$Status))
cat(sprintf("Class 0: %.2f%%\n", table(data$Status)[1] / nrow(data) * 100))
cat(sprintf("Class 1: %.2f%%\n", table(data$Status)[2] / nrow(data) * 100))

# Set random seed for reproducibility
set.seed(42)

# Split the data into training, validation, and test sets (60-20-20 split)
train_index <- createDataPartition(data$Status, p = 0.6, list = FALSE)
train_data <- data[train_index, ]
temp_data <- data[-train_index, ]

val_index <- createDataPartition(temp_data$Status, p = 0.5, list = FALSE)
val_data <- temp_data[val_index, ]
test_data <- temp_data[-val_index, ]

# Verify splits
cat("\nClass distributions after splitting:\n")
cat("\nTraining set (60%):\n")
print(table(train_data$Status))
cat("\nValidation set (20%):\n")
print(table(val_data$Status))
cat("\nTest set (20%):\n")
print(table(test_data$Status))

# Create model matrices for glmnet
# First, identify factor columns and create dummy variables
dummy_cols <- model.matrix(~ . - 1 - Status, data = train_data)
feature_names <- colnames(dummy_cols)

# Create matrices for all sets using the same feature names
x_train <- model.matrix(~ . - 1 - Status, data = train_data)[, feature_names]
x_val <- model.matrix(~ . - 1 - Status, data = val_data)[, feature_names]
x_test <- model.matrix(~ . - 1 - Status, data = test_data)[, feature_names]

# Convert target variables
y_train <- as.numeric(train_data$Status) - 1
y_val <- as.numeric(val_data$Status) - 1
y_test <- as.numeric(test_data$Status) - 1

# Verify dimensions
cat("\nMatrix dimensions:")
cat("\nTraining:", dim(x_train), "y:", length(y_train))
cat("\nValidation:", dim(x_val), "y:", length(y_val))
cat("\nTest:", dim(x_test), "y:", length(y_test))

# Define expanded hyperparameter grid
alpha_grid <- c(0, 0.25, 0.5, 0.75, 1)  # Mix of ridge and lasso
lambda_grid <- 10^seq(-6, 3, length.out = 50)  # Regularization strength
maxit_grid <- c(1000, 5000, 10000)  # Maximum number of iterations
tol_grid <- c(1e-7, 1e-6, 1e-5)  # Convergence tolerance
standardize_grid <- c(TRUE, FALSE)  # Whether to standardize features

# Initialize variables for best model
best_auc <- 0
best_alpha <- NULL
best_lambda <- NULL
best_maxit <- NULL
best_tol <- NULL
best_standardize <- NULL
best_model <- NULL

# Perform grid search
cat("\nStarting hyperparameter tuning...\n")
total_combinations <- length(alpha_grid) * length(lambda_grid) * 
                     length(maxit_grid) * length(tol_grid) * 
                     length(standardize_grid)
current_combination <- 0

for(alpha in alpha_grid) {
  for(maxit in maxit_grid) {
    for(tol in tol_grid) {
      for(standardize in standardize_grid) {
        # Fit cross-validated model for current parameters
        cv_fit <- cv.glmnet(
          x = x_train,
          y = y_train,
          family = "binomial",
          alpha = alpha,
          lambda = lambda_grid,
          nfolds = 5,
          type.measure = "auc",
          maxit = maxit,
          thresh = tol,
          standardize = standardize
        )
        
        # Try each lambda value
        for(lambda in cv_fit$lambda) {
          current_combination <- current_combination + 1
          if(current_combination %% 50 == 0) {
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
            best_maxit <- maxit
            best_tol <- tol
            best_standardize <- standardize
            best_model <- glmnet(
              x = x_train,
              y = y_train,
              family = "binomial",
              alpha = alpha,
              lambda = lambda,
              maxit = maxit,
              thresh = tol,
              standardize = standardize
            )
            
            cat(sprintf("\nNew best validation AUC: %.4f with parameters:\n", best_auc))
            cat(sprintf("  alpha: %.2f\n", best_alpha))
            cat(sprintf("  lambda: %.6f\n", best_lambda))
            cat(sprintf("  max iterations: %d\n", best_maxit))
            cat(sprintf("  tolerance: %.1e\n", best_tol))
            cat(sprintf("  standardize: %s\n", best_standardize))
          }
        }
      }
    }
  }
}

# Print final best parameters
cat("\n\nFinal best parameters:\n")
cat(sprintf("alpha: %.2f\n", best_alpha))
cat(sprintf("lambda: %.6f\n", best_lambda))
cat(sprintf("max iterations: %d\n", best_maxit))
cat(sprintf("tolerance: %.1e\n", best_tol))
cat(sprintf("standardize: %s\n", best_standardize))
cat(sprintf("Validation AUC: %.4f\n", best_auc))

# Make predictions on test set
predictions_prob <- as.vector(predict(best_model, newx = x_test, type = "response"))

# Calculate ROC curve and find optimal threshold
roc_obj <- roc(y_test, predictions_prob)
optimal_coords <- coords(roc_obj, "best", ret = c("threshold", "specificity", "sensitivity"))
optimal_threshold <- optimal_coords$threshold

# Make final predictions using optimal threshold
predictions <- factor(ifelse(predictions_prob > optimal_threshold, 1, 0), levels = c(0, 1))
test_labels <- factor(y_test, levels = c(0, 1))

# Calculate confusion matrix
conf_matrix <- confusionMatrix(predictions, test_labels)
cat("\nConfusion Matrix:\n")
print(conf_matrix)

# Plot ROC curve
png("plots/roc_curve.png", width = 800, height = 600)
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc(roc_obj), 3), ")"))
abline(h = seq(0, 1, 0.2), v = seq(0, 1, 0.2), col = "gray", lty = 2)
points(optimal_coords$specificity, optimal_coords$sensitivity, 
       pch = 19, col = "red")
dev.off()

# Save model performance metrics
performance <- data.frame(
  Metric = c(
    "Accuracy",
    "Balanced Accuracy",
    "Sensitivity",
    "Specificity",
    "Precision",
    "F1 Score",
    "AUC",
    "Optimal Threshold"
  ),
  Value = c(
    conf_matrix$overall["Accuracy"],
    conf_matrix$byClass["Balanced Accuracy"],
    conf_matrix$byClass["Sensitivity"],
    conf_matrix$byClass["Specificity"],
    conf_matrix$byClass["Pos Pred Value"],
    conf_matrix$byClass["F1"],
    auc(roc_obj),
    optimal_threshold
  )
)

# Save results
write.csv(performance, "results/model_performance.csv", row.names = FALSE)
saveRDS(best_model, "models/logistic_regression_model.rds")

# Print final performance summary
cat("\nFinal Model Performance:\n")
print(performance) 