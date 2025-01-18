# Import required libraries
library(dplyr)
library(caret)
library(pROC)
library(ggplot2)

# Create directories if they don't exist
dir.create("plots", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)
dir.create("models", showWarnings = FALSE)

# Read the processed data
data <- read.csv("dataset/processed_housing_classification.csv")

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
data <- data %>%
  mutate(
    Status = factor(Status, levels = c(0, 1), labels = c("Class0", "Class1")),
    across(c(is_single, has_children, is_senior, 
             Disability, Homelessness, DisabilityMod, Indigenous), 
           ~factor(., levels = c(0, 1), labels = c("No", "Yes")))
  )

# Split the data into training and testing sets (80-20 split)
set.seed(123)
train_index <- createDataPartition(data$Status, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Define cross-validation settings
ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

# Train logistic regression model with cross-validation
model <- train(
  Status ~ .,
  data = train_data,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)

# Print model summary
cat("\nModel Summary:\n")
print(model)
summary(model$finalModel)

# Make predictions on test set
predictions_prob <- predict(model, test_data, type = "prob")[,"Class1"]

# Find optimal threshold using ROC curve
roc_obj <- roc(test_data$Status == "Class1", predictions_prob)
optimal_threshold <- coords(roc_obj, "best", ret = "threshold")$threshold
predictions <- factor(ifelse(predictions_prob > optimal_threshold, "Class1", "Class0"), 
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
feature_importance <- abs(coef(model$finalModel))[-1]  # Remove intercept
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
    subtitle = paste("Based on absolute coefficient values"),
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
    "Optimal Threshold"
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
    optimal_threshold
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
  "Optimal probability threshold for classification"
)

# Format numeric values
performance$Value <- round(performance$Value, 4)

# Save performance metrics with more detailed information
write.csv(performance, "results/model_performance.csv", row.names = FALSE)

# Save cross-validation results
cv_results <- model$results
write.csv(cv_results, "results/cross_validation_results.csv", row.names = FALSE)

# Save the model
saveRDS(model, "models/logistic_regression_model.rds") 