# Load necessary libraries
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(pROC)

# Load the dataset
file_path <- "/Users/skmazhit/Desktop/ind project /Sanim Mazhit - 33176A/heart.csv"
data <- read.csv(file_path)

# Ensure categorical columns are factors
data <- data %>% mutate(across(where(is.character), as.factor))

# Check if the output column exists and has at least two classes
if (!"output" %in% colnames(data)) {
  stop("The 'output' column is missing from the dataset.")
}

# Check unique values in the output column
print("Unique values in 'output':")
print(unique(data$output))

if (length(unique(data$output)) < 2) {
  stop("The 'output' column must have at least two unique values.")
}

# Handle missing or incomplete data in the output column
data <- data %>% filter(!is.na(output))

# Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(data$output, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# 1. Logistic Regression

# Fit the logistic regression model
logistic_model <- glm(output ~ ., data = train_data, family = "binomial")

# Summary of the logistic regression model
summary(logistic_model)

# Make predictions on the test set
logistic_predictions <- predict(logistic_model, newdata = test_data, type = "response")
logistic_pred_class <- ifelse(logistic_predictions > 0.5, 1, 0)

# Evaluate logistic regression performance
logistic_conf_matrix <- confusionMatrix(factor(logistic_pred_class), factor(test_data$output))
print("Confusion Matrix for Logistic Regression:")
print(logistic_conf_matrix)

# Calculate AUC-ROC for Logistic Regression
logistic_roc <- roc(test_data$output, logistic_predictions)
auc_logistic <- auc(logistic_roc)
print(paste("AUC for Logistic Regression:", auc_logistic))

# Reset graphical device and set margins for plotting
par(mar = c(5, 5, 4, 2))
plot(logistic_roc, main = "ROC Curve for Logistic Regression", col = "blue")

# 2. Random Forest

# Ensure the output column is a factor
train_data$output <- as.factor(train_data$output)
test_data$output <- as.factor(test_data$output)

# Fit the random forest model for classification
set.seed(123)
random_forest_model <- randomForest(output ~ ., data = train_data, ntree = 500, mtry = sqrt(ncol(train_data) - 1), importance = TRUE)

# Print the random forest model
print(random_forest_model)

# Feature importance
importance(random_forest_model)
varImpPlot(random_forest_model, main = "Feature Importance")

# Make predictions on the test set
rf_predictions <- predict(random_forest_model, newdata = test_data)

# Evaluate random forest performance
rf_conf_matrix <- confusionMatrix(factor(rf_predictions), factor(test_data$output))
print("Confusion Matrix for Random Forest:")
print(rf_conf_matrix)

# Calculate AUC-ROC for Random Forest
rf_roc <- roc(as.numeric(test_data$output), as.numeric(rf_predictions))
auc_rf <- auc(rf_roc)
print(paste("AUC for Random Forest:", auc_rf))

# Reset graphical device and set margins for plotting
par(mar = c(5, 5, 4, 2))
plot(rf_roc, main = "ROC Curve for Random Forest", col = "green")


# Compare Results
print(paste("Logistic Regression AUC:", auc_logistic))
print(paste("Random Forest AUC:", auc_rf))