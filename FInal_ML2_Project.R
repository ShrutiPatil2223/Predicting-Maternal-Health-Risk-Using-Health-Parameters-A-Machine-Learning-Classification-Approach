# Load necessary libraries
library(tidyverse)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(ROCR)

# Read the dataset
df <- read.csv('C:/Users/shrut/Downloads/Maternal Health Risk Data .csv')
head(df)

# Check for missing values
colSums(is.na(df))

# Map risk levels to binary values
risk_mapping <- c('low risk' = 0, 'high risk' = 1)
df$RiskLevel <- as.numeric(risk_mapping[df$RiskLevel])


barplot(table(df$RiskLevel), main = "Distribution of Risk Levels", xlab = "Risk Level", ylab = "Count") ##add the counts


# Split data into train and test sets
set.seed(123)
trainIndex <- createDataPartition(df$RiskLevel, p = 0.8, list = FALSE) ## do 80:20 recheck
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

# Train a decision tree model

model <- rpart(RiskLevel ~ ., data = trainData, method = 'class')

# Plot the tree
rpart.plot(model)

# Make predictions
predictions <- predict(model, testData, type = 'class')

# Evaluate the model
conf_matrix <- confusionMatrix(as.factor(predictions), as.factor(testData$RiskLevel))

#Update row and column names to reflect risk levels
rownames(conf_matrix$table) <- c('Low Risk', 'High Risk')
colnames(conf_matrix$table) <- c('Low Risk', 'High Risk')
conf_matrix$table <- t(conf_matrix$table)  # Transpose to match predicted as columns and actual as rows
colnames(conf_matrix$table) <- c('Low Risk', 'High Risk')  # Predicted
rownames(conf_matrix$table) <- c('Low Risk', 'High Risk')  # Actual
print(conf_matrix$table)

## checking balanicng
table(trainData$RiskLevel)


## balancing


# Separate the majority and minority classes
majority_class <- trainData[trainData$RiskLevel == 1, ]
minority_class <- trainData[trainData$RiskLevel == 0, ]

# Oversample the minority class
set.seed(42)
minority_oversampled <- minority_class[sample(1:nrow(minority_class), nrow(majority_class), replace = TRUE), ]

# Combine the oversampled minority class with the majority class
balanced_train_data <- rbind(majority_class, minority_oversampled)

# Shuffle the balanced dataset
balanced_train_data <- balanced_train_data[sample(1:nrow(balanced_train_data)), ]

# Separate the features and target variable again
X_train_balanced <- balanced_train_data[, !names(balanced_train_data) %in% c('RiskLevel')]
Y_train_balanced <- balanced_train_data$RiskLevel

# Output the counts of each class in the balanced dataset
cat("Balanced Training Data Class Distribution:\n")
print(table(Y_train_balanced))



########## searching for the best hyperparameter
#------------------------------------------------------------------------------
# Ensure RiskLevel is a factor
balanced_train_data$RiskLevel <- as.factor(balanced_train_data$RiskLevel)
testData$RiskLevel <- as.factor(testData$RiskLevel)

# Define hyperparameter grid for Random Forest
param_grid <- expand.grid(
  mtry = c(2:5)  # Only mtry can be tuned directly
)

ntree_values <- c(100, 200, 300, 400)  # Number of trees to iterate over
nodesize_values <- c(1:4)  # Node size to iterate over

control <- trainControl(method = 'cv', number = 5)

best_model <- NULL
best_params <- list()
best_recall <- -Inf

for (ntree in ntree_values) {
  for (nodesize in nodesize_values) {
    set.seed(42)
    rf_model <- randomForest(
      RiskLevel ~ .,
      data = balanced_train_data,
      mtry = param_grid$mtry[1],
      ntree = ntree,
      nodesize = nodesize
    )
    
    predictions <- predict(rf_model, testData)
    
    # Ensure predictions are factors with the same levels as testData$RiskLevel
    predictions <- factor(predictions, levels = levels(testData$RiskLevel))
    
    #conf_matrix <- confusionMatrix(predictions, testData$RiskLevel)
    
    recall <- conf_matrix$byClass["Recall"]
    
    if (!is.na(recall) && recall > best_recall) {
      best_recall <- recall
      best_model <- rf_model
      best_params <- list(
        mtry = param_grid$mtry[1],
        ntree = ntree,
        nodesize = nodesize
      )
    }
  }
}

# Print the best parameters and recall
cat("Best Parameters:\n")
print(best_params)

cat("Best Cross-Validation Recall:\n")
print(best_recall)

cat("Formatted Best Parameters:\n")
cat("'mtry':", best_params$mtry, ", 'ntree':", best_params$ntree,
    ", 'nodesize':", best_params$nodesize, "\n")

# Evaluate the best model on the untouched test set
final_predictions <- predict(best_model, testData)

# Ensure final_predictions are factors with the same levels as testData$RiskLevel
final_predictions <- factor(final_predictions, levels = levels(testData$RiskLevel))

# Evaluate the best model on the untouched test set
final_predictions <- predict(best_model, testData)

# Confusion Matrix
final_conf_matrix <- confusionMatrix(as.factor(final_predictions), as.factor(testData$RiskLevel))
print("Confusion Matrix:")
print(final_conf_matrix$table)


#Update row and column names to reflect risk levels
rownames(final_conf_matrix$table) <- c('Low Risk', 'High Risk')
colnames(final_conf_matrix$table) <- c('Low Risk', 'High Risk')
final_conf_matrix$table <- t(final_conf_matrix$table)  # Transpose to match predicted as columns and actual as rows
colnames(final_conf_matrix$table) <- c('Low Risk', 'High Risk')  # Predicted
rownames(final_conf_matrix$table) <- c('Low Risk', 'High Risk')  # Actual
print(final_conf_matrix$table)

# Extract the confusion matrix table
conf_table <- final_conf_matrix$table

# Calculate accuracy
accuracy <- sum(diag(conf_table)) / sum(conf_table)

# Print accuracy
cat("Accuracy:", accuracy, "\n")


### Model 2 --

## using Gereneralized Additive Models (GAMs)


# Load the gam library set the cross validation here 
library(gam)

# Fit a logistic GAM model using the gam library
gam_model <- gam(RiskLevel ~ lo(Age) + lo(BS) + lo(BodyTemp) + lo(HeartRate) + lo(SystolicBP) + lo(DiastolicBP),
                 family = binomial, data = balanced_train_data)
  
# Summarize the model
summary(gam_model)

# Predict on the test set
gam_pred_prob <- predict(gam_model, testData, type = "response")

# Adjust the decision threshold
threshold <- 0.30  # Lower threshold to increase recall
gam_predictions <- ifelse(gam_pred_prob > threshold, 1, 0)

  gam_predictions <- factor(gam_predictions, levels = levels(testData$RiskLevel))

# Confusion Matrix
gam_conf_matrix <- confusionMatrix(as.factor(gam_predictions), as.factor(testData$RiskLevel))

# Print Confusion Matrix
print("GAM Confusion Matrix:")
print(gam_conf_matrix$table)

# Update row and column names to reflect risk levels
rownames(gam_conf_matrix$table) <- c('Low Risk', 'High Risk')
colnames(gam_conf_matrix$table) <- c('Low Risk', 'High Risk')
gam_conf_matrix$table <- t(gam_conf_matrix$table)  # Transpose for consistency
colnames(gam_conf_matrix$table) <- c('Low Risk', 'High Risk')  # Predicted
rownames(gam_conf_matrix$table) <- c('Low Risk', 'High Risk')  # Actual

print("Updated GAM Confusion Matrix:")
print(gam_conf_matrix$table)

# Calculate accuracy
gam_conf_table <- gam_conf_matrix$table
gam_accuracy <- sum(diag(gam_conf_table)) / sum(gam_conf_table)  # Accuracy formula: (TP + TN) / Total

# Print accuracy
cat("GAM Model - Accuracy with Adjusted Threshold:", gam_accuracy, "\n")


# Generate and Plot the ROC Curve
roc_curve <- roc(testData$RiskLevel, gam_pred_prob)

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for GAM Model")
abline(a = 0, b = 1, lty = 2, col = "red")  # Diagonal line for random guessing

# Calculate and display AUC
auc_value <- auc(roc_curve)
cat("AUC for GAM Model:", auc_value, "\n")






