# Load caret library
library(caret)
# Load ggplot2 library
library(ggplot2)

# Load the mtcars dataset
data(mtcars)

# Define the target variable and predictors
target <- "mpg"
# Use all predictors except target
predictors <- setdiff(names(mtcars), target)  

# Cross-validation setup
train_control <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE,
  savePredictions = "final",
  summaryFunction = defaultSummary,
# Use 'best' for RFE  
  selectionFunction = "best"  
)

# Recursive feature elimination (RFE) with cross-validation
rfe_control <- rfeControl(
# Linear regression  
  functions = lmFuncs, 
# Cross-validation
  method = "cv",
# Number of folds
  number = 10  
)
# Run RFE to find the best predictors
rfe_results <- rfe(
# Predictor variables  
  mtcars[, predictors],
# Target variable  
  mtcars[, target],  
# Testing all subsets of features
  sizes = 1:length(predictors),  
  rfeControl = rfe_control
)

# Plot the results of RFE to see feature importance
plot(rfe_results, main = "RFE Results for Feature Selection in Linear Regression")


# Best subset of predictors
best_features <- predictors[rfe_results$optVariables]
# Get the optimal subset of features
# Optimal variables
best_features <- rfe_results$optVariables  

# Train the linear regression model with the best subset
final_model <- lm(as.formula(paste(target, "~", paste(best_features, collapse = " + "))), data = mtcars)
# Display the final model summary
summary(final_model)
# Predictions with the final model
# Define new data with all required features
new_cars_data <- data.frame(  
  wt = c(2.5, 3, 3.5),
  hp = c(110, 140, 170),
  cyl = c(4, 6, 8),
  gear = c(4, 4, 5),
# Add 'am' feature (1 for automatic, 0 for manual)
  am = c(1, 0, 1)  
)

# Get predictions
predictions <- predict(final_model, newdata = new_cars_data)

# Display predictions
print(predictions)
