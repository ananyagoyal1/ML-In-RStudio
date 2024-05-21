# Load tidymodels libraries
library(tidymodels)
# Load MASS libraries
library(MASS)

# Load the dataset
data("Boston")

# Load the dataset and create data splits
set.seed(123)
data_split <- initial_split(Boston, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

# Fit a decision tree model
tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")

# Fit to the training data
final_tree_fit <- tree_spec %>%
  fit(medv ~ ., data = train_data)

# Check the structure to ensure it's a valid rpart object
str(final_tree_fit)
# Define a tuning grid for decision trees
tuning_grid <- expand.grid(
  cost_complexity = seq(0.001, 0.05, length.out = 10)
)

# Create a decision tree specification with tuning
tree_spec <- decision_tree(
# Indicate that we're tuning this parameter
  cost_complexity = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("regression")

# Cross-validation for tuning
# 5-fold cross-validation
cv_folds <- vfold_cv(train_data, v = 5)  

# Tune the model
tuning_results <- tune_grid(
  tree_spec,
# Formula for regression
  medv ~ .,  
  resamples = cv_folds,

  grid = tuning_grid,
  control = control_grid(verbose = TRUE)  
)
# Get the best parameters for RMSE
best_params <- select_best(tuning_results, metric = "rmse")

print(best_params)
# Finalize the tree model with the best parameters
final_tree_spec <- finalize_model(tree_spec, best_params)

# Fit the final model with the best parameters
final_tree_fit <- final_tree_spec %>%
  fit(medv ~ ., data = train_data)

# Make predictions with the correct model
# Ensure 'tree_fit' is the fitted model
predictions <- predict(tree_fit, test_data) %>%
  pull(.pred)

# Calculate RMSE and R-squared for model performance evaluation
metrics <- metric_set(rmse, rsq)
model_performance <- test_data %>%
  mutate(predictions = predictions) %>%
  metrics(truth = medv, estimate = predictions)

# Display performance metrics
print(model_performance)
# Plot the decision tree
#load rpart.plot library
library(rpart.plot)
rpart.plot(final_tree_fit$fit, type = 4, extra = 101, under = TRUE, cex = 0.8, box.palette = "auto")
# Variable importance plot
#load vip library
library(vip)
var_importance <- vip::vip(final_tree_fit, num_features = 10)
print(var_importance)
