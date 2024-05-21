# Load packages
library(ggplot2)
library(caret)

# Load the dataset
heart_data <- read.csv("C:/Users/Vijeta/Desktop/heart.csv")

#print number of instances and number of instances
cat("Number of attributes:", ncol(heart_data), "\n")
cat("Number of instances:", nrow(heart_data), "\n") 

# Split the data into training and testing sets
set.seed(123) # for reproducibility
train_indices <- sample(1:nrow(heart_data), 0.7 * nrow(heart_data))
train_data <- heart_data[train_indices, ]
test_data <- heart_data[-train_indices, ]

# Define the hyperparameter grid
hyper_grid <- expand.grid(
  alpha = seq(0, 1, by = 0.1),  # Regularization parameter
  lambda = seq(0.001, 0.1, by = 0.001)  # Shrinkage parameter
)

# Train the logistic regression model with hyperparameter tuning
ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation
logistic_model <- train(
  target ~ ., 
  data = train_data, 
  method = "glmnet", 
  trControl = ctrl,
  tuneGrid = hyper_grid,
  family = binomial
)

# Make predictions on the test set
predicted_probabilities <- predict(logistic_model, newdata = test_data)

# Calculate accuracy
predictions <- ifelse(predicted_probabilities > 0.5, 1, 0)
accuracy <- sum(predictions == test_data$target) / length(predictions)
cat("Accuracy:", accuracy, "\n")

# Combine predicted probabilities with test_data
test_data$predicted_probabilities <- predicted_probabilities

# Visualize the logistic regression decision boundary
logistic_plot <- ggplot(test_data, aes(x = age, y = chol, color = factor(target))) +
  geom_point(alpha = 0.7) +
  geom_smooth(aes(y = predicted_probabilities), method = "loess", se = FALSE, linetype = "solid", size = 1) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Logistic Regression Decision Boundary",
       x = "Age",
       y = "Cholesterol",
       color = "Target") +
  theme_minimal()

print(logistic_plot)
