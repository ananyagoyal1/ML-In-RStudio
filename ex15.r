# Load caret library
library(caret)

# Load a suitable dataset
data("Ionosphere", package = "mlbench")

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(Ionosphere$Class, p = .8, 
                                  list = FALSE, 
                                  times = 1)
train_data <- Ionosphere[trainIndex, ]
test_data <- Ionosphere[-trainIndex, ]

# Train the model
trained_model <- train(Class ~ ., 
                       data = train_data, 
                       method = "lda", 
                       trControl = trainControl(method = "cv", number = 5))

# Print the tuned parameters
print(trained_model)

# Make predictions on test data
predictions <- predict(trained_model, newdata = test_data)

# Evaluate the model
confusionMatrix(predictions, test_data$Class)

# Plot the results (e.g., performance vs. hyperparameters)
plot(trained_model)


