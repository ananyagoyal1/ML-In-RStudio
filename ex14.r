# Load packages
library(mlbench)
library(nnet)
library(caret)

# Load the sonar dataset
data(Sonar)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(Sonar$Class, p = 0.7, list = FALSE)
trainData <- Sonar[trainIndex, ]
testData <- Sonar[-trainIndex, ]

# Preprocess the data
x_train <- model.matrix(Class ~ ., data = trainData)[, -1]
y_train <- trainData$Class
x_test <- model.matrix(Class ~ ., data = testData)[, -1]
y_test <- testData$Class

# Train the MLP model
mlp_model <- train(x_train, y_train,
                   method = "nnet",
                   trace = FALSE,
                   linout = FALSE,
                   tuneGrid = expand.grid(size = c(5, 10, 15),
                                          decay = c(0.01, 0.1, 0.2)),
                   metric = "Accuracy")

# Make predictions on the test data
predictions <- predict(mlp_model, newdata = x_test)

# Calculate accuracy
accuracy <- sum(predictions == y_test) / length(y_test)
cat("Accuracy:", accuracy, "\n")

# Plot the model performance
plot(mlp_model)


