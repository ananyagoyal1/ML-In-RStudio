# Load necessary packages
install.packages("caret")
install.packages("e1071")
install.packages("ggplot2")
library(caret)
library(e1071)
library(ggplot2)

# Load the Pima Indians Diabetes dataset
PimaIndiansDiabetes <- read.csv("C:/Users/Vijeta/Downloads/diabetes.csv")

# Check the structure of the dataset
str(PimaIndiansDiabetes)

#print number of instances and number of instances
cat("Number of attributes:", ncol(train_data), "\n")
cat("Number of instances:", nrow(train_data), "\n")

# Set seed for reproducibility
set.seed(123)

# Generate random indices for splitting the data
indices <- sample(1:nrow(PimaIndiansDiabetes), 0.7 * nrow(PimaIndiansDiabetes))

# Create training and testing sets
train_data <- PimaIndiansDiabetes[indices, ]
test_data <- PimaIndiansDiabetes[-indices, ]


# Define the hyperparameter grid for SVM
svm_grid <- expand.grid(C = c(0.1, 1, 10),
                        gamma = c(0.1, 1, 10))

str(train_data)

# Define the hyperparameter grid for SVM
svm_grid <- expand.grid(sigma = c(0.1, 1, 10),
                        C = c(0.1, 1, 10))

# Convert Outcome variable to factor with two levels
train_data$Outcome <- factor(train_data$Outcome, levels = c(0, 1))

# Train the SVM model with hyperparameter tuning
svm_model <- train(
  Outcome ~ ., 
  data = train_data, 
  method = "svmRadial",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = svm_grid
)


# Make predictions on the test set
predictions <- predict(svm_model, newdata = test_data)

# Calculate accuracy
accuracy <- sum(predictions == test_data$outcome) / length(predictions)
cat("Accuracy:", accuracy, "\n")


# Create the SVM plot (density plot)
svm_plot_density <- ggplot(train_data, aes(x = Glucose, y = BMI)) +
  geom_density_2d(aes(color = Outcome)) +
  geom_point(data = support_vectors, aes(x = Glucose, y = BMI), color = "black", size = 3, shape = 1) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "SVM Decision Boundary and Support Vectors (Density Plot)",
       x = "Glucose",
       y = "BMI",
       color = "Outcome") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(size = 12), legend.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5), axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# Print the SVM density plot
print(svm_plot_density)



