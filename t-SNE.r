# Install and load necessary packages
install.packages("caret")
install.packages("e1071")  # Required for SVM
install.packages("Rtsne")  # Required for t-SNE
library(caret)
library(e1071)  # Required for SVM
library(Rtsne)  # Required for t-SNE

# Load the mtcars dataset
data(mtcars)

# Check the structure of the dataset
str(mtcars)

#print number of instances and number of instances
cat("Number of attributes:", ncol(mtcars), "\n")
cat("Number of instances:", nrow(mtcars), "\n")

# Perform t-SNE
tsne_result <- Rtsne(mtcars, dims = 2, perplexity = 5, verbose = TRUE)

# Create a data frame with matching rows
tsne_df <- data.frame(Y1 = tsne_result$Y[,1], Y2 = tsne_result$Y[,2], mpg = mtcars$mpg)

# Split the t-SNE result into training and testing sets
set.seed(123) # for reproducibility
train_indices <- createDataPartition(tsne_df$mpg, p = 0.7, list = FALSE)
train_data <- tsne_df[train_indices, ]
test_data <- tsne_df[-train_indices, ]

# Define the hyperparameter grid for SVM
svm_grid <- expand.grid(sigma = c(0.1, 1, 10),  # For radial basis kernel
                        C = c(0.1, 1, 10))

# Train the SVM model with hyperparameter tuning
svm_model <- train(
  mpg ~ ., 
  data = train_data, 
  method = "svmRadial",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = svm_grid
)

# Make predictions on the test set
predicted_values <- predict(svm_model, newdata = test_data)

# Calculate RMSE
rmse <- sqrt(mean((predicted_values - test_data$mpg)^2))
cat("RMSE:", rmse, "\n")


# Scatter plot of t-SNE results
scatter_plot <- ggplot(tsne_df, aes(x = Y1, y = Y2)) +
  geom_point() +
  labs(title = "t-SNE Scatter Plot",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2") +
  theme_minimal()

print(scatter_plot)

# Density plot of t-SNE results
density_plot <- ggplot(tsne_df, aes(x = Y1, y = Y2)) +
  geom_density_2d() +
  labs(title = "t-SNE Density Plot",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2") +
  theme_minimal()

print(density_plot)
