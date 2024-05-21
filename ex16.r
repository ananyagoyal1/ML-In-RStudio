# Load the libraries
library(ggplot2)  # For data visualization
library(kohonen)  # For SOM
library(caret)  # For data preprocessing

# Load the dataset
wine_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep = ";")

# Exploratory data analysis
str(wine_data)  # Check the structure of the dataset
summary(wine_data)  # Get summary statistics # Data visualization
ggplot(wine_data, aes(x = alcohol, y = quality)) +
  geom_point() +
  labs(title = "Relationship between Alcohol and Quality",
       x = "Alcohol Content",
       y = "Quality Score")

# Data preprocessing
wine_data_processed <- wine_data[, -ncol(wine_data)]  # Remove the target variable (quality)
wine_data_processed <- scale(wine_data_processed)  # Normalize the data

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(wine_data$quality, p = 0.8, list = FALSE)
train_data <- wine_data_processed[train_index, ]
test_data <- wine_data_processed[-train_index, ]

# Fit the SOM model
som_model <- som(train_data, grid = somgrid(5, 5, "hexagonal"))  # 5x5 hexagonal grid

# Visualize the SOM
plot(som_model, type = "codes", main = "SOM Codes")  # Plot SOM codes


plot(som_model, type = "dist.neighbours", main = "Distance to Neighbouring Units")  # Plot distances to neighboring units

# Evaluate the model
predictions <- predict(som_model, test_data)
mse <- mean((predictions$unit.classif - wine_data$quality[-train_index])^2)
print(paste("Mean Squared Error:", mse))  # Print the Mean Squared Error



