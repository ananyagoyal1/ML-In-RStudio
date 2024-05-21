# Load libraries
library(dplyr)

# Load data
data(CO2)

# Visualize dataset
plot(CO2$conc, CO2$uptake, col = CO2$Treatment, main = "CO2 Dataset")


# Normalize data
CO2_normalized <- as.matrix(scale(CO2[, c("conc", "uptake")]))

# Hyperparameter tuning
wss <- (1:10)
k_range <- seq(2, 10, by = 2)

for (k in k_range) {
  km_fit <- kmeans(CO2_normalized, centers = k, nstart = 20)
  wss[k] <- km_fit$withinss
}

plot(wss, type = "b", pch = 19,
     xlab = "Number of Clusters", ylab = "Within groups sum of squares",
     main = "The Elbow Method")


# Fine-tuning and hyperparameter tuning
# Choose k = 3 for elbow point
k <- 3
set.seed(123)
km_fit <- kmeans(CO2_normalized, centers = k, nstart = 20)

# Visualize clusters
plot(CO2$conc, CO2$uptake, col = km_fit$cluster, main = "CO2 Dataset Clusters", pch = 20)



# View cluster assignments
km_fit$cluster
