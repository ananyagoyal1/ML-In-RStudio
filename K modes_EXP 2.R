# Load required packages
if (!require("klaR")) install.packages("klaR", dependencies = TRUE)
if (!require("cluster")) install.packages("cluster", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)

# Load klar library
library(klaR)
# Load cluster library
library(cluster)
# Load ggplot2 library
library(ggplot2)

# Load a real categorical dataset (Titanic)
data("Titanic")
titanic_df <- as.data.frame(Titanic)

# Data preprocessing: Create a categorical dataset for clustering
titanic_cluster_df <- data.frame(
  Class = factor(rep(titanic_df$Class, titanic_df$Freq)),
  Sex = factor(rep(titanic_df$Sex, titanic_df$Freq)),
  Age = factor(rep(titanic_df$Age, titanic_df$Freq)),
  Survived = factor(rep(titanic_df$Survived, titanic_df$Freq))
)

# Hyperparameter tuning: Find the optimal number of clusters (k) using silhouette analysis
# for reproducibility
set.seed(42)  
# Range of k values to test
k_values <- 2:10 
silhouette_scores <- numeric(length(k_values))

# Loop through each value of k and compute silhouette scores
for (i in seq_along(k_values)) {
  k <- k_values[i]
  # Perform K-modes clustering with the given k
  kmodes_result <- kmodes(titanic_cluster_df, modes = k, iter.max = 100)
  cluster_labels <- kmodes_result$cluster
  
# Calculate the silhouette score for the current clustering
# Dissimilarity matrix
  dissimilarity <- daisy(titanic_cluster_df, metric = "gower")  
# Compute the silhouette
  ss <- silhouette(cluster_labels, dissimilarity) 
# Average silhouette score
  silhouette_scores[i] <- mean(ss[, "sil_width"])  
}

# Determine the optimal number of clusters based on the highest silhouette score
optimal_k <- k_values[which.max(silhouette_scores)]

# Perform K-modes clustering with the optimal number of clusters
kmodes_final <- kmodes(titanic_cluster_df, modes = optimal_k, iter.max = 100)

# Print the results
cat("Optimal number of clusters:", optimal_k, "\n")
cat("Cluster mode information:\n")
print(kmodes_final$modes)

# Visualize the clusters (for example, by Class and cluster)
ggplot(titanic_cluster_df, aes(x = Class, fill = factor(kmodes_final$cluster))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribution of Clusters by Class",
    x = "Class",
    y = "Count",
    fill = "Cluster"
  )
