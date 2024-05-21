library(factoextra)
library(cluster)
#load data
df <- USArrests

#print number of instances and number of instances
cat("Number of attributes:", ncol(data), "\n")
cat("Number of instances:", nrow(data), "\n")

#remove rows with missing values
df <- na.omit(df)
#scale each variable to have a mean of 0 and sd of 1
df <- scale(df)
#view first six rows of dataset
head(df)
#define linkage methods
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
#function to compute agglomerative coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}
#calculate agglomerative coefficient for each clustering linkage method
sapply(m, ac)

#perform hierarchical clustering using Ward's minimum variance
clust <- agnes(df, method = "ward")
#produce dendrogram
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 
#calculate gap statistic for each number of clusters (up to 10 clusters)
gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)
#produce plot of clusters vs. gap statistic
fviz_gap_stat(gap_stat)
#compute distance matrix
d <- dist(df, method = "euclidean")

#perform hierarchical clustering using Ward's method
final_clust <- hclust(d, method = "ward.D2" )
#cut the dendrogram into 4 clusters
groups <- cutree(final_clust, k=4)
#find number of observations in each cluster
table(groups)
#append cluster labels to original data
final_data <- cbind(USArrests, cluster = groups)
#display first six rows of final data
head(final_data)
#find mean values for each cluster
aggregate(final_data, by=list(cluster=final_data$cluster), mean)
# Fine-Tuning
gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)

# Find optimal number of clusters
optimal_clusters <- max(gap_stat$Tab[, "gap"], na.rm = TRUE)

# Perform clustering with optimal number of clusters
final_clust <- hclust(d, method = "ward.D2")
groups <- cutree(final_clust, k = optimal_clusters)

# Display mean values for each cluster
aggregate(final_data, by = list(cluster = groups), mean)

