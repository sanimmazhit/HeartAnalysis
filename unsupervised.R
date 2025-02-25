# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(scales)
library(gridExtra)
library(cluster)
library(factoextra)

# Load the dataset
file_path <- "/Users/skmazhit/Desktop/ind project /Sanim Mazhit - 33176A/heart.csv"
data <- read.csv(file_path)

# Ensure categorical columns are factors
data <- data %>% mutate(across(where(is.character), as.factor))

# Scale numeric data for clustering
scaled_data <- data %>% mutate(across(where(is.numeric), ~ scale(.) %>% as.numeric()))
numeric_data <- scaled_data %>% select(where(is.numeric))

# 1. K-Means Clustering

# Determine the optimal number of clusters using the Elbow method
fviz_nbclust(numeric_data, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters")

# Determine the optimal number of clusters using Silhouette method
fviz_nbclust(numeric_data, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method for Optimal Clusters")

# Apply K-Means with a chosen number of clusters (e.g., k = 3)
kmeans_model <- kmeans(numeric_data, centers = 3, nstart = 25)
print("K-Means Clustering Results:")
print(kmeans_model$cluster)

# Visualize K-Means Clusters
fviz_cluster(kmeans_model, data = numeric_data, geom = "point") +
  labs(title = "K-Means Clustering")

# Compute silhouette scores for K-Means
silhouette_kmeans <- silhouette(kmeans_model$cluster, dist(numeric_data))
print("Average Silhouette Width for K-Means:")
print(mean(silhouette_kmeans[, 3]))

# Visualize silhouette plot for K-Means
fviz_silhouette(silhouette_kmeans) +
  labs(title = "Silhouette Plot for K-Means Clustering")

# 2. Hierarchical Clustering

# Compute distance matrix
dist_matrix <- dist(numeric_data, method = "euclidean")

# Apply hierarchical clustering using complete linkage
hclust_model <- hclust(dist_matrix, method = "complete")

# Plot the dendrogram
plot(hclust_model, main = "Dendrogram", xlab = "Observations", ylab = "Height")

# Cut the dendrogram into clusters (e.g., k = 3)
hclust_clusters <- cutree(hclust_model, k = 3)
print("Hierarchical Clustering Results:")
print(hclust_clusters)

# Visualize hierarchical clustering results
fviz_dend(hclust_model, k = 3, rect = TRUE, show_labels = FALSE) +
  labs(title = "Hierarchical Clustering Dendrogram")

# Compute silhouette scores for Hierarchical Clustering
silhouette_hclust <- silhouette(hclust_clusters, dist_matrix)
print("Average Silhouette Width for Hierarchical Clustering:")
print(mean(silhouette_hclust[, 3]))

# Visualize silhouette plot for Hierarchical Clustering
fviz_silhouette(silhouette_hclust) +
  labs(title = "Silhouette Plot for Hierarchical Clustering")
