# ------------------------------------------------------------------------------
# Script Name: 05_kmeans_clustering.R
# Purpose: Perform K-Means clustering on PCA-reduced dataset and evaluate clusters
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(ggplot2)
library(gridExtra)
library(caret)

# ------------------------------------------------------------------------------
# Step 1: Load PCA-Reduced Data with Class
# ------------------------------------------------------------------------------
pca_data <- read.csv("outputs/tables/pca_top.csv")

# Separate features and class
features <- pca_data %>% select(-class)
class_labels <- pca_data$class

# ------------------------------------------------------------------------------
# Step 2: Compute WSS and Determine Optimal k (Elbow Method)
# ------------------------------------------------------------------------------
wss <- sapply(1:10, function(k) {
  kmeans(features, centers = k, nstart = 10)$tot.withinss
})

elbow_df <- data.frame(k = 1:10, WSS = wss)

# Plot and highlight elbow point
elbow_plot <- ggplot(elbow_df, aes(x = k, y = WSS)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "red", size = 2) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "darkred") +
  annotate("text", x = 3.3, y = max(elbow_df$WSS), label = "Elbow (k=3)",
           hjust = 0, color = "darkred", fontface = "bold") +
  labs(title = "Elbow Plot for K-Means Clustering",
       subtitle = "Dashed Line: Suggested Number of Clusters (k = 3)",
       x = "Number of Clusters (k)",
       y = "Within-cluster Sum of Squares (WSS)") +
  theme_light(base_size = 12)

ggsave("outputs/figures/kmeans_elbow_plot.png", elbow_plot, width = 8, height = 6, dpi = 300)

# Optional: Print elbow plot
print(elbow_plot)

# ------------------------------------------------------------------------------
# Step 3: Apply K-Means with Optimal k (k = 3)
# ------------------------------------------------------------------------------
k_opt <- 3
set.seed(42)
kmeans_model <- kmeans(features, centers = k_opt, nstart = 25)

# Add cluster labels to data
pca_data$cluster <- factor(kmeans_model$cluster)

# Save cluster assignments
write.csv(pca_data, "outputs/tables/kmeans_clustered_data.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 4: Cluster Visualization
# ------------------------------------------------------------------------------
cluster_plot <- fviz_cluster(kmeans_model, data = features, geom = "point",
                             ellipse.type = "convex", palette = "jco",
                             ggtheme = theme_minimal()) +
  labs(title = "K-Means Clustering Result (k = 3)")

ggsave("outputs/figures/kmeans_clusters_plot.png", cluster_plot, width = 8, height = 6, dpi = 300)
print(cluster_plot)

# ------------------------------------------------------------------------------
# Step 5: Compute Silhouette Score (Internal Validation)
# ------------------------------------------------------------------------------
silhouette_score <- silhouette(kmeans_model$cluster, dist(features))
silhouette_avg <- mean(silhouette_score[, 3])

# Save silhouette plot
silhouette_plot <- fviz_silhouette(silhouette_score) +
  labs(title = "Silhouette Plot for K-Means Clustering") +
  theme_minimal()

ggsave("outputs/figures/kmeans_silhouette_plot.png", silhouette_plot, width = 8, height = 6, dpi = 300)
print(silhouette_plot)

# Save silhouette score
write.csv(data.frame(Silhouette_Score = silhouette_avg), "outputs/tables/kmeans_silhouette_score.csv", row.names = FALSE)

------------------------------------------------------------------------------
# Step 6: Confusion Matrix vs True Class (External Validation)
# ------------------------------------------------------------------------------

confusion_matrix <- table(True = class_labels, Cluster = kmeans_model$cluster)
write.csv(confusion_matrix, "outputs/tables/kmeans_confusion_matrix.csv")
print(confusion_matrix)

# ------------------------------------------------------------------------------
