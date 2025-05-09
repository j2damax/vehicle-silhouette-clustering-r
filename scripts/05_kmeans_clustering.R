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
# Step 1: Load PCA-reduced Data
# ------------------------------------------------------------------------------
pca_data <- read.csv("outputs/tables/pca_top.csv")
pca_features <- pca_data %>% select(-class)

# ------------------------------------------------------------------------------
# Step 2: Determine Optimal Number of Clusters (Elbow Method)
# ------------------------------------------------------------------------------
wss <- map_dbl(1:10, function(k) {
  kmeans(pca_features, centers = k, nstart = 25)$tot.withinss
})

elbow_df <- data.frame(k = 1:10, WSS = wss)

elbow_plot <- ggplot(elbow_df, aes(x = k, y = WSS)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "red", size = 2) +
  labs(title = "Elbow Plot for K-Means Clustering", x = "Number of Clusters (k)", y = "Within-cluster Sum of Squares") +
  theme_light()

ggsave("outputs/figures/kmeans_elbow_plot.png", elbow_plot, width = 8, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# Step 3: Apply K-Means Clustering with Optimal k = 2 (based on visual inspection)
# ------------------------------------------------------------------------------
k <- 2
kmeans_model <- kmeans(pca_features, centers = k, nstart = 25)
pca_data$Cluster <- factor(kmeans_model$cluster)

# ------------------------------------------------------------------------------
# Step 4: Visualize Clusters
# ------------------------------------------------------------------------------
kmeans_cluster_plot <- fviz_cluster(kmeans_model, data = pca_features, geom = "point", ellipse.type = "convex", palette = "jco") +
  labs(title = "K-Means Clusters (Top Principal Components)")

ggsave("outputs/figures/kmeans_clusters_plot.png", kmeans_cluster_plot, width = 8, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# Step 5: Compute Silhouette Score (Internal Validation)
# ------------------------------------------------------------------------------
silhouette_score <- silhouette(kmeans_model$cluster, dist(pca_features))
silhouette_avg <- mean(silhouette_score[, 3])
cat("Average Silhouette Score: ", silhouette_avg, "\n")

silhouette_plot <- fviz_silhouette(silhouette_score) +
  labs(title = "Silhouette Plot for K-Means Clustering") +
  theme_minimal()

ggsave("outputs/figures/kmeans_silhouette_plot.png", silhouette_plot, width = 8, height = 6, dpi = 300)

write.csv(data.frame(Silhouette_Score = silhouette_avg), "outputs/tables/kmeans_silhouette_score.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 6: Cluster vs Class Label Evaluation
# ------------------------------------------------------------------------------

conf_mat <- table(True_Class = pca_data$class, Predicted_Cluster = pca_data$Cluster)
print(conf_mat)
write.csv(conf_mat, "outputs/tables/kmeans_confusion_matrix.csv")

# ------------------------------------------------------------------------------
# Step 7: Visualize Class vs Cluster
# ------------------------------------------------------------------------------
p_class <- ggplot(pca_data, aes(x = PC1, y = PC2, color = class)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "True Class Labels", x = "PC1", y = "PC2") +
  theme_light()

p_cluster <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "K-Means Cluster Assignments", x = "PC1", y = "PC2") +
  theme_light()

png("outputs/figures/kmeans_class_vs_cluster.png", width = 1000, height = 500)
grid.arrange(p_class, p_cluster, ncol = 2)
dev.off()
