# ------------------------------------------------------------------------------
# Script Name: 06_hierarchical_clustering.R
# Purpose: Apply hierarchical clustering on PCA-reduced features and evaluate results
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(cluster)
library(factoextra)

# ------------------------------------------------------------------------------
# Step 1: Load PCA-reduced Data with Class
# ------------------------------------------------------------------------------
pca_data <- read.csv("outputs/tables/pca_top.csv")
features <- pca_data %>% select(-class)
class_labels <- pca_data$class

# ------------------------------------------------------------------------------
# Step 2: Compute Distance Matrix and Apply Hierarchical Clustering
# ------------------------------------------------------------------------------
dist_matrix <- dist(features, method = "euclidean")
hclust_model <- hclust(dist_matrix, method = "ward.D2")

# ------------------------------------------------------------------------------
# Step 3: Dendrogram with Highlighted Cut Line
# ------------------------------------------------------------------------------
png("outputs/figures/hierarchical_dendrogram.png", width = 1000, height = 600)
plot(hclust_model, labels = FALSE, main = "Dendrogram - Hierarchical Clustering")
height_cut <- 150  # Approximate height based on visual inspection to produce 3 clusters
abline(h = height_cut, col = "red", lty = 3)
dev.off()

# ------------------------------------------------------------------------------
# Step 4: Cut Tree and Assign Clusters (k = 3)
# ------------------------------------------------------------------------------
k <- 3
clusters <- cutree(hclust_model, k = k)
pca_data$cluster <- factor(clusters)

# Save clustered data
write.csv(pca_data, "outputs/tables/hierarchical_clustered_data.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 5: Visualize Clustered Data
# ------------------------------------------------------------------------------
hclust_plot <- fviz_cluster(list(data = features, cluster = clusters),
                            geom = "point", ellipse.type = "convex",
                            palette = "jco", ggtheme = theme_minimal()) +
  labs(title = "Hierarchical Clustering Result (k = 3)")

ggsave("outputs/figures/hierarchical_clusters_plot.png", hclust_plot, width = 8, height = 6, dpi = 300)
print(hclust_plot)

# ------------------------------------------------------------------------------
# Step 6: Silhouette Score
# ------------------------------------------------------------------------------
silhouette_score <- silhouette(clusters, dist_matrix)
silhouette_avg <- mean(silhouette_score[, 3])

silhouette_plot <- fviz_silhouette(silhouette_score) +
  labs(title = "Silhouette Plot - Hierarchical Clustering") +
  theme_minimal()

ggsave("outputs/figures/hierarchical_silhouette_plot.png", silhouette_plot, width = 8, height = 6, dpi = 300)
print(silhouette_plot)

write.csv(data.frame(Silhouette_Score = silhouette_avg), "outputs/tables/hierarchical_silhouette_score.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 7: Confusion Matrix vs Class Labels
# ------------------------------------------------------------------------------
confusion_matrix <- table(True = class_labels, Cluster = clusters)
write.csv(confusion_matrix, "outputs/tables/hierarchical_confusion_matrix.csv")
print(confusion_matrix)

# ------------------------------------------------------------------------------
# Step 8: Visual Comparison - True Class vs HC Clusters
# ------------------------------------------------------------------------------
p_class <- ggplot(pca_data, aes(x = PC1, y = PC2, color = class)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "True Class Labels", x = "PC1", y = "PC2") +
  theme_light()

p_cluster <- ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "Hierarchical Clustering Assignments", x = "PC1", y = "PC2") +
  theme_light()

png("outputs/figures/hierarchical_class_vs_cluster.png", width = 1000, height = 500)
grid.arrange(p_class, p_cluster, ncol = 2)
dev.off()
