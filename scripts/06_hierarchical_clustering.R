# ------------------------------------------------------------------------------
# Script Name: 06_hierarchical_clustering.R
# Purpose: Perform Hierarchical Clustering on PCA-reduced dataset and evaluate clusters
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)
library(caret)

# ------------------------------------------------------------------------------
# Step 1: Load PCA Data
# ------------------------------------------------------------------------------
pca_data <- read.csv("outputs/tables/pca_top.csv")
pca_features <- pca_data %>% select(-class)

# ------------------------------------------------------------------------------
# Step 2: Compute Distance Matrix
# ------------------------------------------------------------------------------
dist_matrix <- dist(pca_features, method = "euclidean")

# ------------------------------------------------------------------------------
# Step 3: Hierarchical Clustering (Ward’s Method)
# ------------------------------------------------------------------------------
hc_model <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
png("outputs/figures/hierarchical_dendrogram.png", width = 900, height = 600)
plot(hc_model, labels = FALSE, hang = -1, main = "Hierarchical Clustering Dendrogram")
abline(h = 100, col = "red", lty = 2)  # Cut line (adjust as needed)
dev.off()

# ------------------------------------------------------------------------------
# Step 4: Cut Tree to Form Clusters (k = 4)
# ------------------------------------------------------------------------------
k <- 4
clusters <- cutree(hc_model, k = k)
pca_data$cluster <- factor(clusters)

# ------------------------------------------------------------------------------
# Step 5: Visualize Cluster Assignments on PC1-PC2
# ------------------------------------------------------------------------------
hc_cluster_plot <- fviz_cluster(list(data = pca_features, cluster = clusters),
                                ellipse.type = "convex", palette = "jco",
                                ggtheme = theme_minimal(), main = "Hierarchical Clustering (PC1 vs PC2)")

ggsave("outputs/figures/hierarchical_clusters_plot.png", hc_cluster_plot, width = 8, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# Step 6: Silhouette Score Evaluation
# ------------------------------------------------------------------------------
sil_score_hc <- silhouette(clusters, dist_matrix)
silhouette_avg_hc <- mean(sil_score_hc[, 3])
cat("Average Silhouette Score (Hierarchical):", silhouette_avg_hc, "\n")

silhouette_plot_hc <- fviz_silhouette(sil_score_hc) +
  labs(title = "Silhouette Plot for Hierarchical Clustering") +
  theme_light()

ggsave("outputs/figures/hierarchical_silhouette_plot.png", silhouette_plot_hc, width = 8, height = 6, dpi = 300)

write.csv(data.frame(Silhouette_Score = silhouette_avg_hc), "outputs/tables/hierarchical_silhouette_score.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 7: Cluster vs Class Label Evaluation (Confusion Matrix)
# ------------------------------------------------------------------------------
conf_mat_hc <- table(True_Class = pca_data$class, Predicted_Cluster = pca_data$cluster)
print(conf_mat_hc)
write.csv(conf_mat_hc, "outputs/tables/hierarchical_confusion_matrix.csv")

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
