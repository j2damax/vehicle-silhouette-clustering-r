# ------------------------------------------------------------------------------
# Script Name: 06_hierarchical_clustering.R
# Purpose: Perform Agglomerative Hierarchical Clustering on PCA-reduced data
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(cluster)       # For silhouette()
library(factoextra)    # For fviz_dend, fviz_cluster
library(dendextend)    # For colored dendrogram

# ------------------------------------------------------------------------------
# Step 1: Load PCA-Reduced Data (top components)
# ------------------------------------------------------------------------------
pca_data <- read.csv("outputs/tables/pca_top.csv")

# ------------------------------------------------------------------------------
# Step 2: Compute Distance Matrix
# ------------------------------------------------------------------------------
distance_matrix <- dist(pca_data, method = "euclidean")

# ------------------------------------------------------------------------------
# Step 3: Perform Hierarchical Clustering (Agglomerative)
# ------------------------------------------------------------------------------
hc_model <- hclust(distance_matrix, method = "ward.D2")

# Save dendrogram plot
png("outputs/figures/hierarchical_dendrogram.png", width = 1000, height = 600)
plot(hc_model, cex = 0.6, hang = -1, main = "Hierarchical Clustering Dendrogram")
abline(h = 150, col = "red", lty = 2)  # Example cut height (adjust as needed)
dev.off()

# ------------------------------------------------------------------------------
# Step 4: Determine Number of Clusters
# ------------------------------------------------------------------------------
# Let's assume we want 4 clusters (as with K-means)
hc_clusters <- cutree(hc_model, k = 4)

# Append to PCA data
pca_data$cluster <- factor(hc_clusters)

# ------------------------------------------------------------------------------
# Step 5: Visualize Cluster Assignments (PC1 vs PC2)
# ------------------------------------------------------------------------------
p_hc_cluster <- fviz_cluster(list(data = pca_data[, 1:(ncol(pca_data)-1)], cluster = hc_clusters),
                             geom = "point",
                             ellipse.type = "convex",
                             palette = "jco",
                             ggtheme = theme_minimal()) +
  labs(title = "Hierarchical Clustering (Ward.D2) on PCA-Reduced Data")

ggsave("outputs/figures/hierarchical_clusters_plot.png", p_hc_cluster, width = 10, height = 8, dpi = 300)

# ------------------------------------------------------------------------------
# Step 6: Silhouette Score for Hierarchical Clustering
# ------------------------------------------------------------------------------
silhouette_score_hc <- silhouette(hc_clusters, distance_matrix)
silhouette_avg_hc <- mean(silhouette_score_hc[, 3])
cat("Average Silhouette Score (Hierarchical):", silhouette_avg_hc, "\n")

# Save silhouette plot
silhouette_plot_hc <- fviz_silhouette(silhouette_score_hc) +
  labs(title = "Silhouette Plot for Hierarchical Clustering") +
  theme_minimal()

ggsave("outputs/figures/hierarchical_silhouette_plot.png", silhouette_plot_hc, width = 8, height = 6, dpi = 300)

# Save silhouette score
write.csv(data.frame(Silhouette_Score = silhouette_avg_hc), "outputs/tables/hierarchical_silhouette_score.csv", row.names = FALSE)

# Save final cluster labels
write.csv(pca_data, "outputs/tables/pca_top_with_hc_clusters.csv", row.names = FALSE)

# Print silhouette plot
print(silhouette_plot_hc)
