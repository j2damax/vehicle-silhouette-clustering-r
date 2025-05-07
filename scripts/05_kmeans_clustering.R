# ------------------------------------------------------------------------------
# Script Name: 05_kmeans_clustering.R
# Purpose: Perform K-Means clustering on PCA-reduced data and visualize results
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(factoextra)    # Clustering visualization
library(cluster)       # Clustering functions

# ------------------------------------------------------------------------------
# Step 1: Load PCA-Reduced Data
# ------------------------------------------------------------------------------
pca_data <- read.csv("outputs/tables/pca_top.csv")

# ------------------------------------------------------------------------------
# Step 2: Determine Optimal Number of Clusters (Elbow Method)
# ------------------------------------------------------------------------------
set.seed(123)

wss <- map_dbl(1:10, function(k) {
  kmeans(pca_data, centers = k, nstart = 25)$tot.withinss
})

elbow_df <- data.frame(k = 1:10, WSS = wss)

elbow_plot <- ggplot(elbow_df, aes(x = k, y = WSS)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "red", size = 2) +
  labs(title = "Elbow Method for Optimal Clusters", x = "Number of Clusters (k)", y = "Total Within-Cluster Sum of Squares") +
  theme_minimal()

ggsave("outputs/figures/kmeans_elbow_plot.png", elbow_plot, width = 8, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# Step 3: Apply K-Means Clustering (choose optimal k, e.g., k = 4)
# ------------------------------------------------------------------------------
optimal_k <- 4  # Change this if elbow suggests another
kmeans_model <- kmeans(pca_data, centers = optimal_k, nstart = 25)

# Append cluster labels to data
pca_data$Cluster <- factor(kmeans_model$cluster)

# Save clustered data
write.csv(pca_data, "outputs/tables/pca_kmeans_clusters.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 4: Visualize Clusters
# ------------------------------------------------------------------------------
cluster_plot <- fviz_cluster(kmeans_model, data = pca_data[, 1:(ncol(pca_data)-1)],
                             palette = "Set2", geom = "point", ellipse.type = "norm",
                             ggtheme = theme_minimal(), main = "K-Means Clustering Result (PCA-Reduced Data)")

ggsave("outputs/figures/kmeans_clusters_plot.png", cluster_plot, width = 10, height = 8, dpi = 300)

# Print to viewer
print(elbow_plot)
print(cluster_plot)
