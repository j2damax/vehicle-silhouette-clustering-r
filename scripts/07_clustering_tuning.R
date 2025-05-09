# ------------------------------------------------------------------------------
# Script Name: 07_clustering_tuning.R
# Purpose: Experiment with clustering parameters to improve cluster quality
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(NbClust)

# ------------------------------------------------------------------------------
# Step 1: Load Full PCA Result with Class Labels
# ------------------------------------------------------------------------------
pca_data <- read.csv("outputs/tables/pca_results.csv")
true_labels <- pca_data$class
pca_features <- pca_data %>% select(-class)

# ------------------------------------------------------------------------------
# Step 2: Try Alternative Number of PCs
# ------------------------------------------------------------------------------
top_pcs_variants <- list(
  PC2 = 2,
  PC6 = 6,
  PC10 = 10
)

results <- list()

for (variant in names(top_pcs_variants)) {
  top_k <- top_pcs_variants[[variant]]
  available_k <- min(top_k, ncol(pca_features))
  data_subset <- pca_features[, 1:available_k]
  
  # Elbow Plot
  elbow_plot <- fviz_nbclust(data_subset, kmeans, method = "wss") +
    labs(title = paste("Elbow Plot -", variant))
  ggsave(paste0("outputs/figures/tuning_elbow_", variant, ".png"), elbow_plot, width = 8, height = 5)
  
  # Silhouette Analysis for k = 2 to 6
  sil_plot <- fviz_nbclust(data_subset, kmeans, method = "silhouette") +
    labs(title = paste("Silhouette Plot -", variant))
  ggsave(paste0("outputs/figures/tuning_silhouette_", variant, ".png"), sil_plot, width = 8, height = 5)
  
  # Choose best k from silhouette and re-run kmeans (default to 4 if unclear)
  set.seed(123)
  kmeans_model <- kmeans(data_subset, centers = 4, nstart = 25)
  
  # Silhouette score
  silhouette_score <- silhouette(kmeans_model$cluster, dist(data_subset))
  silhouette_avg <- mean(silhouette_score[, 3])
  
  # Confusion Matrix
  confusion_matrix <- table(true_labels, kmeans_model$cluster)
  
  # Save visual
  cluster_plot <- fviz_cluster(kmeans_model, data = data_subset, geom = "point") +
    labs(title = paste("K-Means Clusters -", variant))
  ggsave(paste0("outputs/figures/tuning_cluster_plot_", variant, ".png"), cluster_plot, width = 8, height = 6)
  
  # Save Results
  results[[variant]] <- list(
    top_k = available_k,
    silhouette = silhouette_avg,
    confusion = confusion_matrix
  )
}

# ------------------------------------------------------------------------------
# Step 3: Summarize Results
# ------------------------------------------------------------------------------
summary_df <- tibble(
  Variant = names(results),
  Top_PCs = sapply(results, function(x) x$top_k),
  Silhouette_Score = sapply(results, function(x) round(x$silhouette, 4))
)

write.csv(summary_df, "outputs/tables/tuning_clustering_summary.csv", row.names = FALSE)

cat("\u2705 Clustering tuning completed. Results saved in outputs/tables and outputs/figures.\n")
