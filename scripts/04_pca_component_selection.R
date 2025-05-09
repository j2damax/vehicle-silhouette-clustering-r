# ------------------------------------------------------------------------------
# Script Name: 04_pca_component_selection.R
# Purpose: Analyze PCA results and select number of components for clustering
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(ggplot2)

# ------------------------------------------------------------------------------
# Step 1: Load PCA Model (from 03_pca.R)
# ------------------------------------------------------------------------------
pca_model <- readRDS("outputs/tables/pca_result.rds")

# ------------------------------------------------------------------------------
# Step 2: Extract Variance Explained
# ------------------------------------------------------------------------------
pca_variance <- pca_model$sdev^2
variance_explained <- pca_variance / sum(pca_variance)
cumulative_variance <- cumsum(variance_explained)

# Create a summary data frame
pca_summary_df <- data.frame(
  PC = paste0("PC", 1:length(variance_explained)),
  Variance_Explained = round(variance_explained, 4),
  Cumulative_Variance = round(cumulative_variance, 4)
)

# Save the summary table
write.csv(pca_summary_df, "outputs/tables/pca_variance_summary.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 3: Visualize Cumulative Variance
# ------------------------------------------------------------------------------
p_cumulative <- ggplot(pca_summary_df, aes(x = 1:nrow(pca_summary_df), y = Cumulative_Variance)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "red", size = 2) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "gray") +
  labs(
    title = "Cumulative Variance Explained by Principal Components",
    x = "Number of Principal Components",
    y = "Cumulative Variance Explained"
  ) +
  theme_light()

ggsave("outputs/figures/pca_cumulative_variance_plot.png", p_cumulative, width = 10, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# Step 4: Apply Strategy to Select Components
# ------------------------------------------------------------------------------
# Manual strategy: Select top 2 components
message("Manually selected top 2 principal components")
top_k <- 2

# ------------------------------------------------------------------------------
# Step 5: Extract and Save Top k Principal Components
# ------------------------------------------------------------------------------
pca_scores <- as.data.frame(pca_model$x)

# Ensure class column is available
pca_results_full <- read.csv("outputs/tables/pca_results.csv")
pca_scores$class <- pca_results_full$class

pca_top_k <- pca_scores[, c(1:top_k, ncol(pca_scores))]

write.csv(pca_top_k, "outputs/tables/pca_top.csv", row.names = FALSE)
cat("\u2705 Top Principal Components extracted and saved for clustering:", top_k, "\n")

