# ------------------------------------------------------------------------------
# Script Name: 04_pca_component_selection.R
# Purpose: Analyze PCA results and select number of components for clustering
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(ggplot2)
library(scales)

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
  Variance_Explained = variance_explained,
  Cumulative_Variance = cumulative_variance
)

# Save summary table
write.csv(pca_summary_df, "outputs/tables/pca_variance_summary.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 3: Visualize Cumulative Variance
# ------------------------------------------------------------------------------
top_k <- which(cumulative_variance >= 0.9)[1]  # Strategy: 90% cumulative variance

p_cumulative <- ggplot(pca_summary_df, aes(x = 1:nrow(pca_summary_df), y = Cumulative_Variance)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "red", size = 2) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "gray") +
  annotate("text", x = top_k, y = cumulative_variance[top_k] + 0.03,
           label = paste0("Top ", top_k, " PCs (", round(cumulative_variance[top_k]*100), "%)"),
           color = "darkred", size = 5) +
  labs(
    title = "Cumulative Variance Explained by Principal Components",
    x = "Number of Principal Components",
    y = "Cumulative Variance Explained"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal()

ggsave("outputs/figures/pca_cumulative_variance_plot.png", p_cumulative, width = 10, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# Step 4: Bar Chart of Variance Explained per PC
# ------------------------------------------------------------------------------
p_individual <- ggplot(pca_summary_df, aes(x = PC, y = Variance_Explained)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = percent(Variance_Explained, accuracy = 0.1)),
            vjust = -0.5, size = 3.5) +
  labs(
    title = "Variance Explained by Each Principal Component",
    x = "Principal Component", y = "Variance Explained"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal()

ggsave("outputs/figures/pca_variance_per_pc.png", p_individual, width = 10, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# Step 5: Extract and Save Top k Principal Components
# ------------------------------------------------------------------------------
pca_scores <- as.data.frame(pca_model$x)
pca_top_k <- pca_scores[, 1:top_k]

# Optionally add class label for visualization purposes later
full_data <- read.csv("outputs/tables/vehicle_features_final_cleaned.csv")
if ("class" %in% names(full_data)) {
  pca_top_k$class <- full_data$class
  write.csv(pca_top_k, "outputs/tables/pca_top_with_class.csv", row.names = FALSE)
}

# Save plain PCA top-k dataset
write.csv(pca_top_k[, 1:top_k], "outputs/tables/pca_top.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 6: Save Summary Note
# ------------------------------------------------------------------------------
writeLines(
  c(
    "PCA Component Selection Summary:",
    paste("Strategy Used: Cumulative Variance Threshold ≥ 90%"),
    paste("Top", top_k, "components retained."),
    paste("Cumulative variance explained:", round(cumulative_variance[top_k]*100, 2), "%")
  ),
  "outputs/tables/pca_selection_notes.txt"
)

cat("✅ PCA component selection and export completed successfully.\n")
