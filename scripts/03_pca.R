# ------------------------------------------------------------------------------
# Script Name: 03_pca.R
# Purpose: Perform PCA on the final cleaned dataset and visualize results
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(factoextra)     # For PCA visualizations
library(ggplot2)        # For plotting

# ------------------------------------------------------------------------------
# Step 1: Load the Cleaned Dataset
# ------------------------------------------------------------------------------
features_final <- read.csv("outputs/tables/vehicle_features_final_cleaned.csv")

# Keep only numeric features
numeric_data <- features_final %>% select(where(is.numeric))

# ------------------------------------------------------------------------------
# Step 2: Standardize the Data
# ------------------------------------------------------------------------------
scaled_data <- scale(numeric_data)

# ------------------------------------------------------------------------------
# Step 3: Apply PCA
# ------------------------------------------------------------------------------
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Save PCA object
saveRDS(pca_result, file = "outputs/tables/pca_result.rds")

# ------------------------------------------------------------------------------
# Step 4: Scree Plot - Variance Explained by Components
# ------------------------------------------------------------------------------
png("outputs/figures/pca_scree_plot.png", width = 800, height = 600)
fviz_eig(pca_result, addlabels = TRUE, barfill = "steelblue", barcolor = "black") +
  labs(title = "Scree Plot: Variance Explained by Each Principal Component")
dev.off()

# ------------------------------------------------------------------------------
# Step 5: Cumulative Variance Explained
# ------------------------------------------------------------------------------
pca_var <- pca_result$sdev^2
total_var <- sum(pca_var)
pca_var_exp <- pca_var / total_var
cum_var_exp <- cumsum(pca_var_exp)

# Create data frame to save
var_df <- data.frame(
  PC = paste0("PC", 1:length(pca_var_exp)),
  Variance_Explained = round(pca_var_exp, 4),
  Cumulative_Explained = round(cum_var_exp, 4)
)

write.csv(var_df, "outputs/tables/pca_variance_explained.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 6: Biplot of First Two Principal Components
# ------------------------------------------------------------------------------
png("outputs/figures/pca_biplot_pc1_pc2.png", width = 800, height = 600)
fviz_pca_biplot(pca_result, repel = TRUE,
                col.var = "red", col.ind = "blue",
                label = "var") +
  labs(title = "PCA Biplot: PC1 vs PC2")
dev.off()

# ------------------------------------------------------------------------------
# Step 7: Interpretation - How Many PCs to Retain?
# ------------------------------------------------------------------------------
# Rule of thumb: retain components explaining 85-95% of cumulative variance
retain_components <- which(cum_var_exp >= 0.95)[1]
cat("Number of components to retain for 95% variance explained:", retain_components, "\n")

# ------------------------------------------------------------------------------
# Step 8: Save PCA Results
# ------------------------------------------------------------------------------
# Save PCA results to a CSV file
pca_results_df <- as.data.frame(pca_result$x)
pca_results_df$class <- features_final$class
write.csv(pca_results_df, "outputs/tables/pca_results.csv", row.names = FALSE)
