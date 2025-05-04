# ------------------------------------------------------------------------------
# Script Name: 01_data_preprocessing.R
# Purpose: Data preprocessing for clustering analysis 
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)      # Data manipulation and visualization
library(mice)        # Multivariate Imputation by Chained Equations

# Read the dataset 
vehicles <- read.csv("data/vehicles.csv")

# View first few rows
head(vehicles)

# Exclude class for numeric analysis
features_only <- vehicles %>% select(-class)

# Basic structure and summary
str(vehicles)
summary(features_only)

# Convert summary to a data frame
summary_df <- as.data.frame(do.call(cbind, lapply(features_only, summary)))

# Transpose for better readability
summary_df_t <- as.data.frame(t(summary_df))

# Add column names
colnames(summary_df_t) <- c("Min", "1st_Qu", "Median", "Mean", "3rd_Qu", "Max")

# Create output folder if not exist
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# Write to CSV
write.csv(summary_df_t, "output/tables/vehicle_feature_summary.csv", row.names = TRUE)

# ----- Boxplot for Outlier Detection -----
# Create output folder if not exist
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# Save boxplot of all scaled features
png("output/figures/boxplot_scaled_features.png", width = 1000, height = 600)
boxplot(scale(features_only), las = 2, col = "orange", main = "Boxplots of Scaled Features")
dev.off()

# Check Missing Values
missing_counts <- colSums(is.na(features_only))
total_counts <- nrow(features_only)

missing_df <- data.frame(
  Feature = names(missing_counts),
  Missing_Values = missing_counts,
  Total_Observations = total_counts,
  Missing_Percent = round((missing_counts / total_counts) * 100, 2)
)

# Save to CSV
write.csv(missing_df, "output/tables/missing_value_report.csv", row.names = FALSE)

print(missing_df)

# Impute using MICE (PMM)
# Run mice on features_only (Predictive Mean Matching by default)
mice_result <- mice(features_only, m = 1, method = 'pmm', maxit = 5, seed = 123)

# Extract the completed dataset
features_imputed <- complete(mice_result, 1)

# Verify that no missing values remain
stopifnot(sum(is.na(features_imputed)) == 0)
cat("✅ Missing values imputed using MICE (PMM method).\n")

# Save the imputed dataset
write.csv(features_imputed, "output/tables/vehicle_features_imputed.csv", row.names = FALSE)

