# ------------------------------------------------------------------------------
# Script Name: 01_data_preprocessing.R
# Purpose: Data preprocessing for clustering analysis 
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)   # Data manipulation and visualization
library(mice)        # Multivariate Imputation by Chained Equations

# ----- Step 1: Load & Inspect Dataset -----
vehicles <- read.csv("data/vehicles.csv")
head(vehicles)
str(vehicles)

# Exclude class column for numeric analysis
features_only <- vehicles %>% select(-class)

# ----- Step 2: Summary Statistics -----
summary_df <- as.data.frame(do.call(cbind, lapply(features_only, summary)))
summary_df_t <- as.data.frame(t(summary_df))
colnames(summary_df_t) <- c("Min", "1st_Qu", "Median", "Mean", "3rd_Qu", "Max")

# Create output folders if not exist
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

write.csv(summary_df_t, "outputs/tables/vehicle_feature_summary.csv", row.names = TRUE)

# ----- Step 3: Missing Value Report -----
missing_counts <- colSums(is.na(features_only))
total_counts <- nrow(features_only)

missing_df <- data.frame(
  Feature = names(missing_counts),
  Missing_Values = missing_counts,
  Total_Observations = total_counts,
  Missing_Percent = round((missing_counts / total_counts) * 100, 2)
)

write.csv(missing_df, "outputs/tables/missing_value_report.csv", row.names = FALSE)
print(missing_df)

# ----- Step 4: Imputation Using MICE (PMM) -----
mice_result <- mice(features_only, m = 1, method = 'pmm', maxit = 5, seed = 123)
features_imputed <- complete(mice_result, 1)

# Confirm no missing values after imputation
stopifnot(sum(is.na(features_imputed)) == 0)
cat("✅ Missing values imputed using MICE. No NA values remain.\n")

# Save imputed dataset
write.csv(features_imputed, "outputs/tables/vehicle_features_imputed.csv", row.names = FALSE)

# ----- Step 5: Correlation Matrix -----
