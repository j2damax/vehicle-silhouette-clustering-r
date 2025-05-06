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

# Extract class column separately before imputation
class_col <- vehicles$class

# Exclude class column for numeric analysis
features_only <- vehicles %>% select(-class)

data.frame(
  Column = names(features_only),
  Class = sapply(features_only, class)
)

# ----- Step 2: Summary Statistics -----
summary_df <- as.data.frame(do.call(cbind, lapply(features_only, summary)))
summary_df_t <- as.data.frame(t(summary_df))
colnames(summary_df_t) <- c("Min", "1st_Qu", "Median", "Mean", "3rd_Qu", "Max")

# Create output folders if not exist
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

write.csv(summary_df_t, "outputs/tables/vehicle_feature_summary.csv", row.names = TRUE)

# ----- Step 3: Find duplicate rows -----
duplicates <- features_only[duplicated(features_only), ]
if (nrow(duplicates) > 0) {
  cat("Duplicate rows found:\n")
  print(duplicates)
} else {
  cat("No duplicate rows found.\n")
}

# ----- Step 4: Missing Value Report -----
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

# ----- Step 5: Imputation Using MICE (PMM) -----
mice_result <- mice(features_only, m = 1, method = 'pmm', maxit = 5, seed = 123)
features_imputed <- complete(mice_result, 1)

# Add class column back
features_imputed$class <- class_col

# Confirm no missing values after imputation
stopifnot(sum(is.na(features_imputed)) == 0)

# Save imputed dataset
write.csv(features_imputed, "outputs/tables/vehicle_features_imputed.csv", row.names = FALSE)
