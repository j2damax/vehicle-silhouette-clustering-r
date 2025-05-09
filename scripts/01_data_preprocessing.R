# ------------------------------------------------------------------------------
# Script Name: 01_data_preprocessing.R
# Purpose: Handle missing values and summarize feature distribution
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(mice)
library(psych)

# ------------------------------------------------------------------------------
# Step 1: Load Data
# ------------------------------------------------------------------------------
vehicles <- read.csv("data/vehicles.csv")
features_only <- vehicles %>% select(-class)

# ------------------------------------------------------------------------------
# Step 2: Summary Statistics
# ------------------------------------------------------------------------------
summary_list <- lapply(features_only %>% select(where(is.numeric)), summary)
summary_df <- do.call(rbind, summary_list) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Feature")
write.csv(summary_df, "outputs/tables/vehicle_feature_summary.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 3: Check for Missing Values
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# Step 4: Imputation Using MICE (PMM)
# ------------------------------------------------------------------------------
# Suppress logging output
mice_result <- suppressMessages(
  mice(features_only, m = 5, method = 'pmm', maxit = 50, seed = 123)
)

# Save convergence plot
pdf("outputs/figures/mice_convergence_plot.pdf")
plot(mice_result)
dev.off()

# Extract completed dataset
features_imputed <- complete(mice_result, 1)

# Add class column back
features_imputed$class <- vehicles$class

# Save imputed dataset
write.csv(features_imputed, "outputs/tables/vehicle_features_imputed.csv", row.names = FALSE)

# Check post-imputation missing values
post_missing <- colSums(is.na(features_imputed))
print(post_missing)
