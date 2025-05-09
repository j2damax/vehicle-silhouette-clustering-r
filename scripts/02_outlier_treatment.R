# ------------------------------------------------------------------------------
# Script Name: 02_outlier_treatment.R
# Purpose: Detect, visualize, and treat outliers using adaptive IQR-based Winsorization
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)  # Data manipulation and visualization
library(psych)      # Descriptive statistics

# ------------------------------------------------------------------------------
# Step 1: Load Imputed Dataset
# ------------------------------------------------------------------------------
features_imputed <- read.csv("outputs/tables/vehicle_features_imputed.csv")

# Create output folders
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Step 2: Create Summary with Outlier Stats (Before Treatment)
# ------------------------------------------------------------------------------
numeric_data <- features_imputed %>% select(where(is.numeric))

outlier_summary_before <- numeric_data %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Value") %>%
  group_by(Feature) %>%
  summarise(
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Median = quantile(Value, 0.50, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR,
    Num_Outliers = sum(Value < Lower_Bound | Value > Upper_Bound),
    Total_Obs = n(),
    Outlier_Percent = round(100 * Num_Outliers / Total_Obs, 2),
    .groups = "drop"
  )

write.csv(outlier_summary_before, "outputs/tables/outlier_summary_before_winsorization.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 3: Adaptive IQR-Based Winsorization Function
# ------------------------------------------------------------------------------
winsorize_iqr <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

# Apply winsorization to all numeric features
features_winsorized <- features_imputed
features_winsorized[names(numeric_data)] <- features_winsorized[names(numeric_data)] %>%
  mutate(across(everything(), winsorize_iqr))

# Save cleaned dataset
write.csv(features_winsorized, "outputs/tables/vehicle_features_final_cleaned.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 4: Post-Winsorization Outlier Summary
# ------------------------------------------------------------------------------
numeric_data_after <- features_winsorized %>% select(where(is.numeric))

outlier_summary_after <- numeric_data_after %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Value") %>%
  group_by(Feature) %>%
  summarise(
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Median = quantile(Value, 0.50, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR,
    Num_Outliers = sum(Value < Lower_Bound | Value > Upper_Bound),
    Total_Obs = n(),
    Outlier_Percent = round(100 * Num_Outliers / Total_Obs, 2),
    .groups = "drop"
  )

write.csv(outlier_summary_after, "outputs/tables/outlier_summary_after_winsorization.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 5: Boxplot Comparison (Optional Visualization)
# ------------------------------------------------------------------------------
library(ggplot2)

before_long <- numeric_data %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(-row_id, names_to = "Feature", values_to = "Value") %>%
  mutate(State = "Before")

after_long <- numeric_data_after %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(-row_id, names_to = "Feature", values_to = "Value") %>%
  mutate(State = "After")

combined_long <- bind_rows(before_long, after_long)

p_compare <- ggplot(combined_long, aes(x = State, y = Value, fill = State)) +
  geom_boxplot(outlier.size = 0.8, outlier.color = "orange") +
  facet_wrap(~ Feature, scales = "free_y", ncol = 3) +
  theme_light(base_size = 12) +
  labs(title = "Outlier Treatment Comparison (Before vs After IQR Winsorization)",
       x = NULL, y = "Value")

ggsave("outputs/figures/outlier_treatment_comparison.png", p_compare, width = 14, height = 10, dpi = 300)
