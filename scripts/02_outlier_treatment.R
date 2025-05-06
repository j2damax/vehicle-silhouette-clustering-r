# ------------------------------------------------------------------------------
# Script Name: 02_outlier_treatment.R
# Purpose: Detect, visualize, and treat outliers (before and after winsorization)
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
# Step 2: Create Summary with Outlier Stats (Before Winsorization)
# ------------------------------------------------------------------------------
numeric_data <- features_imputed %>% select(where(is.numeric))

outlier_summary <- numeric_data %>%
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
    Outlier_Percent = round(100 * Num_Outliers / Total_Obs, 2)
  )

write.csv(outlier_summary, "outputs/tables/outlier_summary_before_winsorization.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 3: Create and Export Raw Long Format (Before Winsorization)
# ------------------------------------------------------------------------------
raw_long <- numeric_data %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(-row_id, names_to = "Feature", values_to = "Value")

# Enhance with outlier metadata
raw_long_enhanced <- raw_long %>%
  left_join(outlier_summary %>% select(Feature, Q1, Median, Q3, IQR, Lower_Bound, Upper_Bound), by = "Feature") %>%
  mutate(Is_Outlier = Value < Lower_Bound | Value > Upper_Bound)

write.csv(raw_long_enhanced, "outputs/tables/boxplot_raw_long_before_enhanced.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 4: Faceted Boxplot of All Features (Before Winsorization)
# ------------------------------------------------------------------------------
p_all_dark <- ggplot(raw_long, aes(x = "", y = Value)) +
  geom_boxplot(fill = "lightblue", color = "white", outlier.colour = "orange", outlier.shape = 21) +
  facet_wrap(~ Feature, scales = "free_y", ncol = 3) +
  theme_dark(base_size = 12) +
  labs(title = "Boxplots of All Features (Before Winsorization)", x = NULL, y = "Value") +
  theme(strip.text = element_text(color = "white"), axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"), plot.title = element_text(color = "white", face = "bold"))

ggsave("outputs/figures/boxplot_all_features_facet_before_winsorization.png", p_all_dark, width = 14, height = 10, dpi = 300)

# ------------------------------------------------------------------------------
# Step 5: Visualize Selected High-Outlier Features
# ------------------------------------------------------------------------------
selected_features <- c(
  "max.length_aspect_ratio", "pr.axis_aspect_ratio", "radius_ratio",
  "scaled_radius_of_gyration.1", "scaled_variance", "scaled_variance.1",
  "skewness_about", "skewness_about.1"
)

p_selected_dark <- raw_long %>%
  filter(Feature %in% selected_features) %>%
  ggplot(aes(x = "", y = Value)) +
  geom_boxplot(fill = "lightblue", color = "white", outlier.colour = "orange", outlier.shape = 21) +
  facet_wrap(~ Feature, scales = "free_y", ncol = 2) +
  theme_dark(base_size = 12) +
  labs(title = "Boxplots of High-Outlier Features", x = NULL, y = "Value") +
  theme(strip.text = element_text(color = "white"), axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"), plot.title = element_text(color = "white", face = "bold"))

ggsave("outputs/figures/boxplot_selected_high_outlier.png", p_selected_dark, width = 10, height = 8, dpi = 300)

# ------------------------------------------------------------------------------
# Step 6: Apply Manual Winsorization to Selected Features
# ------------------------------------------------------------------------------
winsorize_manual <- function(x, lower = 0.01, upper = 0.99) {
  qnt <- quantile(x, probs = c(lower, upper), na.rm = TRUE)
  x[x < qnt[1]] <- qnt[1]
  x[x > qnt[2]] <- qnt[2]
  return(x)
}

features_winsorized <- features_imputed
features_winsorized[selected_features] <- features_winsorized[selected_features] %>%
  mutate(across(everything(), winsorize_manual))

write.csv(features_winsorized, "outputs/tables/vehicle_features_winsorized_selected.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 7: Create Enhanced Long Format After Winsorization
# ------------------------------------------------------------------------------
winsorized_long_all <- features_winsorized %>%
  select(where(is.numeric)) %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(-row_id, names_to = "Feature", values_to = "Value")

outlier_summary_after <- winsorized_long_all %>%
  group_by(Feature) %>%
  summarise(
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Median = quantile(Value, 0.50, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR,
    Num_Outliers = sum(Value < (Q1 - 1.5 * IQR) | Value > (Q3 + 1.5 * IQR)),
    Total_Obs = n(),
    Outlier_Percent = round(100 * Num_Outliers / Total_Obs, 2),
    .groups = "drop"
  )

winsorized_long_enhanced <- winsorized_long_all %>%
  left_join(outlier_summary_after %>% select(-Num_Outliers, -Total_Obs, -Outlier_Percent), by = "Feature") %>%
  mutate(
    Is_Outlier = Value < Lower_Bound | Value > Upper_Bound,
    Treatment_Applied = case_when(
      Value == Lower_Bound ~ "Winsorized (Lower)",
      Value == Upper_Bound ~ "Winsorized (Upper)",
      TRUE ~ "Untouched"
    ),
    Outlier_Type = case_when(
      !Is_Outlier ~ "Normal",
      Value > Upper_Bound ~ "High Outlier",
      Value < Lower_Bound ~ "Low Outlier"
    )
  )

write.csv(winsorized_long_enhanced, "outputs/tables/boxplot_raw_long_after_enhanced_v2.csv", row.names = FALSE)
write.csv(outlier_summary_after, "outputs/tables/outlier_summary_after_winsorization.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 8: Outlier Count Comparison Bar Chart (Before vs After)
# ------------------------------------------------------------------------------
outlier_comparison <- outlier_summary %>%
  select(Feature, Num_Outliers) %>%
  rename(Outliers_Before = Num_Outliers) %>%
  left_join(
    outlier_summary_after %>% select(Feature, Num_Outliers) %>% rename(Outliers_After = Num_Outliers),
    by = "Feature"
  ) %>%
  pivot_longer(cols = starts_with("Outliers"), names_to = "Stage", values_to = "Count")

comparison_plot <- ggplot(outlier_comparison, aes(x = Feature, y = Count, fill = Stage)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    title = "Number of Outliers: Before vs After Winsorization",
    x = "Feature",
    y = "Number of Outliers"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  ) +
  scale_fill_manual(values = c("Outliers_Before" = "#FF6666", "Outliers_After" = "#66CC99"))

ggsave("outputs/figures/outlier_comparison_bar_chart.png", comparison_plot, width = 12, height = 6, dpi = 300)

print(comparison_plot)

# ------------------------------------------------------------------------------
# Step 9: Manually Treat Remaining Outliers in Selected Features
# ------------------------------------------------------------------------------

# Define function to replace outliers with median
replace_outliers_with_median <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  median_val <- median(x, na.rm = TRUE)
  x[x < lower_bound | x > upper_bound] <- median_val
  return(x)
}

# Apply manual treatment to selected features
features_winsorized[["max.length_aspect_ratio"]] <- replace_outliers_with_median(features_winsorized[["max.length_aspect_ratio"]])
features_winsorized[["scaled_radius_of_gyration.1"]] <- replace_outliers_with_median(features_winsorized[["scaled_radius_of_gyration.1"]])
features_winsorized[["skewness_about"]] <- replace_outliers_with_median(features_winsorized[["skewness_about"]])

# Save final cleaned data
write.csv(features_winsorized, "outputs/tables/vehicle_features_final_cleaned.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Step 10: Validate Final Cleaned Data After Manual Outlier Treatment
# ------------------------------------------------------------------------------

# Recreate long format from final cleaned data
final_long <- features_winsorized %>%
  select(where(is.numeric)) %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(-row_id, names_to = "Feature", values_to = "Value")

# Recalculate summary
final_outlier_summary <- final_long %>%
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

# Annotate final long data with outlier flags
final_long_enhanced <- final_long %>%
  left_join(final_outlier_summary %>% select(Feature, Q1, Median, Q3, IQR, Lower_Bound, Upper_Bound), by = "Feature") %>%
  mutate(
    Is_Outlier = Value < Lower_Bound | Value > Upper_Bound,
    Outlier_Type = case_when(
      !Is_Outlier ~ "Normal",
      Value > Upper_Bound ~ "High Outlier",
      Value < Lower_Bound ~ "Low Outlier"
    )
  )

# Create final boxplot for validation
p_final_dark <- ggplot(final_long_enhanced, aes(x = "", y = Value)) +
  geom_boxplot(fill = "lightblue", color = "white", outlier.colour = "orange", outlier.shape = 21) +
  facet_wrap(~ Feature, scales = "free_y", ncol = 3) +
  theme_dark(base_size = 12) +
  labs(title = "Boxplots of All Features (Final Cleaned Data)", x = NULL, y = "Value") +
  theme(strip.text = element_text(color = "white"), axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"), plot.title = element_text(color = "white", face = "bold"))

ggsave("outputs/figures/boxplot_final_cleaned_data.png", p_final_dark, width = 14, height = 10, dpi = 300)
# Print final summary
print(final_outlier_summary)
print(p_final_dark)

# Export for validation
write.csv(final_long_enhanced, "outputs/tables/boxplot_raw_long_final_enhanced.csv", row.names = FALSE)
write.csv(final_outlier_summary, "outputs/tables/outlier_summary_final.csv", row.names = FALSE)
