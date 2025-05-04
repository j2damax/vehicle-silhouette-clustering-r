# 00_setup.R
# This script installs and loads all necessary libraries for the clustering analysis project

# List of required packages
required_packages <- c(
  "tidyverse",      # Data manipulation and visualization
  "cluster",        # Clustering algorithms
  "factoextra",     # PCA and clustering visualization
  "ggplot2",        # Plotting
  "dendextend",     # Advanced dendrogram customization
  "NbClust",        # Determine the optimal number of clusters
  "psych",          # PCA and multivariate analysis
  "readr",          # Reading CSV files efficiently
  "conflicted",     # Handle function name conflicts explicitly
  "mice"            # Multivariate Imputation by Chained Equations
)

# Install any packages that are not already installed
new_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(new_packages)) {
  install.packages(new_packages, dependencies = TRUE)
}

# Load all libraries
lapply(required_packages, library, character.only = TRUE)

# Explicit conflict resolution
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

cat("✅ All required packages installed and loaded successfully.\n")
