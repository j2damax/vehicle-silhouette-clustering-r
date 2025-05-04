# 00_setup.R
# This script installs and loads all necessary libraries for the clustering analysis project

required_packages <- c(
  "tidyverse",      # Data manipulation and visualization
  "cluster",        # Clustering algorithms
  "factoextra",     # PCA and clustering visualization
  "ggplot2",        # Plotting
  "dendextend",     # Advanced dendrogram customization
  "NbClust",        # Determine the optimal number of clusters
  "psych",          # PCA and multivariate analysis
  "readr"           # Reading CSV files efficiently
)

# Install any packages that are not already installed
new_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(new_packages)) {
  install.packages(new_packages, dependencies = TRUE)
}

cat("✅ All required packages installed and loaded successfully.\n")
