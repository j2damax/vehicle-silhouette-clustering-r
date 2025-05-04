# Vehicle Silhouette Clustering (R)

This project is part of the MSc Data Science coursework (Module: NIB2001CEM – Data Mining). It focuses on clustering different types of vehicles based on silhouette-derived features using unsupervised machine learning techniques, specifically:

- Principal Component Analysis (PCA)
- K-Means Clustering
- Agglomerative Hierarchical Clustering

## 📊 Dataset

The dataset `vehicles.csv` contains 846 samples of vehicles with 18 numerical features extracted from their silhouettes and 1 categorical label indicating the type of vehicle.

## 🧪 Project Structure

- `data/` — Contains the original dataset.
- `scripts/` — Step-by-step R scripts for preprocessing, dimensionality reduction, and clustering.
- `outputs/` — Plots and tables generated during the analysis.
- `README.md` — This file.

## 📈 Key Techniques Used

- Missing Value and Outlier Handling
- Data Normalization
- Principal Component Analysis (PCA)
- Elbow Method and Silhouette Score to identify optimal number of clusters
- K-Means Clustering
- Hierarchical Clustering with Dendrograms
- Feature Projection on Principal Components

## 📦 Dependencies

Make sure to install the following R packages:

```r
install.packages(c("tidyverse", "cluster", "factoextra", "ggplot2", "dendextend", "NbClust", "psych"))
