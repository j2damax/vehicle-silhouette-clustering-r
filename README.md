# 🚗 Vehicle Silhouette Clustering (R)

This project is part of the **MSc Data Science** coursework under the module **NIB2001CEM – Data Mining**. The goal is to apply unsupervised machine learning techniques to group vehicles based on their silhouette-derived features using:

- Principal Component Analysis (PCA)
- K-Means Clustering
- Agglomerative Hierarchical Clustering

---

## 📂 Dataset

The dataset `vehicles.csv` consists of **846 samples** and **19 features**:

- **18 numerical features** extracted using the HIPS BINATTS system (including compactness, circularity, skewness, kurtosis, etc.).
- **1 categorical class label** identifying the vehicle type: *Double-decker bus*, *Chevrolet van*, *Opel Manta*, and *Saab*.

---

## 🧪 Project Pipeline

All R scripts are structured to reflect the logical data mining workflow:

| Script                          | Description                                                             |
|---------------------------------|-------------------------------------------------------------------------|
| `00_setup.R`                    | Install and load required packages.                                     |
| `01_data_preprocessing.R`       | Handle missing values (MICE), inspect duplicates, and generate summaries.|
| `02_outlier_treatment.R`        | Visualize and treat outliers using Winsorization and manual strategies. |
| `03_pca.R`                      | Perform PCA and visualize explained variance and biplots.               |
| `04_pca_component_selection.R`  | Analyze and manually select principal components for clustering.        |
| `05_kmeans_clustering.R`        | Perform K-Means clustering, Elbow method, Silhouette scoring.           |
| `06_hierarchical_clustering.R`  | Apply agglomerative clustering and analyze dendrograms.                 |
| `07_clustering_tuning.R`        | Compare cluster quality using different PC configurations.              |

---

## 📊 Techniques and Tools

- **Missing Value Imputation:** MICE with Predictive Mean Matching (PMM)
- **Outlier Treatment:** Winsorization (1%-99%) and median replacement
- **Feature Scaling:** Standardization prior to PCA
- **Dimensionality Reduction:** PCA with cumulative variance analysis
- **Clustering Evaluation:** Elbow Plot, Silhouette Score, Confusion Matrix
- **Visualization:** ggplot2, factoextra, ggrepel

---

## 📈 Outputs

All plots, metrics, and intermediate data are saved under:

- `outputs/tables/` — Cleaned datasets, PCA outputs, clustering summaries
- `outputs/figures/` — Visualizations for PCA, clustering, outliers

---

## 📦 Required R Packages

Install all required packages at once:

```r
install.packages(c(
  "tidyverse", "cluster", "factoextra", "ggplot2",
  "dendextend", "NbClust", "psych", "mice", "ggrepel", "gridExtra"
))
