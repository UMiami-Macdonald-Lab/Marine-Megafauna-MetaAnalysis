# Vessel Impact Meta-Analysis

This repository contains the data, statistical models, and visualizations for a meta-analysis assessing the effects of vessels on marine megafauna. It includes response variables across behavior, physiology, vocalization, and abundance, and examines variation by effect type, taxonomic group, and conservation status.

---

## 📂 Repository Structure

| File | Description |
|------|-------------|
| `MA_Data_Submission.xlsx` | Raw dataset used for all analyses and visualizations. |
| `ANOVA_Histograms.R` | Generates histograms and exploratory plots to test ANOVA assumptions. |
| `Abundance.Models.R` | GLMs and analysis code for abundance-related responses to vessel effects. |
| `Behavior.Models.R` | Code for modeling behavioral response variables (e.g., avoidance, foraging shifts). |
| `Physiology.Models.R` | Code to analyze physiological stress responses (e.g., cortisol, heart rate). |
| `Figures.R` | Script to generate composite figures summarizing effect sizes across metrics. |
| `README.md` | Overview of the repository and instructions for use. |

---

## 📊 Project Summary

This project compiles effect sizes (log response ratios, or LRRs) from published literature examining how marine megafauna respond to vessel-related disturbances. It includes:

- **Behavioral** responses (e.g., movement, activity budgets)
- **Physiological** responses (e.g., stress hormones, metabolism)
- **Vocalization** responses (e.g., call amplitude/frequency shifts)
- **Abundance** changes associated with vessel activity

Each model quantifies average absolute LRRs and standard errors across:

- **Effect Types** (e.g., presence/absence, noise levels, distance, regulation)
- **Focal Megafauna Groups** (e.g., cetaceans, pinnipeds, fishes)
- **IUCN Conservation Status**

---

## 📈 Data Description

**File:** `MA_Data_Submission.xlsx`

This dataset contains the following key columns:

- `LRR`: Log Response Ratio (effect size)
- `EFFECT_CLEAN_JULIA`: Cleaned categorical variable representing vessel effect types
- `Focal.Megafauna.Group`: Group-level classification (e.g., 'Cetaceans', 'Fishes')
- `IUCN.Status`: Conservation status (e.g., 'Least Concern', 'Critically Endangered')
- `Year_paper`: Year of publication for each study
- `LATITUDE_CLEAN_MICHELLE`, `LONGITUDE_CLEAN_MICHELLE`: Coordinates for study site (used in mapping)

All plots and models draw from this master dataset.

---

## 🖥️ How to Use

1. Open RStudio and clone or download this repository.
2. Install required libraries:
   ```r
   install.packages(c("ggplot2", "dplyr", "ggpubr", "viridis", "sf", "readxl"))
