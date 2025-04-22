# Vessel Impact Meta-Analysis

This repository contains the data, models, and figures for a meta-analysis of the effects of vessel presence, noise, and regulation on marine megafauna. The project synthesizes behavioral, physiological, vocal, and abundance responses using effect sizes (log response ratios, LRRs) extracted from the literature.

---

## 📂 Repository Structure

| File | Description |
|------|-------------|
| `MA_Data_Submission.xlsx` | Master dataset containing study-level extracted effect sizes and metadata. |
| `ANOVA_Histograms.R` | Code for plotting histograms and diagnostic plots to assess ANOVA assumptions. |
| `Abundance.Models.R` | Statistical models for abundance responses. |
| `Behavior.Models.R` | GLMs and effect size models for behavioral responses. |
| `Physiology.Models.R` | Code for modeling physiological metrics (e.g., stress responses). |
| `Figures.R` | Script for generating composite summary figures across metrics. |
| `README.md` | Repository overview and usage instructions. |

---

## 📊 Project Summary

This analysis compiles and synthesizes data on how marine megafauna respond to anthropogenic vessel disturbance. It evaluates variation in effect size magnitude across:
- **Effect Types** (e.g., noise levels, presence/absence, distance, regulation)
- **Response Categories** (Behavior, Physiology, Vocalization, Abundance)
- **Taxonomic Groups** (e.g., Cetaceans, Fishes, Pinnipeds)
- **IUCN Status** (e.g., Least Concern, Critically Endangered)

All responses are standardized using the absolute log response ratio (|LRR|) to allow cross-study comparison.

---

## 📈 Data Description

**File:** `MA_Data_Submission.xlsx`  
This file includes one row per study/treatment comparison. Key columns:

| Column | Description |
|--------|-------------|
| `Reference`, `Authors`, `Year`, `Journal`, `Link` | Study citation metadata |
| `Ocean`, `Country`, `Body.Water`, `Specific.Location`, `Latitude`, `Longitude` | Geographic metadata |
| `Focal.MF.Group`, `Focal.Species`, `IUCN.Status` | Biological grouping and conservation status |
| `Effect.Category`, `Effect.Clean` | Vessel-related experimental treatment types |
| `Response.Category`, `Response.Units.`, `Response.Clean` | Response variable type and standardization |
| `Response.Mean.Vessels.Treatment`, `Response.SD.Vessels.Treatment`, `Response.N.Vessels.Treatment` | Treatment group summary statistics |
| `Response.Mean.No.Vessels.Control`, `Response.SD.No.Vessels.Control`, `Response.N.No.Vessels.Control` | Control group summary statistics |
| `LRR` | Log Response Ratio (effect size) |
| `Vessel.Type.Broad`, `Vessel.Type.Specific` | Description of the vessel disturbance type |
| `Collector`, `Entry.Date`, `Data.Source`, `Comments` | Data entry metadata |

---

## 🖥️ How to Use

1. Clone or download this repository.
2. Open any script in R or RStudio.
3. Install required packages:
   ```r
   install.packages(c("dplyr", "ggplot2", "ggpubr", "viridis", "sf", "readxl"))

---
If you have any questions feel free to reach out to Julia Saltzman, JRS395@miami.edu 

If using this repository or dataset in your work, please cite:

Saltzman, J. et al. (in prep). Meta-analysis of Vessel Impacts on Marine Megafauna.
