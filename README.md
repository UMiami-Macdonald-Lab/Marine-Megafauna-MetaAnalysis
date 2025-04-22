# SaltzmanMetaAnalysis
# Vessel Impact Meta-Analysis

This repository contains code and visualizations for a meta-analysis of the effects of vessel presence and noise on marine megafauna behavior, physiology, vocalization, and abundance.

## 📂 Repository Structure

| File | Description |
|------|-------------|
| `ANOVA_Histograms.R` | Generates histograms and exploratory plots for ANOVA assumptions and visual data checks. |
| `Abundance.Models.R` | Statistical models and analyses for abundance-based response variables. |
| `Behavior.Models.R` | Generalized linear models (GLMs) and supporting code for behavior-related effects. |
| `Physiology.Models.R` | Code for modeling physiological responses to vessel effects. |
| `Figures.R` | Script for generating summary figures across all response types (behavior, physiology, vocalization, abundance). |
| `README.md` | Project overview and documentation. |

## 📊 Summary

This project compiles published effect sizes (log response ratios, or LRRs) of marine megafauna responses to anthropogenic vessel disturbance. Key response types include:
- **Behavioral** changes (e.g., displacement, altered foraging)
- **Physiological** stress (e.g., cortisol, heart rate)
- **Vocalization** adjustments (e.g., amplitude, frequency)
- **Abundance** shifts across habitats or exposure gradients

Each model script focuses on one of these response types, using fixed and mixed effects models to identify patterns based on:
- **Effect type** (e.g., noise levels, distance, regulations)
- **Taxonomic group** (e.g., cetaceans, pinnipeds, fishes)
- **IUCN status**

## 🖼️ Figures

The `Figures.R` script generates high-resolution composite figures summarizing average absolute LRRs and standard errors across grouping variables. Outputs are designed for use in manuscripts, presentations, or conservation policy briefs.

## 🧪 Requirements

All scripts are written in R. Required packages include:
- `ggplot2`
- `dplyr`
- `ggpubr`
- `viridis`
- `sf` (for mapping)
- `maps`

Use `install.packages("package_name")` in R to install any missing libraries.

## 📘 Citation

If using this codebase for your own work, please cite the repository and any relevant publications associated with the meta-analysis.

---

✉️ For questions or collaborations, contact Julia Saltzman at jrs395@miami.edu 
