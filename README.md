# Liver Health Statistical Analysis

Statistical analysis project exploring associations between demographic variables, liver biomarkers and liver health status.

## Overview

The analysis uses a dataset of 100 patients to investigate relationships between:

- age;
- sex;
- bilirubin;
- alkaline phosphatase (Alkphos);
- SGOT / AST;
- SGPT / ALT;
- total proteins;
- liver health status.

The project combines descriptive statistics, distributions, group comparisons and graphical exploration in R.

## Analysis workflow

```text
Patient dataset
      │
      ▼
Data import & exploration
      │
      ├── Descriptive statistics
      ├── Distribution analysis
      ├── Group comparisons
      └── Age / sex stratification
      │
      ▼
Visualisation & interpretation
```

## Technologies

- **R**
- **Base R** for statistical exploration and visualisation
- **ggplot2** for data visualisation

## Repository structure

```text
.
├── projet_stat_scripte_1.R
├── Projet_stat_2024 (1) (2).pdf
└── README.md
```

## Example analyses

The script includes exploration of biomarker distributions, summary statistics, histograms, boxplots and comparisons by sex, age group and liver-health status.

## Portfolio note

This is an academic statistics project and is intentionally positioned as a secondary portfolio project. It demonstrates R-based exploratory data analysis and statistical reasoning, complementing the bioinformatics projects in the profile.

The dataset used for the academic exercise should be treated as example/educational data and not as clinical evidence.

## Future improvements

- replace the original machine-specific data path with a project-relative path;
- organise the analysis into reproducible R scripts or an R Markdown/Quarto report;
- add explicit statistical-test assumptions and effect sizes;
- improve variable naming and remove exploratory code that is no longer used;
- add reproducible figures and a documented environment.

---

*Portfolio project — Statistics & Computational Biology.*
