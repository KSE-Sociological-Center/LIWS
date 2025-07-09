# Reproducible Analysis for Academic Publications

This repository contains scripts and data processing workflows used in the quantitative analysis for a series of academic publications. The aim is to ensure transparency, reproducibility, and ease of access for reviewers, collaborators, and readers interested in replicating or building on the findings.

## 📁 Folder Structure

Each subfolder or script in this repository corresponds to a specific publication or working paper. All code is written in R and organized to follow a reproducible pipeline.

## 📚 Publications and Corresponding Scripts

| Publication Title | Folder/Script | Description |
|-------------------|---------------|-------------|
| [*Does Conflict Create or Ruin Bonds Within Society? Depends on the definition* (2025)](https://voxukraine.org/chy-stvoryuye-vijna-sotsialni-zv-yazky-chy-rujnuye-yih-zalezhyt-vid-vyznachennya?fbclid=IwY2xjawK81uxleHRuA2FlbQIxMQBicmlkETFKMURvYWFxVHhBcjNiS0kxAR5WzSMJhPu92oRRe34zSNshM0dOj2graWCY0U9Aig00-eCpeiOz0ZT37O0dTw_aem_U0J219WPhCDuXg9lAW54qQ) | `social_trust_vox_column.R` | Code for replicating multilevel regression models using panel survey data from KIIS (Wave 1 & 2). Includes construction of war exposure indicators and modeling their interaction with time. |
| [*Existential upheavals: Tracing war’s immediate effect on individual religiosity in Ukraine* (2025)] (https://doi.org/10.1177/00377686241311421) | `War’s effect on religiosity_Social Compass.R` | Code for replicating the data analysis presented in the article using panel survey data from both Waves 1 and 2. Includes construction of key variables (war exposure index, Religiosity Index, change scores for religiosity indicators and the Religiosity Index, previous religiosity) as well as the script for Figures 2-5. |
