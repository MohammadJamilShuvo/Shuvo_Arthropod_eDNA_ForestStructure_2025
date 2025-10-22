# Data and Analysis for “Influence of deadwood, tree-related microhabitats, and forest structural features on saproxylic arthropod community composition”

**Author:** Mohammad Jamil Shuvo  
**Affiliation:** Chair of Wildlife Ecology and Management, University of Freiburg, Germany  
**Contact:** jamilshuvo94@gmail.com  

The datasets generated and analyzed during this study are also available in the Figshare repository at https://doi.org/10.6084/m9.figshare.30421744

---

## Overview
This repository contains the complete dataset and R scripts used for the study submitted to *Ecology and Evolution* (2025). The research investigates how forest composition, deadwood composition, and tree-related microhabitats (TreMs) influence the diversity and distribution of saproxylic arthropods in the Black Forest, Germany. Using environmental DNA (eDNA) metabarcoding, arthropod communities were assessed across 135 forest plots with varying degrees of retention. The analyses reveal that specific TreMs, such as cavities, mosses, and insect galleries, play an essential role in supporting arthropod richness, particularly for Collembola, Coleoptera, and Arachnida, while canopy closure, deadwood volume, and snag availability are key determinants of community composition. The results highlight that species richness and community composition respond to different ecological drivers and emphasize the importance of preserving deadwood and TreMs to maintain structural heterogeneity and promote arthropod biodiversity.

---

## Files Included

| File | Description |
|------|--------------|
| `Metadata_SEM.xlsx` | Forest structure, environmental, and arthropod diversity metadata for 135 plots. |
| `OTUs_info_97.xlsx` | Processed eDNA OTU dataset (97% similarity clustering). |
| `richness_df.xlsx` | Arthropod group-level richness data for heatmap visualization. |
| `SCRIPT.R` | Complete R master script for correlation analysis, GLM/GAM, Random Forest, SEM, and heatmap. |
| `SEM_fig.png` | Final structural equation model (SEM) visualization figure. |

---

## How to Run the Analysis

1. Open R or RStudio.  
2. Set your working directory to the folder containing these files:
   ```r
   setwd("path/to/YourProjectFolder")
   ```
3. Run the script:
   ```r
   source("SCRIPT.R")
   ```
4. All analyses will run sequentially and generate plots and model summaries in your R session.

---

## Software and Packages

- R version ≥ 4.3  
- Required R packages are automatically installed when the script is run:  
  `tidyverse`, `mgcv`, `lavaan`, `semPlot`, `corrplot`, `randomForest`, `performance`, `mvabund`, `MASS`, `pscl`, `car`, `reshape2`, `RColorBrewer`, `writexl`.

---

## License
This dataset and code are shared under the **Creative Commons Attribution 4.0 International (CC BY 4.0)** license.  
You may reuse, adapt, or build upon this material provided appropriate credit is given to the original authors.

---

## Citation
If using these data or scripts, please cite as:

> Shuvo, M. J., Wohlwend, M., Heer, K., Paillet, Y., & Segelbacher, G. (2025).  
> *Influence of deadwood, tree-related microhabitats, and forest structural features on saproxylic arthropod community composition.*  
> *Ecology and Evolution.* DOI: (to be updated upon publication)

---

## Contact
For any questions or collaboration inquiries:  
**Mohammad Jamil Shuvo**  
University of Freiburg, Germany  
📧 jamilshuvo94@gmail.com
