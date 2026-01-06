# Avian usage of urban greenspaces throughout the full annual cycle and between migratory groups

This repository contains all code and derived data necessary to reproduce the analyses, figures, and tables for the manuscript:

> **“Avian usage of urban greenspaces throughout the full annual cycle and between migratory groups.”**

The project quantifies seasonal patterns of avian species richness across urban greenspaces and contrasts responses between migratory and resident species using eBird data, greenspace attributes, and human modification metrics.

---

## Getting started

The simplest way to reproduce the full workflow is to open the project’s `.Rproj` file and run the scripts in order.

**Recommended execution order:**
1. Run scripts **1–7** in the `R/` directory sequentially  
2. Run the scripts titled 'misc' last

All analyses were conducted in **R**, and required packages are loaded within individual scripts.

---

## Repository structure

### `R/`
Scripts for data processing, modeling, and figure/table generation.

- **Scripts 1–4**:  
  - Filter and process eBird data  
  - Spatially overlay observations with greenspaces  
  - Select urban greenspaces  
  - Derive greenspace attributes (area, isolation, and GHMI)

- **Scripts 5–7**:  
  - Fit species richness models  
  - Generate **Figures 2–5**  
  - Generate **Supplementary Tables S2A–S2E**

- **Miscellaneous scripts** (`R/misc/`):  
  - Create **Figure 1**  
  - Create **Supplementary Figure S2**  
  - Create **Supplementary Table S1**

⚠️ **Important notes**:
- Script **1** relies on raw data files that are too large to be hosted on GitHub. These files are available upon request (via email).  
- Users without access to these files can begin inspection and execution at **Script 2**.
- GHMI data extraction requires access to **Google Earth Engine**.

---

### `data/`
Input and intermediate datasets used throughout the workflow.

- **`AVONET/`**  
  - AVONET trait data  
  - RDS files with assigned migratory status

- **`eBird/`**  
  - Raw and processed eBird data  
  - Extracted eBird files used in analyses

- **`ghmi_dynamic_world/`**  
  - CSV containing mean GHMI values per park  
  - Used to append GHMI metrics to the final dataset

- **`intermediate_data/`**  
  - Saved intermediate objects used to reduce runtime  
  - Many long-running sections of scripts are commented out (`###`) and replaced by loading these files

- **`polygons/`**  
  - Shapefiles for South Florida study area and greenspaces

- **`final_data_for_big_script.RDS`**  
  - Final compiled dataset used for all models  
  - Loaded as `greenspaces` in modeling scripts

---

### `figures/`
Contains all figures and tables included in the manuscript.

- Files are saved as `.png` or `.pdf`
- Organized into subfolders by **figure or table number**
- Includes all main-text figures and supplementary materials

---

## Reproducibility notes

- Model fitting uses `glmmTMB` with effort covariates and seasonal interactions.
- Figures are generated using `ggplot2`, and marginal effects are summarized with `emmeans`.
- Due to data size and API requirements, full end-to-end reproduction requires:
  - Access to large eBird-derived datasets
  - Access to Google Earth Engine for GHMI extraction

---

## Contact

For access to large input datasets or questions about the workflow, please contact the repository author.
