# Drivers of plant invasions and their scale-dependency in grasslands

#### Code and data for

#### Buzhdygan et al. (2025) Drivers of plant invasions and their scale-dependency in grasslands. Biological Conservation


Cite the code and data: [![DOI]()

<!-- badges: start -->
<!-- badges: end -->

### Abstract
Conservation and management policies for plant invasions often rely on coarse-scale data, while plant diversity effects on ecosystem functions and services are primarily driven by species interactions at small spatial scales. Yet, most evidence on invasion drivers at fine scales is limited to a single grain size, leaving uncertainty about their scale-dependency. Understanding such scale-dependency is essential for predicting and managing invasions effectively.
We sampled plant communities across grassland habitats in Ukraine to assess how native species richness, environmental factors, and anthropogenic disturbances influence community invasion level – the proportions of all alien species, and separately for invasive species (fast-spreading aliens at advanced stages of invasion), archaeophytes (introduced before AD 1500) and neophytes (post-1500 AD aliens). By analysing these groups across six fine-grain areas (0.001‒100 m2), we tested for scale-dependent effects.
Native species richness was the strongest driver of invasions, showing negative effects that weakened with increasing scale. Alien species were dominated by archaeophytes and occurred most in dry grasslands, and least in fringe, alpine, and mesic types, driven by climatic and disturbance gradients. A range of abiotic and anthropogenic drivers, including precipitation, temperature, disturbance, land use and urbanization also influenced invasion levels, but their importance varied with scale. Notably, the scale-dependency of invasion drivers differed among archaeophytes, neophytes, and invasive species. 
Our results highlight the importance of separating alien groups and considering multiple spatial grains to avoid overlooking key drivers of invasion. Focusing on scale- and group-specific factors can enhance the ecological relevance and efficiency of conservation and management strategies targeting plant invasions.


## Project Structure

### R Scripts

The project contains several R script folders that process and analyze the data:

#### 1. Data Preparation (`R/01_prepare_data/`)

The following scripts are used for preparing the raw data for analysis. The
prepared data sets are already included in the project, so preparation does not
need to be repeated.:

- `00_extract_land_cover.R`: Extracts land cover data from Copernicus raster files (100m resolution) and calculates mean built-up and cropland cover fractions in buffers around sampling points (250-2000m).
- `01_prepare_species_data.R`: Prepares vegetation survey data by filtering vascular plants, calculating mean values for multi-layer species, and adding naturalization status
- `02_prepare_matrix.R`: Creates species composition matrices for analysis
- `03_spatial_indices.R`: Extract road density data and add it to the header data
- `04_prepare_header_data.R`: Prepare the header data with environmental variables and combine with the land cover data from `00_extract_land_cover.R`


#### 2. Statistical Analysis (`R/02_run_models/`)

- `01_run_all_binomial_models.R`: Runs binomial models for all scales using presence/absence data using the functions from `zzz_functions/run_single_model.R`
- `02_driver_scale_dependency.R`: Analyses scale dependency of different drivers
- `03_residual_spatial_correlation.R`: Tests for spatial autocorrelation in model residuals

#### 3. Visualization (`R/03_make_plots/`)
- Scripts numbered 01-05 creating various plots and figures for the publication

#### 4. Ordination Analysis (`R/04_ordination/`)
- `01_alien_sp_composition_100m2.R`: NMDS analysis of alien species composition
- `02_alien_Trait_Composition_100m2.R`: Analysis of trait composition

#### 5. Summary Statistics (`R/05_summary_stats/`)
- Scripts calculating various summary statistics and tables

### Data Files

#### Raw Data (`data-raw/`)
- `database_analysis.csv`: Raw vegetation survey data
- `non_native_species.csv`: List of non-native species and their categorization
- `database_analysis_categorized.csv`: Categorized species data
- `headers.csv`: Environmental variables for all plots
- `Disturbn_commun_mean.csv`: Disturbance data
- Additional spatial data in `spatial/` subfolder

#### Processed Data (`data/`)
- `database_analysis_summary.csv`: Summary of species data at plot level
- `alien_dataset_all.csv`: Combined dataset with all alien species information
- `header_data_prepared.csv`: Processed environmental variables
- Various matrices used in analyses (e.g., `invasive_matrix.csv`, `non-native_matrix.csv`)

### Results (`results/`)
Contains output tables and figures from analyses:
- Model results summaries
- Statistical test results
- Generated figures

### Other Files
- `.gitignore`: Git ignore rules
- `scale_native_non-native.Rproj`: R Project file

## Usage

Scripts should be run in numerical order within each folder, starting with data preparation (01_prepare_data).

## Requirements

R packages required:
- tidyverse
- vegan
- lme4
- ggplot2
- And others (complete list TBD)