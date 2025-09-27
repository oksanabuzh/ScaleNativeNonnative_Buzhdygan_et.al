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

### R Scripts (`R/`)

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

- `01_run_all_binomial_models.R`: Runs binomial GLMM models for all scales using presence/absence data using the functions from `zzz_functions/run_single_model.R`
- `02_driver_scale_dependency.R`: Analyses scale dependency of different drivers
- `03_Residual_Spatial_Correlation.R`: Tests for spatial autocorrelation in model residuals

#### 3. Visualization (`R/03_make_plots/`)
The following scripts create various plots and figures for the publication:
- `01_plot_standardized_effects.R`: Plots standardized effects of the predictors (Fig. 3) 
- `02_plot_scale_dependency_native_effcets.R`: Generates plots and statistical summaries that illustrate how the relationship between native and non-native species richness varies with spatial scale (Fig. 1 C,F,I,L)
- `03_plot_scale_dependency_drivers.R`: Plots the relationship of each driver slope with scale (Fig. 4), creates Table for Supplementary Data 2  
- `04_plot_native_alien.R`: Plots relationship between alien and native richness and cover (Fig. 2 A,B,D,E,J,K,G,H; Fig. S6; Fig. S7)
- `05_R2_heatmap.R`: Plots variance explained by each predictor (R2) as heatmap (Fig. S8)

#### 4. Ordination Analysis (`R/04_ordination/`)
- `01_alien_sp_composition_100m2.R`: NMDS analysis of alien species composition for 100 m2 plots. Produces Table S7 and Fig. S9.

- `02_alien_Trait_Composition_100m2.R`: NMDS analysis for trait composition of alien species. Produces Table S8 and Fig. S10.

#### 5. Summary Statistics (`R/05_summary_stats/`)
- Scripts calculating various summary statistics and tables
- `01_correlation_invasive_categories.R`: # Calculate relationships between different categories of alien species (Fig. S3)
- `02_predictor_correlation.R`: Tests the correlation between different predictor variables (Fig. S2) 
- `03_summary_statistics.R`: Summary statistics for the number of sampled plots, number of plots containing alien species (Table S3, Fig1 D,E,F), minimum, maximum, and mean values for total species richness, native species richness, and the percentage of aliens (Table S4) across spatial scales and habitat types.
- `04_species_occurances_by_scale.R`: Plots frequency of occurrence of individual alien species across spatial grain sizes (Fig S4) 
- `05_species_occurances_by_habitat.R`: Plots frequency of occurrence of individual alien species across grassland habitat types at the 100-m2 scale (Fig S5)
- `06_pairwise_habitat_differences_alien_marginal_means.R`: Pairwise comparisons among grassland types in marginal means of alien species proportions (Table S5, Fig.S11, Fig.S12).

### Data Files

#### Raw Data (`data-raw/`)
Contains raw data used in this study.

See [data-raw/README.md](data-raw) for details.

#### Processed Data (`data/`)
Contains data prepared and used in this study.

See [data/README.md](data) for details.

### Results (`results/`)
Contains output tables and figures from analyses:
- Model results summaries
- Statistical test results
- Generated figures

See [results/README.md](results) for details.

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