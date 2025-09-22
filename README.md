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


# Project Structure

This project is structured as follows:

```md
.gitignore
.Rproj.user/
data/
    database_analysis_summary.csv
    invasive_matrix.csv
    osm_plots.RData
data-raw/
    database_analysis_categorized.csv
    database_analysis.csv
    headers.csv
    non_native_species.csv
img/
R/
    01_prepare_data/
    osm_geographical_indices.R
scale_native_non-native.Rproj
```

## R Files

### Folder `01_prepare_data`

In this folder, the raw data is prepared for analysis:

- `01_prepare_data.R`: This script prepares the data for analysis. It reads in the raw data, filters for vascular plants, and calculates the mean value for species present in multiple layers. It also adds information on the 
naturalization levels of the species and summarized species richness and cover for each plot for
all species, native vs. non-native and invasive species.

- `02_prepare_matrix.R`: This script prepares a matrix for all plots with cover (10,100) and all invasive species. This matrix is used for NMDS analysis.

## Data files

### Folder `data-raw`

This folder contains the raw data files:

- `database_analysis.csv`: This is the raw data file used in the `01_prepare_data.R` script. It contains data from vegetation surveys.
- `non_native_species.csv`: This file contains a list of non-native species and their categorization, which is used in the `01_prepare_data.R` script to categorize species based on their naturalization level.
- `database_analysis_categorized.csv`: This file is created by the `01_prepare_data.R` script. It contains the categorized species data.
- `headers.csv`: This file contains environmental variables for all plots.

### Folder `data`

This folder contains the processed data files:

- `database_analysis_summary.csv`: This file is created by the `01_prepare_data.R` script. It contains a summary of the species data on the plot level.

- `invasive_matrix.csv`: This file is created by the `02_prepare_data.R` script. It contains a matrix of all plots with cover (10,100) and all invasive species.




## Other Files

- `.gitignore`: This file specifies intentionally untracked files that Git should ignore.
- `scale_native_non-native.Rproj`: This is the R project file for this project.