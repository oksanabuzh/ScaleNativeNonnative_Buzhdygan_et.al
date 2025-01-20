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