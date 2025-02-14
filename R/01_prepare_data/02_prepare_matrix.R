# Purpose: Prepare a matrix for all plots with cover (10,100) and all invasive
# species. This matrix will be used for NMDS analysis. Also prepare the trait
# matrix.

# Load libraries
library(tidyverse)

# Load data -----------------------------------------------------------------

# Vegetation survey already categorized by naturalization level
species <- read_csv("data-raw/database_analysis_categorized.csv")
traits <- read_csv("data-raw/non_native_species.csv")

# Prepare matrix of invasive species ---------------------------------------
invasive_matrix <- species |>
  # filter(naturalisation_level == "invasive") |>
  filter(!is.na(naturalisation_level)) |>
  filter(response == "cover") |>
  unite(col = "plot_name", series, subplot, scale, sep = "-") |>
  select(species, plot_name, value) |>
  pivot_wider(names_from = species, values_from = value, values_fill = 0)

# Prepare trait matrix -----------------------------------------------------
trait_matrix <- traits |>
  select(taxon_tpl, introduction_time, origin, naturalisation_level,
    naturalisation_category)

# Save matrix ---------------------------------------------------------------
invasive_matrix |> write_csv("data/non-native_matrix.csv")
trait_matrix |> write_csv("data/non-native_trait_matrix.csv")
