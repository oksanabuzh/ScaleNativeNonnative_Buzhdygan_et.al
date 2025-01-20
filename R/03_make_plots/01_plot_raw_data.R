# Purpose: Plot raw data against predictors
# This will show if there is a hump shape or a linear relationship

# Load necessary libraries
library(tidyverse)

# Load the data and prepare it ----------------------------------------------
species <- read_csv("data/database_analysis_summary.csv")
climate_pc <- read_csv("data/Clima_PC.csv") |>
  select(series, pca1_clima) |>
  unique()
header_data <- read_csv("data/header_data_prepared.csv")

# Select only presence absence data for the binomial models
species <- species |>
  filter(type == "p_a")

species <- species |>
  mutate(
    non_native_percent = non_native / total_species,
    invasive_percent = invasive / total_species,
    binary_non_native = ifelse(non_native > 0, 1, 0),
    binary_invasive = ifelse(invasive > 0, 1, 0)
  )

species <- left_join(species, climate_pc,
  by = "series",
  relationship = "many-to-many"
)

# Add header data to the species data
species <- species |>
  left_join(header_data, by = c("series", "subplot"))

# select predictors interesting for hump-shaped effect
names(species)
test_hump_shape <- c("pca1_clima", "pH", "cover_litter", "Corg")

to_plot <- species |>
  dplyr::select(scale, non_native_percent:binary_invasive, habitat_group, all_of(test_hump_shape)) |>
  pivot_longer(cols = all_of(test_hump_shape), names_to = "predictor", values_to = "predictor_value")

to_plot <- to_plot |> pivot_longer(cols = non_native_percent:binary_invasive, names_to = "response", values_to = "response_value")

to_plot |>
  filter(response %in% c("non_native_percent", "invasive_percent")) |>
  ggplot(aes(x = predictor_value, y = response_value, color = scale)) +
  geom_point() +
  facet_grid(response ~ predictor, scales = "free") +
  theme_classic()

