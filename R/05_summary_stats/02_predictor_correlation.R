# Purpose: Test the correlation between different variables in the header data

# Load packages
library(tidyverse)

# Read the data --------------------------------------------------------------
header_data <- read_csv("data/header_data_prepared.csv")
climate_pc <- read_csv("data/Clima_PC.csv") |>
  select(series, pca1_clima) |>
  unique()

disturbance_data <- read_csv("data-raw/Disturbn_commun_mean.csv") |>
  filter(subplot == "x") |>
  select(-scale, -subplot) |>
  distinct()

header_data <- header_data |>
  left_join(climate_pc, by = "series") |>
  left_join(disturbance_data, by = "series")

# Correlate differen buffers around urban areas and cropland -----------------
header_data |>
  filter(subplot == "x") |>
  select(starts_with("builtup"), starts_with("cropland"), built_up_2km, build_up_5km) |>
  cor() |>
  ggcorrplot::ggcorrplot(type = "lower", lab = TRUE)

# Correlate all variables for the model --------------------------------------
preds <- c(
  "pca1_clima", "pH", "microrelief", "heat_index",
  "cover_litter", "cover_herbs_sum", "cover_gravel_stones",
  "builtup_250m", "cropland_250m", "builtup_500m", "cropland_500m", "grazing_intencity", "mowing", "abandonment", "roads",
  "Disturbance.Frequency", "Disturbance.Severity"
)

header_data |>
  filter(subplot == "x") |>
  select(all_of(preds)) |>
  cor() |>
  ggcorrplot::ggcorrplot(type = "lower", lab = TRUE)
