# Purpose: Test the correlation between different predictor variables 

# Load packages
library(tidyverse)

# Read the data --------------------------------------------------------------
header_data <- read_csv("data/header_data_prepared.csv")
climate_pc <- read_csv("data/Clima_PC.csv") |>
  dplyr::select(series, pca1_clima) |>
  unique()

disturbance_data <- read_csv("data-raw/Disturbn_commun_mean.csv") |>
  filter(subplot == "x") |>
  dplyr::select(-scale, -subplot) |>
  distinct()


header_data <- header_data |>
  left_join(climate_pc, by = "series") |>
  left_join(disturbance_data, by = "series")


# Correlate differen buffers around urban areas and cropland -----------------
header_data |>
  filter(subplot == "x") |>
  dplyr::select(starts_with("builtup"), starts_with("cropland")) |>
  cor() |>
  ggcorrplot::ggcorrplot(type = "lower", lab = TRUE)


# Correlate all variables for the model --------------------------------------
preds <- c(
  "altitude",
  "pca1_clima",
  "pH",
  "microrelief",
  "heat_index",
  "cover_litter",
  "cover_herbs_sum",
  "cover_gravel_stones",
  "builtup_1000m",
  "builtup_500m",
  "builtup_250m",
  "cropland_1000m",
  "cropland_500m",
  "cropland_250m",
  "grazing_intencity",
  "mowing",
  "abandonment",
  "roads",
  "Disturbance.Frequency",
  "Disturbance.Severity"
)


# Fig. S2. Pairwise Pearson correlations among predictor variables 
# measured at the 100-m2 plots and used in the analysis of proportions of alien,
# invasive, archaeophyte and neophyte species. 
header_data |>
  filter(subplot == "x") |>
  dplyr::select(all_of(preds)) |>
  rename(
    "Altitude" = "altitude",
    "Climate PC" = "pca1_clima",
    "Soil pH" = "pH",
    #   "Soil humus"= "Corg",
    "Microrelief" = "microrelief",
    "Gravel & stone cover" = "cover_gravel_stones",
    "Herb cover" = "cover_herbs_sum",
    #  "Shrub cover"= "cover_shrub_total",
    "Litter cover" = "cover_litter",
    "Grazing" = "grazing_intencity",
    "Mowing" = "mowing",
    "Abandonment" = "abandonment",
    "Heat stress" = "heat_index",
    "Urban built-up (1000 m)" = "builtup_1000m",
    "Croplands cover (1000 m)" = "cropland_1000m",
    "Urban built-up (250 m)" = "builtup_250m",
    "Croplands cover (250 m)" = "cropland_250m",
    "Urban built-up (500 m)" = "builtup_500m",
    "Croplands cover (500 m)" = "cropland_500m",
    "Road density" = "roads",
    "Disturbance frequency" = "Disturbance.Frequency",
    "Disturbance severity" = "Disturbance.Severity"
  ) %>%
  cor() |>
  ggcorrplot::ggcorrplot(
    type = "upper",
    lab = TRUE,
    lab_size = 3.5,
    hc.order = F,
    colors = c("red", "white", "blue")
  )
