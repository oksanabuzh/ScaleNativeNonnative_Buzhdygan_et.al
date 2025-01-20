# Purpose: Check correlations between our predcitors and see how many plots we
# have each predictor.
library(tidyverse)

# Prepare data --- -----------------------------------------------------------

climate_pc <- read_csv("data/Clima_PC.csv") |>
  select(series, pca1_clima) |>
  unique()
header_data <- read_csv("data/header_data_prepared.csv")

predictors <- header_data |> left_join(climate_pc, by = "series")

# Select only the predictors of interest

target_predictors <- c(
  "pca1_clima","economic_use", "zonality", "lon", "lat", "altitude", "BIO1",
  "BIO12", "pca1_clima", "BIO7", "BIO15", "pH", "depth_mean", "cover_herbs",
  "cover_herbs_sum", "inclination", "heat_index", "cover_litter", "grazing",
  "grazing_intencity", "mowing", "burning", "abandonment",
  "build_up_5km", "built_up_2km", "deph_SD", "microrelief",
  "cover_stones", "cover_gravel", "cover_soil", "cover_shrub_total",
  "sand", "silt", "clay", "conductivity", "Corg", "roads", "footprint_values"
)
predictors <- predictors |> select(series, subplot, all_of(target_predictors))

# Check correlations --------------------------------------------------------

correlations <- predictors |> select((where(is.numeric))) |> 
  cor(use="pairwise.complete.obs")

# plot correlations
corrplot <- ggcorrplot::ggcorrplot(
  correlations,
  title = "Driver correlations",
  hc.order = TRUE,
  type = "lower", outline.color = "white",
  ggtheme = ggplot2::theme_bw,
  tl.cex = 8,
  #lab = TRUE,
  colors = c("#6D9EC1", "white", "#E46726")
)
ggsave("img/corrplot_drivers.png", corrplot, width = 13, height = 13, 
       units = "cm", scale = 1.3)

# Table of how many plots (series we have for each predictor)
predictors |> 
  select(-subplot) |> 
  unique() |> 
  # calculate mean predictor if the predictor is numeric
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
            .by = series) |> 
  pivot_longer(-series, names_to = "predictor", values_to = "value") |> 
  filter(!is.na(value)) |>
  count(predictor) |> 
  arrange(-n) |> 
  knitr::kable(format = "markdown") |> 
  clipr::write_clip()
  