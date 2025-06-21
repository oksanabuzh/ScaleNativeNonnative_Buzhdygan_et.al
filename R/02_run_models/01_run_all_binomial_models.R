# Purpose: Run binomial models for all scales, using presence/absence data

# Load necessary libraries
library(tidyverse)

# Load the function
source("R/zzz_functions/run_single_model.R")

# Load and prepare the data ----------------------------------------------
# Read the species data and climate principal component data
species <- read_csv("data/database_analysis_summary.csv")
climate_pc <- read_csv("data/Clima_PC.csv") |>
  select(series, pca1_clima) |>
  unique()
header_data <- read_csv("data/header_data_prepared.csv")
# read disturbance data
disturbance_data <- read_csv("data-raw/Disturbn_commun_mean.csv")

# Select presence absence data for binomial models
species <- species |> filter(type == "p_a")

# Add additional columns to the species data
species <- species |> mutate(
  non_native_percent = non_native / total_species,
  invasive_percent = invasive / total_species,
  neophyte_percent = neophyte / total_species
)

# Join the species data with the climate principal component data
species <- left_join(species, climate_pc, by = "series", relationship = "many-to-many")

# Add header data to the species data
species <- species |> left_join(header_data, by = c("series", "subplot"))

# Add disturbance data to the species data
species <- species |> left_join(disturbance_data, by = c("series", "subplot", "scale"))

# Scale predictor variables ----------------------------------------------
# Predictor variables are on very different scales which causes problems in the
# models. Therefore, we rescale some of the variables
species <- species |> mutate(
  grazing=log1p(grazing),
  cover_gravel_stones = cover_gravel_stones / 100,
  roads = roads / 100,
  builtup_1000m = log1p(builtup_1000m), # builtup_1000m / 100,
  builtup_250m = log1p(builtup_250m), #builtup_250m / 100,
  builtup_500m = log1p(builtup_500m), #builtup_500m / 100,
  cropland_1000m = log1p(cropland_500m), # cropland_500m / 100,
    cropland_250m = log1p(cropland_250m), # cropland_250m / 100,
  cropland_500m = log1p(cropland_500m), # cropland_500m / 100,
  cover_litter = cover_litter / 100,
  cover_herbs_sum = cover_herbs_sum / 100
)
names(species)
log1p(species$grazing)
# Prepare the data for the models -------------------------------------------

# Pivot the data to long format and select relevant variables
data_for_models <- species |>
  pivot_longer(
    cols = c(non_native_percent, invasive_percent, neophyte_percent),
    names_to = "response_var",
    values_to = "response_value"
  ) |>
  relocate(c(response_var, response_value), .after = scale) |>
  # Split the data by scale, type, and response variable
  nest(.by = c(scale, type, response_var))

# Make table of model predictors -------------------------------------------

# Define the target predictors for the models

# Model 1
predictors_model_1 <- c(
  "pca1_clima", "pH", "microrelief", "heat_index",
  "cover_litter", "cover_herbs_sum", "cover_gravel_stones",
  "builtup_1000m", "cropland_1000m", "grazing_intencity", "mowing", "abandonment"
)

# Model 2
predictors_model_2 <- c("roads", "Disturbance.Frequency", "Disturbance.Severity")

# Model 3
predictors_model_3 <- c("native")

# Model 4
predictors_model4 <- c("pca1_clima", "native")

# Model 5 (revision): Add other buffer radius options
# urban and cropland buffer 250 m
predictors_model_5 <- c(
  "pca1_clima", "pH", "microrelief", "heat_index",
  "cover_litter", "cover_herbs_sum", "cover_gravel_stones",
  "builtup_250m", "cropland_250m",  
  "grazing_intencity", "mowing", "abandonment"
)

# urban and cropland buffer 500 m
predictors_model_6 <- c(
  "pca1_clima", "pH", "microrelief", "heat_index",
  "cover_litter", "cover_herbs_sum", "cover_gravel_stones",
  "builtup_500m", "cropland_500m",  
  "grazing_intencity", "mowing", "abandonment"
)

# Check which target predictors are not in the data
predictors_model_1[!predictors_model_1 %in% colnames(species)]
predictors_model_2[!predictors_model_2 %in% colnames(species)]
predictors_model_3[!predictors_model_3 %in% colnames(species)]
predictors_model4[!predictors_model4 %in% colnames(species)]
predictors_model_5[!predictors_model_5 %in% colnames(species)]
predictors_model_6[!predictors_model_6 %in% colnames(species)]

# Add the predictors and a model_id as a new column to the table
# "climate" represents model one "disturbance" represents model 2
# "native" represents model 3 and "climate_native" represents model 4
# Remove zeroes is a column that states if zero values in the response should be
# removed from the data or not
data_for_models <- expand_grid(
  model_id = c("climate", "disturbance", "native", "climate_native", "builtup_250m", "builtup_500m"),
  remove_zeroes = c(TRUE, FALSE),
  data_for_models
)

# we only want to remove zeroes for native and climate_native models
data_for_models <- data_for_models |>
  filter(
    !(model_id %in% c("climate", "disturbance", "builtup_250m", "builtup_500m") & remove_zeroes)
  )

# Remove the zeroes from the response_value column for those models where the
# remove_zeroes column is TRUE, else leave all response values inside
data_for_models <- data_for_models |>
  mutate(
    data = map2(data, remove_zeroes, ~ if (.y) filter(.x, response_value != 0) else .x)
  )

# Add the predictors to the table
data_for_models <- data_for_models |>
  mutate(
    predictors = case_when(
      model_id == "climate" ~ list(predictors_model_1),
      model_id == "disturbance" ~ list(predictors_model_2),
      model_id == "native" ~ list(predictors_model_3),
      model_id == "climate_native" ~ list(predictors_model4),
      model_id == "builtup_250m" ~ list(predictors_model_5),
      model_id == "builtup_500m" ~ list(predictors_model_6)
    )
  )

# Add correct random effect as a column (series for all except 100m²)
data_for_models <- data_for_models |>
  mutate(
    random_variable = case_when(
      scale == 100 ~ "dataset",
      .default = "series"
    )
  )

# Add a column that states if optimizers should be used or not
data_for_models <- data_for_models |>
  mutate(
    use_optimizers = case_when(
      response_var == "non_native_percent" ~ FALSE,
      response_var == "invasive_percent" ~ TRUE,
      response_var == "neophyte_percent" ~ TRUE
    )
  )
# Run the model for each group ------------------------------------
model_results <- data_for_models |>
  rowwise() |>
  mutate(
    run_single_model(
      data_to_model = data,
      response_variable = "response_value",
      random_effect = random_variable,
      fixed_effects = predictors,
      use_optim = use_optimizers
    )
  ) |>
  ungroup()

# Run one quasibinomial model instead of binomial -------------------------
# For the model with
# scale = 0.001
# model_id = "climate"
# response_var = "non_native_percent"
# The binomial model does not converge but it is not caught by performance::check_convergence
# Therefore we replace this single model by hand with a quasibinomial model

# first, we select the model to run again with quasi
model_quasi <- model_results |> filter(
  scale == 0.001 &
  model_id == "climate" &
  response_var == "non_native_percent"
) |> 
  select(all_of(names(data_for_models)))

model_quasi_result <- model_quasi |> 
  rowwise() |> 
  mutate(
  run_single_model_quasi(
    data_to_model = data,
    response_variable = "response_value",
    random_effect = random_variable,
    fixed_effects = predictors
  )
)

# remove the binomial model and add instead the new quasibinomial model
model_results <- model_results |> 
  filter(
    !(scale == 0.001 &
      model_id == "climate" &
      response_var == "non_native_percent")
  ) |> 
  bind_rows(model_quasi_result)

# check the status of the model runs
count(model_results, status)

# Summarize model results
model_results_summary <- model_results |> select(
  model_id, scale, response_var, remove_zeroes, model_res, status, model_converged, model_used
)
# Add the tibble from the model_res list column to the tibble, by duplicating the rows
model_results_summary <- model_results_summary |>
  unnest(model_res)

# Save the model results as R data file and CSV file
saveRDS(model_results, "data/model_results.rds")
write_csv(model_results_summary, "data/model_results_summary_OB.csv")
