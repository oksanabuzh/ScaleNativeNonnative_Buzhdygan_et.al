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
  invasive_percent = invasive / total_species
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
  cover_gravel_stones = cover_gravel_stones / 100,
  roads = roads / 100,
  built_up_2km = built_up_2km / 100,
  cover_litter = cover_litter / 100,
  cover_herbs_sum = cover_herbs_sum / 100
)

# Prepare the data for the models -------------------------------------------

# Pivot the data to long format and select relevant variables
data_for_models <- species |>
  pivot_longer(
    cols = c(non_native_percent, invasive_percent),
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
  "built_up_2km", "grazing_intencity", "mowing", "abandonment"
)

# Model 2
predictors_model_2 <- c("roads", "Disturbance.Frequency", "Disturbance.Severity")

# Check which target predictors are not in the data
predictors_model_1[!predictors_model_1 %in% colnames(species)]
predictors_model_2[!predictors_model_2 %in% colnames(species)]

# Add the predictors and a model_id as a new column to the table
# "climate" represents model one "disturbance" represents model 2
data_for_models <- expand_grid(
  model_id = c("climate", "disturbance"),
  data_for_models
)

# Add the predictors to the table
data_for_models <- data_for_models |>
  mutate(
    predictors = case_when(
      model_id == "climate" ~ list(predictors_model_1),
      model_id == "disturbance" ~ list(predictors_model_2)
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
      response_var == "invasive_percent" ~ TRUE
    )
  )

# Run the model for each group ------------------------------------
model_results <- data_for_models |>
  rowwise() |>
  mutate(
    run_single_model(
      data = data,
      response_variable = "response_value",
      random_effect = random_variable,
      fixed_effects = predictors,
      use_optim = use_optimizers
    )
  ) |>
  ungroup()

# Summarize model results
model_results_summary <- model_results |> select(
  scale, response_var, model_res, status, model_converged
)
# Add the tibble from the model_res list column to the tibble, by duplicating the rows
model_results_summary <- model_results_summary |>
  unnest(model_res)

# Save the model results as R data file and CSV file
# saveRDS(model_results, "data/model_results.rds")
write_csv(model_results_summary, "data/model_results_summary_OB.csv")
