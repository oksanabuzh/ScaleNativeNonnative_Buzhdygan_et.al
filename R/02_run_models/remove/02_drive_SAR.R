# Purpose: Drive species area relationships (slopes and intercepts)
# SAR relationship follows the power law: SR = c * A^z

# Load necessary libraries
library(tidyverse)

# Source power law function
source("R/zzz_functions/fit_power_law_SAR.R")

# Load data
species <- read_csv("data/database_analysis_summary.csv")

species <- species |> 
  pivot_longer(
   cols = c(total_species, native, non_native, invasive),
   names_to = "richness_type",
   values_to = "richness"
  )

coeff_SAR <- species |> 
  select(-subplot) |> 
  nest(data = c(richness, scale)) |> 
  rowwise() |> 
  # Fit the non-linear model to each series and type
  mutate(fit_power(data))

# Save results
coeff_SAR |> select(-data) |> 
  write_csv("data/coeff_SAR.csv")
