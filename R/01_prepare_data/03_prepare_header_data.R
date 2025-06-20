# Purpose: Prepare the header data for the models
# Select only needed variables
# Make categorical variables ordinal where possible
# For 100 m2 we need to calculate the mean between the two corners

library(tidyverse)

header_data <- read_csv("data-raw/headers.csv")
# read header data with land cover buffers
header_data_landcover <- read_csv("data-raw/header_landcover_buffers.csv")

# Select only predictors of interest and info on series and subplots
# This gives a better overview of the data
# make a list of interesting predictor variables
header_vars <- c("series", "dataset", "subplot",
                 "habitat_group", "economic_use", "zonality", "lon", "lat", "altitude", "BIO1",
                 "BIO12", "BIO7", "BIO15", "pH", "depth_mean", "cover_herbs",
                 "cover_herbs_sum", "inclination", "heat_index", "cover_litter", "grazing",
                 "grazing_intencity", "mowing", "burning", "abandonment",
                 "build_up_5km", "built_up_2km", "deph_SD", "microrelief",
                 "cover_stones", "cover_gravel", "cover_soil", "cover_shrub_total",
                 "sand", "silt", "clay", "conductivity", "Corg", "roads", "footprint_values"
)

header_data <- header_data |>
  select(all_of(header_vars))

# Create a new variable that sums up gravel and stone cover
header_data <- header_data |>
  mutate(cover_gravel_stones = cover_gravel + cover_stones)

# Select relevant variables from the landcover header
header_data_landcover <- header_data_landcover |>
  select(series, dataset, subplot, builtup_250m, cropland_250m, builtup_500m, cropland_500m,
         builtup_1000m, cropland_1000m, builtup_2000m, cropland_2000m)

# Add landcover buffers to the header
header_data <- header_data |>
  left_join(header_data_landcover, by = c("series", "dataset", "subplot"))

# Header data: Contains data for the two corners. 
# But for the 100 m2 plot we need to calculate the mean for all numerical vars
header_data_mean <- header_data |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
            .by = c(dataset, series, habitat_group, zonality)) |>
  mutate(subplot = "x")

# Add the mean data to the header data
header_data <- bind_rows(header_data, header_data_mean) 

# Make categorical variables ordinal -----------------------------------------
header_data |> select(where(is.character))
header_data <- header_data |> 
  mutate(
    zonality = ifelse(zonality == "zonal", 1, 0)
  )

# Save the data
write_csv(header_data, "data/header_data_prepared.csv")
