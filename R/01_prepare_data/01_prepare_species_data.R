# Purpose: Summarize vegetation data from our plots and calculate number of
# total species and alien/invasive species per plot and scale

# Load packages
library(tidyverse)


# Load individual data set ------------------------------------------------

# data from the vegetation surveys
# select only vascular plants and calculate mean value for species that are
# present in multiple layers
species <- read_csv("data-raw/database_analysis_tpl.csv") |> 
 # filter(group == "VP") |>
  mutate(species = str_to_sentence(species)) %>% 
  summarize(
    value = mean(value, na.rm = TRUE),
    .by = c(species, series, subplot, scale, response, value)
  ) |> 
  mutate(
    value = case_when(
      response == "p/a" & value > 1 ~ 1,
      .default = value
    )
  )

# classification of non-native species
non_native_species <- read_csv("data-raw/non_native_species.csv") |> 
  select(taxon_tpl, naturalisation_level, introduction_time)

# Add classification to vegetation data -----------------------------------

species <- species |> 
  left_join(non_native_species, by = c("species" = "taxon_tpl"),
            relationship = "many-to-one")

# Could all species from the non-native species list be matched? Yes
all(species |> filter(!is.na(naturalisation_level)) |> distinct(species) |> pull() %in% 
      non_native_species$taxon_tpl)

# Add general category native/non-native
species <- species |> 
  mutate(category = case_when(
    is.na(naturalisation_level) ~ "native",
    .default = "non_native"
  ))

# Save categorized species data
species |> write_csv("data-raw/database_analysis_categorized.csv")

# Summarize species data on the plot level --------------------------------

# Based on presence absence data -------------------------------------------

# total species
species_summary <- species |> 
  group_by(series, subplot, scale) |> 
  summarize(
    total_species = n_distinct(species),
    .groups = "drop"
  )

# Category native/non_native
category_summary <- species |> 
  group_by(series, subplot, scale, category) |> 
  summarize(
    total_species = n_distinct(species),
    .groups = "drop"
  ) |> 
  pivot_wider(names_from = category, values_from = total_species, 
              values_fill = 0)

# naturalisation level: invasive, naturalized, casual
invasive_summary <- species |> 
  filter(!is.na(naturalisation_level)) |> 
  group_by(series, subplot, scale, naturalisation_level) |> 
  summarize(
    total_species = n_distinct(species),
    .groups = "drop"
  ) |> 
  pivot_wider(names_from = naturalisation_level, values_from = total_species, 
              values_fill = 0) |> 
  # we are only interested in invasive species
  select(series, subplot, scale, invasive, naturalised, casual)

# Introduction time: neophyte, archeophyte
introduction_summary <- species |> 
  filter(!is.na(introduction_time)) |> 
  group_by(series, subplot, scale, introduction_time) |> 
  summarize(
    total_species = n_distinct(species),
    .groups = "drop"
  ) |> 
  pivot_wider(names_from = introduction_time, values_from = total_species, 
              values_fill = 0)

# Bring them together
summary_presence_absence <- species_summary |> 
  left_join(category_summary, by = c("series", "subplot", "scale")) |> 
  left_join(invasive_summary, by = c("series", "subplot", "scale")) |>
  left_join(introduction_summary, by = c("series", "subplot", "scale")) |>
  # replace all NAs with 0
  mutate(across(everything(), ~replace_na(., 0)))

# Based on cover data ------------------------------------------------------

species_summary_cover <- species |>
  filter(response == "cover") |>
  group_by(series, subplot, scale) |> 
  summarize(
    total_species = sum(value),
    .groups = "drop"
  )

# Category native/non_native
category_summary_cover <- species |> 
  filter(response == "cover") |>
  group_by(series, subplot, scale, category) |> 
  summarize(
    total_species_cover = sum(value),
    .groups = "drop"
  ) |> 
  pivot_wider(names_from = category, values_from = total_species_cover, 
              values_fill = 0)

# naturalisation level: invasive, naturalized, casual
invasive_summary_cover <- species |> 
  filter(response == "cover") |>
  filter(!is.na(naturalisation_level)) |> 
  group_by(series, subplot, scale, naturalisation_level) |> 
  summarize(
    total_species_cover = sum(value),
    .groups = "drop"
  ) |> 
  pivot_wider(names_from = naturalisation_level, values_from = total_species_cover, 
              values_fill = 0) |> 
  # we are only interested in invasive species
  select(series, subplot, scale, invasive, naturalised, casual)

# Introduction time: neophyte, archeophyte
introduction_summary_cover <- species |> 
  filter(response == "cover") |>
  filter(!is.na(introduction_time)) |> 
  group_by(series, subplot, scale, introduction_time) |> 
  summarize(
    total_species_cover = sum(value),
    .groups = "drop"
  ) |> 
  pivot_wider(names_from = introduction_time, values_from = total_species_cover, 
              values_fill = 0)

# Bring them together
summary_cover <- species_summary_cover |> 
  left_join(category_summary_cover, by = c("series", "subplot", "scale")) |> 
  left_join(invasive_summary_cover, by = c("series", "subplot", "scale")) |>
  left_join(introduction_summary_cover, by = c("series", "subplot", "scale")) |>
  # replace all NAs with 0
  mutate(across(everything(), ~replace_na(., 0)))

# Bring both datasets together
summary <- bind_rows(p_a = summary_presence_absence, cover = summary_cover,
                    .id = "type")

# Save summary data
summary |> 
  write_csv("data/database_analysis_summary.csv")

  

         