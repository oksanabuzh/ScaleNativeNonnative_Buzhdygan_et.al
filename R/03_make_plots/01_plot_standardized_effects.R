# Purpose: Plot standardized effects of the predictors on the response variables
# Input:
#   - data/model_results_summary.csv: Summary of model results across scales

# Load necessary libraries
library(tidyverse)

# -----------------------------------------------------------------------------#
# Load and Prepare Data
# -----------------------------------------------------------------------------#

results <- read_csv("data/model_results_summary.csv") |>
  filter(
    model_id %in%
      c("climate", "disturbance") |
      (model_id %in%
        c("builtup_250m", "builtup_500m") &
        predictor %in%
          c(
            "builtup_250m",
            "cropland_250m",
            "builtup_500m",
            "cropland_500m",
            "builtup_1000m",
            "cropland_1000m"
          ))
  )

write_csv(
  results |>
    filter(!scale == 0.0001) |>
    dplyr::select(-status, -r2_partial),
  "results/GLMM_results_Table_S5.csv"
)

# Add information indicating whether the effect is significant or not
results <- results |>
  mutate(
    significance = ifelse(
      p_value_chisq < 0.05,
      "significant",
      "not significant"
    )
  )

# Reorder Predictor variables based on the standardized effect of the small scale
variable_order <- results |>
  filter(response_var == "non_native_percent" & scale == 10) |>
  arrange(-std_slope) |>
  pull(predictor) |>
  unique()

# If any predictors are missing, add them to the variable order
missing_vars <- setdiff(results$predictor |> unique(), variable_order)
variable_order <- c(variable_order, missing_vars)

# Configure factor levels for response_var and predictor
results <- results |>
  mutate(
    response_var = factor(
      response_var,
      levels = c(
        "non_native_percent",
        "archaeophyte_percent",
        "neophyte_percent",
        "invasive_percent"
      )
    ),
    predictor = factor(predictor, levels = variable_order)
  )
# Rename and relevel factor variables for better readability in plots
results <- results |>
  mutate(
    variable_new = fct_recode(
      predictor,
      "Climate PC" = "pca1_clima",
      "Soil pH" = "pH",
      "Heat index" = "heat_index",
      "Microrelief" = "microrelief",
      "Gravel & stone cover" = "cover_gravel_stones",
      "Herb cover" = "cover_herbs_sum",
      "Litter cover" = "cover_litter",
      "Grazing" = "grazing_intencity",
      "Mowing" = "mowing",
      "Abandonment" = "abandonment",
      "Urban built-up (1000 m)" = "builtup_1000m",
      "Croplands cover (1000 m)" = "cropland_1000m",
      "Urban built-up (250 m)" = "builtup_250m",
      "Croplands cover (250 m)" = "cropland_250m",
      "Urban built-up (500 m)" = "builtup_500m",
      "Croplands cover (500 m)" = "cropland_500m",
      "Road density" = "roads",
      "Disturbance frequency" = "Disturbance.Frequency",
      "Disturbance severity" = "Disturbance.Severity"
    )
  ) |>
  mutate(
    variable_new = fct_relevel(
      variable_new,
      "Climate PC",
      "Soil pH",
      "Heat index",
      "Microrelief",
      "Gravel & stone cover",
      "Herb cover",
      "Litter cover",
      "Grazing",
      "Mowing",
      "Abandonment",
      "Road density",
      "Croplands cover (250 m)",
      "Croplands cover (500 m)",
      "Croplands cover (1000 m)",
      "Urban built-up (250 m)",
      "Urban built-up (500 m)",
      "Urban built-up (1000 m)",
      "Disturbance frequency",
      "Disturbance severity"
    )
  ) |>
  mutate(variable_new = fct_relevel(variable_new, rev)) |>
  mutate(
    response_var_new = case_when(
      response_var == "non_native_percent" ~ "Alien species, %",
      response_var == "archaeophyte_percent" ~ "Archaeophytes, %",
      response_var == "neophyte_percent" ~ "Neophytes, %",
      response_var == "invasive_percent" ~ "Invasive species, %"
    )
  ) |>
  mutate(response_var_new = factor(response_var_new)) |>
  mutate(
    response_var_new = fct_relevel(
      response_var_new,
      "Alien species, %",
      "Archaeophytes, %",
      "Neophytes, %",
      "Invasive species, %"
    )
  )

# Create the standardized effect plot ------------------------------------------
std_effect_plot <- results |>
  filter(scale == 100) |>
  ggplot(aes(
    x = std_slope,
    y = variable_new
  )) +
  geom_point(
    aes(
      color = ifelse(std_slope < 0, "blue", "red"),
      alpha = significance
    ),
    size = 4
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("red", "blue")) +
  facet_grid(~response_var_new) +
  scale_x_continuous(limits = c(-0.86, 0.55)) +
  scale_alpha_manual(values = c("significant" = 1, "not significant" = 0.4)) +
  labs(
    x = "Standardized effect of the driver",
    y = "Driver"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12, colour = "black"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 10, colour = "black"),
    axis.text.x = element_text(size = 9, colour = "black")
  )

std_effect_plot

#ggsave("img/std_effect_plot_Fig_3AB.png", std_effect_plot, width = 20, height = 20,
#       units = "cm")
