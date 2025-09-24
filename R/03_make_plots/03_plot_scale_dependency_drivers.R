# Purpose: Plot the correlation coefficient of each slope with scale for the
# different drivers

# Load necessary libraries
library(tidyverse)
library(jtools)

# Load the data and prepare it ----------------------------------------------
results <- read_csv("data/model_results_summary.csv") %>%
  filter(model_id == "builtup_500m" | model_id == "disturbance") %>%
  bind_rows(
    results <- read_csv("data/model_results_summary.csv") %>%
      filter(
        model_id %in%
          c("builtup_250m", "climate") &
          predictor %in%
            c(
              "builtup_1000m",
              "cropland_1000m",
              "builtup_250m",
              "cropland_250m"
            )
      )
  )
results %>%
  pull(response_var)

Suppl_dat2 <- results %>%
  filter(!scale == 0.0001) %>%
  filter(
    response_var %in%
      c("non_native_percent", "archaeophyte_percent") |
      response_var %in%
        c("invasive_percent", "neophyte_percent") &
        !scale %in% c(0.0001, 0.001, 0.01)
  ) %>%
  mutate(
    response_variable = case_when(
      response_var == "non_native_percent" ~ "alien",
      response_var == "archaeophyte_percent" ~ "archaeophyte",
      response_var == "neophyte_percent" ~ "neophyte",
      response_var == "invasive_percent" ~ "invasive"
    )
  ) %>%
  dplyr::select(
    scale,
    response_variable,
    predictor,
    slope,
    chisq,
    p_value_chisq,
    r2_partial,
    r2m,
    r2c
  ) %>%
  mutate(
    predictor = fct_recode(
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
    ),
    .before = predictor
  ) %>%
  mutate(
    predictor = fct_relevel(
      predictor,
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
      "Croplands cover (250 m)",
      "Urban built-up (250 m)",
      "Croplands cover (500 m)",
      "Urban built-up (500 m)",
      "Croplands cover (1000 m)",
      "Urban built-up (1000 m)",
      "Road density",
      "Disturbance frequency",
      "Disturbance severity"
    )
  ) %>%
  arrange(scale, response_variable, predictor)

write_csv(Suppl_dat2, "results/Supplementary_Data2.csv")

# to transform back slopes
results = results %>%
  mutate(
    slope = case_when(
      # predictor=="cover_gravel_stones" ~ slope * 100,
      predictor == "roads" ~ slope * 100,
      predictor == "builtup_1000m" ~ expm1(slope),
      predictor == "builtup_250m" ~ expm1(slope),
      predictor == "builtup_500m" ~ expm1(slope),
      predictor == "cropland_1000m" ~ expm1(slope),
      predictor == "cropland_250m" ~ expm1(slope),
      predictor == "cropland_500m" ~ expm1(slope),
      predictor == "cover_litter" ~ slope * 100,
      predictor == "cover_herbs_sum" ~ slope * 100,
      .default = slope
    )
  )

results


# Add information whether the models have just one predictor or whether
# they were run with climate or SR
results <- results |>
  mutate(
    significance = ifelse(
      p_value_chisq < 0.05,
      "significant",
      "not significant"
    )
  )

# filter(!is.na(std_slope))

# Subset only models with climate as a secondary predictor OR the model with
# only climate as predictor
#results <- results |>
#  filter(secondary_variable == "pca1_clima" | variable_of_interest == "pca1_clima"
#         | variable_of_interest == "altitude" # | variable_of_interest == "total_species"
#        )

# Reorder Predictor variables based on the standardized effect of the small scale
variable_order <- results |>
  filter(response_var == "non_native_percent" & scale == 100) |>
  arrange(-std_slope) |>
  pull(predictor) |>
  unique()

# did I miss any predictors?
missing_vars <- setdiff(results$predictor |> unique(), variable_order)
variable_order <- c(variable_order, missing_vars)

results <- results |>
  distinct() |>
  mutate(
    response_var = factor(
      response_var,
      levels = c(
        "non_native_percent",
        "invasive_percent",
        "neophyte_percent",
        "archaeophyte_percent"
      )
    ),
    predictor = factor(predictor, levels = variable_order)
  )


results <- results %>%
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
      #"Urban built-up"= "built_up_2km",
      "Urban built-up (1000 m)" = "builtup_1000m",
      "Croplands (1000 m)" = "cropland_1000m",
      "Urban built-up (250 m)" = "builtup_250m",
      "Croplands (250 m)" = "cropland_250m",
      "Urban built-up (500 m)" = "builtup_500m",
      "Croplands (500 m)" = "cropland_500m",
      "Road density" = "roads",
      "Disturbance frequency" = "Disturbance.Frequency",
      "Disturbance severity" = "Disturbance.Severity"
    )
  ) %>%
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
      "Croplands (250 m)",
      "Urban built-up (250 m)",
      "Croplands (500 m)",
      "Urban built-up (500 m)",
      "Croplands (1000 m)",
      "Urban built-up (1000 m)",

      "Disturbance frequency",
      "Disturbance severity"
    )
  ) %>%
  mutate(variable_new = fct_relevel(variable_new, rev)) %>%
  mutate(
    response_var_new = factor(case_when(
      response_var == "non_native_percent" ~ "Alien species, %",
      response_var == "invasive_percent" ~ "Invasive species, %",
      response_var == "neophyte_percent" ~ "Neophytes, %",
      response_var == "archaeophyte_percent" ~ "Archaeophytes, %"
    ))
  ) %>%
  filter(!variable_new == "Mowing")


# Plots of scale-dependency of driver effects -----

# Alien species ----
# Predicted effects with scale

model_type_alien <- read_csv("results/Model_selection_Table.csv") %>%
  filter(response_var == "non_native_percent") %>%
  # dplyr::select(variable_new, predictor, final_model) %>%
  mutate(
    my.formula = case_when(
      final_model == "m1" ~ "y ~ x",
      .default = "y ~ poly(x, 2)"
    )
  ) %>%
  mutate(
    p_value = case_when(final_model == "m1" ~ m1.p.value, TRUE ~ m2.p.value)
  ) %>%
  mutate(
    signif = case_when(
      p_value < 0.05 ~ "Sign",
      p_value >= 0.05 & p_value < 0.095 ~ "Marg",
      .default = "Nonsig"
    )
  ) %>%
  dplyr::select(
    variable_new,
    predictor,
    my.formula,
    final_model,
    signif,
    p_value
  )


P1 <- ggplot(
  results %>%
    filter(response_var == "non_native_percent") %>%
    filter(!scale == 0.0001),
  aes(log(scale), slope)
) +
  mapply(
    function(x, z, signif) {
      geom_smooth(
        method = "lm",
        data = function(d) subset(d, predictor == z),
        formula = x,
        fill = "forestgreen",
        col = "forestgreen",
        alpha = 0.07,
        linetype = ifelse(
          signif == "Sign" | signif == "Marg",
          "solid",
          "dashed"
        ),
        linewidth = case_when(
          signif == "Sign" ~ 1.1,
          signif == "Nonsig" ~ 0.5,
          .default = 0.4
        )
      )
    },
    model_type_alien$my.formula,
    model_type_alien$predictor,
    model_type_alien$signif
  ) +
  geom_point(col = "gray22", size = 2, alpha = 0.6) +
  labs(
    y = "Slope of the driver effect on proportion of alien species",
    x = expression(paste(
      'Grain size, ',
      m^{
        2
      }
    ))
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 7, colour = "black"),
    axis.text.y = element_text(size = 6, colour = "black"),
    axis.title = element_text(size = 13, colour = "black"),
    strip.background = element_rect(colour = "black", fill = "#EFF7EF")
  ) +
  facet_wrap(
    ~ reorder(variable_new, desc(variable_new)),
    scales = "free",
    ncol = 2
  ) +
  #   facet_wrap(~variable_new, scales = "free", nrow = 7) +
  scale_x_continuous(
    breaks = c(-6.907755, -4.605170, -2.302585, 0.000000, 2.302585, 4.605170),
    labels = c("0.001", "0.01", "0.1", "1", "10", "100")
  )

P1


# Invasive species ----
# Predicted effects with scale

model_type_invas <- read_csv("results/Model_selection_Table.csv") %>%
  filter(response_var == "invasive_percent") %>%
  mutate(
    my.formula = case_when(
      final_model == "m1" ~ "y ~ x",
      .default = "y ~ poly(x, 2)"
    )
  ) %>%
  mutate(
    p_value = case_when(final_model == "m1" ~ m1.p.value, TRUE ~ m2.p.value)
  ) %>%
  mutate(
    signif = case_when(
      p_value < 0.05 ~ "Sign",
      p_value >= 0.05 & p_value < 0.095 ~ "Marg",
      .default = "Nonsig"
    )
  ) %>%
  dplyr::select(
    variable_new,
    predictor,
    my.formula,
    final_model,
    signif,
    p_value
  )


results %>% pull(predictor) %>% unique()


P2 <- ggplot(
  results %>%
    filter(response_var == "invasive_percent") %>%
    filter(!scale %in% c(0.0001, 0.001, 0.01)),
  aes(log(scale), slope)
) +
  mapply(
    function(x, z, signif) {
      geom_smooth(
        method = "lm",
        data = function(d) subset(d, predictor == z),
        formula = x,
        fill = "brown",
        col = "brown",
        alpha = 0.07,
        linetype = ifelse(
          signif == "Sign" | signif == "Marg",
          "solid",
          "dashed"
        ),
        linewidth = case_when(
          signif == "Sign" ~ 1.1,
          signif == "Nonsig" ~ 0.5,
          .default = 0.4
        )
      )
    },
    model_type_invas$my.formula,
    model_type_invas$predictor,
    model_type_invas$signif
  ) +
  geom_point(col = "gray22", size = 2, alpha = 0.6) +
  labs(
    y = "Slope of the driver effect on % invasive species",
    x = expression(paste(
      'Grain size, ',
      m^{
        2
      }
    ))
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 7, colour = "black"),
    axis.text.y = element_text(size = 6, colour = "black"),
    axis.title = element_text(size = 13, colour = "black"),
    legend.text = element_text(size = 10, colour = "black"),
    strip.background = element_rect(colour = "black", fill = "#F9F0F0")
  ) +
  facet_wrap(
    ~ reorder(variable_new, desc(variable_new)),
    scales = "free",
    ncol = 2
  ) +
  scale_x_continuous(
    breaks = c(-2.302585, 0.000000, 2.302585, 4.605170),
    labels = c("0.1", "1", "10", "100")
  )


P2

# Neophytes species ----
# Predicted effects with scale

model_type_neophytes <- read_csv("results/Model_selection_Table.csv") %>%
  filter(response_var == "neophyte_percent") %>%
  # dplyr::select(variable_new, predictor, final_model) %>%
  mutate(
    my.formula = case_when(
      final_model == "m1" ~ "y ~ x",
      .default = "y ~ poly(x, 2)"
    )
  ) %>%
  mutate(
    p_value = case_when(final_model == "m1" ~ m1.p.value, TRUE ~ m2.p.value)
  ) %>%
  mutate(
    signif = case_when(
      p_value < 0.05 ~ "Sign",
      p_value >= 0.05 & p_value < 0.095 ~ "Marg",
      .default = "Nonsig"
    )
  ) %>%
  dplyr::select(
    variable_new,
    predictor,
    my.formula,
    final_model,
    signif,
    p_value
  )

results %>%
  filter(response_var == "neophyte_percent") %>%
  # filter(!scale==0.0001) %>%
  pull(response_var)

P3 <- ggplot(
  results %>%
    filter(response_var == "neophyte_percent") %>%
    filter(!scale %in% c(0.0001, 0.001, 0.01)),
  aes(log(scale), slope)
) +
  mapply(
    function(x, z, signif) {
      geom_smooth(
        method = "lm",
        data = function(d) subset(d, predictor == z),
        formula = x,
        fill = "slateblue",
        col = "slateblue",
        alpha = 0.07,
        linetype = ifelse(
          signif == "Sign" | signif == "Marg",
          "solid",
          "dashed"
        ),
        linewidth = case_when(
          signif == "Sign" ~ 1.1,
          signif == "Nonsig" ~ 0.5,
          .default = 0.4
        )
      )
    },
    model_type_neophytes$my.formula,
    model_type_neophytes$predictor,
    model_type_neophytes$signif
  ) +
  geom_point(col = "gray22", size = 2, alpha = 0.6) +
  labs(
    y = "Slope of the driver effect on % neophytes",
    x = expression(paste(
      'Grain size, ',
      m^{
        2
      }
    ))
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 7, colour = "black"),
    axis.text.y = element_text(size = 6, colour = "black"),
    axis.title = element_text(size = 13, colour = "black"),
    legend.text = element_text(size = 10, colour = "black"),
    strip.background = element_rect(colour = "black", fill = "#F4F3FB")
  ) +
  facet_wrap(
    ~ reorder(variable_new, desc(variable_new)),
    scales = "free",
    ncol = 2
  ) +
  scale_x_continuous(
    breaks = c(-2.302585, 0.000000, 2.302585, 4.605170),
    labels = c("0.1", "1", "10", "100")
  )


P3

# Archaeophytes species ----
# Predicted effects with scale

names(model_type_archaeophytes)

model_type_archaeophytes <- read_csv("results/Model_selection_Table.csv") %>%
  filter(response_var == "archaeophyte_percent") %>%
  # dplyr::select(variable_new, predictor, final_model) %>%
  mutate(
    my.formula = case_when(
      final_model == "m1" ~ "y ~ x",
      .default = "y ~ poly(x, 2)"
    )
  ) %>%
  mutate(
    p_value = case_when(final_model == "m1" ~ m1.p.value, TRUE ~ m2.p.value)
  ) %>%
  mutate(
    signif = case_when(
      p_value < 0.05 ~ "Sign",
      p_value >= 0.05 & p_value < 0.095 ~ "Marg",
      .default = "Nonsig"
    )
  ) %>%
  dplyr::select(
    variable_new,
    predictor,
    my.formula,
    final_model,
    signif,
    p_value
  )

results %>%
  filter(response_var == "archaeophyte_percent") %>%
  # filter(!scale==0.0001) %>%
  pull(response_var)

P4 <- ggplot(
  results %>%
    filter(response_var == "archaeophyte_percent") %>%
    filter(!scale %in% c(0.0001)),
  aes(log(scale), slope)
) +
  mapply(
    function(x, z, signif) {
      geom_smooth(
        method = "lm",
        data = function(d) subset(d, predictor == z),
        formula = x,
        fill = "#5F9EA0",
        col = "#5F9EA0",
        alpha = 0.07,
        linetype = ifelse(
          signif == "Sign" | signif == "Marg",
          "solid",
          "dashed"
        ),
        linewidth = case_when(
          signif == "Sign" ~ 1.1,
          signif == "Nonsig" ~ 0.5,
          .default = 0.4
        )
      )
    },
    model_type_archaeophytes$my.formula,
    model_type_archaeophytes$predictor,
    model_type_archaeophytes$signif
  ) +
  geom_point(col = "gray22", size = 2, alpha = 0.6) +
  labs(
    y = "Slope of the driver effect on % archaeophytes",
    x = expression(paste(
      'Grain size, ',
      m^{
        2
      }
    ))
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 7, colour = "black"),
    axis.text.y = element_text(size = 6, colour = "black"),
    axis.title = element_text(size = 13, colour = "black"),
    legend.text = element_text(size = 10, colour = "black"),
    strip.background = element_rect(colour = "black", fill = "#B5D2D3")
  ) +
  facet_wrap(
    ~ reorder(variable_new, desc(variable_new)),
    scales = "free",
    ncol = 2
  ) +
  #   facet_wrap(~variable_new, scales = "free", nrow = 7) +
  scale_x_continuous(
    breaks = c(-6.907755, -4.605170, -2.302585, 0.000000, 2.302585, 4.605170),
    labels = c("0.001", "0.01", "0.1", "1", "10", "100")
  )


P4
# ---- Combine plots ----
library(patchwork)

combined_plot <- P1 +
  P4 +
  P3 +
  P2 +
  plot_layout(ncol = 4) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = 'bold', size = 18),
    plot.tag.position = c(0.17, 1.045),
    plot.margin = margin(t = 20, r = 10, b = 5, l = 15)
  )


print(combined_plot)


ggsave("results/Fig4.png", combined_plot, width = 12, height = 16, dpi = 150)

# END ----------
