# Figure 5 alternative: Heatmap instead of bubble chart
# Packages
library(tidyverse)

# Prepare data

alien_dat <- read_csv("data/model_results_summary.csv") %>%
  filter(!model_id == "climate_native") %>%
  filter(!(remove_zeroes == FALSE & model_id == "native")) %>%
  filter(scale != 0.0001) %>%
  select(response_var, scale, predictor, r2_partial) %>%
  mutate(
    variable_new =
      fct_recode(
        predictor,
        "Native diversity" = "native",
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
        "Urban built-up" = "built_up_2km",
        "Road density" = "roads",
        "Disturbance frequency" = "Disturbance.Frequency",
        "Disturbance severity" = "Disturbance.Severity"
      )
  ) %>%
  mutate(
    variable_new = fct_relevel(
      variable_new,
      "Native diversity",
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
      "Urban built-up",
      "Road density",
      "Disturbance frequency",
      "Disturbance severity"
    )
  ) %>%
  mutate(variable_new = fct_relevel(variable_new, rev)) %>%
  mutate(response_var_new = factor(
    case_when(
      response_var == "non_native_percent" ~
        "Alien species",
      response_var == "invasive_percent" ~
        "Invasive species"
    )
  ))

data_to_plot <- alien_dat %>%
  filter(
    response_var == "non_native_percent" |
      response_var == "invasive_percent" &
        !scale %in% c(0.0001, 0.001, 0.01)
  ) %>%
  mutate(
    r2_partial = round(r2_partial, 2)
  ) |>
  mutate(
    r2_partial = ifelse(near(r2_partial, 0, 0.001), NA, r2_partial)
  )

# Add color for the text -> if the value of r2 is high, the
# text color should be white, otherwise not legible on the fill
# color scale
data_to_plot$color_text <- ifelse(data_to_plot$r2_partial > 0.5,
  "white", "black")

fig_5 <- ggplot(data_to_plot,
  aes(
    x = as.factor(scale),
    y = variable_new,
    fill = r2_partial
  )
) +
  geom_tile() +
  facet_grid(. ~ response_var_new, scales = "free_x", space = "free") +
  labs(
    fill = bquote("Variance explained, partial R"^"2"),
    x = bquote("Grain size m"^"2"),
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(color = "black", size = 12),
    axis.text = element_text(color = "black", size = 10),
  ) +
  scale_fill_distiller(
    palette = 6,
    direction = 1,
    na.value = "white"
  ) +
  geom_text(aes(label = r2_partial, color = color_text)) +
  scale_color_manual(values = c("black", "white"), guide = FALSE)

ggsave(filename = "fig5.png", fig_5,
  width = 16, height = 10, units = "cm")
