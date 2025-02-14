library(tidyverse)

# Load data
coeff_SAR <- read_csv("data/coeff_SAR.csv")
raw_data <- read_csv("data/database_analysis_summary.csv")
header_data <- read_csv("data/header_data_prepared.csv") |>
  dplyr::select(
    series, habitat_group
  ) |>
  unique()

coeff_SAR <- coeff_SAR |> left_join(header_data, by = "series")
raw_data <- raw_data |> left_join(header_data, by = "series")

coeff_SAR_wide <- coeff_SAR |>
  dplyr::select(-p_value_slope, -r_squared) |>
  pivot_wider(names_from = richness_type, values_from = c(c, z))

slope_n_nn <- ggplot(
  coeff_SAR_wide |> filter(type == "p_a"),
  aes(x = z_native, y = z_non_native, color = habitat_group)
) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  labs(x = "slope native", y = "slope non-native")

slope_n_invasive <- ggplot(
  coeff_SAR_wide |> filter(type == "p_a"),
  aes(x = z_native, y = z_invasive, color = habitat_group)
) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  labs(x = "slope native", y = "slope invasive")

slopes_nn_invasive <- ggplot(
  coeff_SAR_wide |> filter(type == "p_a"),
  aes(x = z_non_native, y = z_invasive, color = habitat_group)
) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  labs(x = "slope non-native", y = "slope invasive")

ggsave("img/slopes_nn_invasive.png", slopes_nn_invasive, width = 5, height = 4)

library(patchwork)
slopes <- slope_n_nn + slope_n_invasive +
  plot_layout(guides = "collect")

ggsave("img/slopes.png", slopes, width = 10, height = 5)

# Calulate fits for c and z estimations
cz_fits <- expand_grid(coeff_SAR, scale = 0:100) %>%
  mutate(
    richness = c * scale^z
  )

raw_data |>
  filter(type == "p_a") |>
  ggplot(aes(x = scale, y = native, group = series, color = habitat_group)) +
  geom_point() +
  geom_line(
    data = cz_fits |> filter(richness_type == "native" & type == "p_a"),
    aes(y = richness, group = series)
  ) +
  facet_wrap(~habitat_group)

raw_data |>
  filter(type == "p_a") |>
  pivot_longer(c(native, non_native, invasive)) |>
  ggplot(aes(x = scale, y = value, color = name)) +
  geom_smooth(
    method = "nls",
    formula = y ~ c * x^z,
    method.args = list(start = c(c = 1, z = 0.1)),
    se = FALSE
  ) +
  geom_point() +
  facet_wrap(~habitat_group)

SAR_curves <- raw_data |>
  filter(type == "p_a") |>
  pivot_longer(c(native, non_native, invasive)) |>
  mutate(name = factor(name, levels = c("native", "non_native", "invasive"))) |>
  ggplot(aes(x = scale, y = value, color = habitat_group)) +
  geom_point(alpha = 0.3) +
  geom_smooth(
    method = "nls",
    formula = y ~ c * x^z,
    method.args = list(start = c(c = 1, z = 0.1)),
    se = FALSE,
    linewidth = 0.8
  ) +
  facet_wrap(~name, scales = "free_y") +
  ggthemes::scale_color_tableau(palette = "Classic Green-Orange 12") +
  theme_bw() +
  labs(y = "Species richness", x = "Scale")

ggsave("img/SAR_curves.png", SAR_curves, width = 20, height = 7,
  units = "cm", scale = 1.1)
