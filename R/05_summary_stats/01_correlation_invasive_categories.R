# Calculate correlations between different categories of alien species

# Load packages
library(tidyverse)

# Read the data --------------------------------------------------------------
species <- read_csv("data/database_analysis_summary.csv") |>
  filter(type == "p_a", !scale == 0.0001) %>%
  mutate(
    non_native_percent = non_native / total_species,
    invasive_percent = invasive / total_species,
    archaeophyte_percent = archaeophyte / total_species,
    neophyte_percent = neophyte / total_species
  ) %>%
  mutate(scale = factor(scale))


names(species)

# Correlation between number of neophytes and number of invasive in each plot
ggplot(species, aes(x = neophyte, y = invasive)) +
  geom_point(
    aes(fill = scale),
    position = position_jitter(width = 0.15, height = 0.15, seed = 123),
    pch = 21,
    size = 2
  ) +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Number of neophytes", y = "Number of invasive species") +
  theme_bw() +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(breaks = 0:5) +
  facet_wrap(~scale, nrow = 1)

# Correlation between % neophytes and % invasive in each plot
ggplot(species, aes(x = neophyte_percent, y = invasive_percent)) +
  geom_point(
    aes(fill = scale),
    position = position_jitter(width = 0.008, height = 0.008, seed = 123),
    pch = 21,
    size = 2
  ) +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Proportion of neophytes", y = "Proportion of invasive species") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~scale, nrow = 1) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 1))


# Correlation between number of archaeophytes and number of aliens in each plot
ggplot(species, aes(x = non_native, y = archaeophyte)) +
  geom_point(
    aes(fill = scale),
    position = position_jitter(width = 0.3, height = 0.3, seed = 123),
    pch = 21,
    size = 2
  ) +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Number of archaeophytes", y = "Number of alien species") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 14, by = 2)) +
  scale_y_continuous(breaks = seq(0, 16, by = 2)) +
  facet_wrap(~scale, nrow = 1)

# Correlation between number of archaeophytes and number of aliens in each plot
ggplot(species, aes(x = non_native_percent, y = archaeophyte_percent)) +
  geom_point(
    aes(fill = scale),
    position = position_jitter(width = 0.02, height = 0.02, seed = 123),
    pch = 21,
    size = 2
  ) +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Proportion of archaeophytes", y = "Proportion of alien species") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~scale, nrow = 1)

# Pairwise Pearson correlations among alien species groups
species %>%
  dplyr::select(
    invasive,
    neophyte,
    neophyte_percent,
    invasive_percent,
    non_native,
    archaeophyte,
    non_native_percent,
    archaeophyte_percent
  ) %>%
  cor(method = "pearson") |>
  ggcorrplot::ggcorrplot(
    type = "upper",
    lab = TRUE,
    lab_size = 3.5,
    hc.order = F,
    colors = c("red", "white", "blue")
  )
