# Variance explained by each predictor
# Packages
library(tidyverse)

# Data
alien_dat <- read_csv("data/model_results_summary.csv") %>%
  filter(!model_id == "climate_native") %>%
  filter(!(remove_zeroes == FALSE & model_id == "native")) %>%
  #  filter(response_var=="non_native_percent") %>%
  filter(!scale == 0.0001) %>%
  select(response_var, scale, predictor, r2_partial) %>%
  mutate(variable_new =
    fct_recode(predictor,
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
    )) %>%
  mutate(variable_new = fct_relevel(variable_new,
    "Native diversity", "Climate PC", "Soil pH", "Heat index",
    "Microrelief", "Gravel & stone cover",
    "Herb cover", "Litter cover",
    "Grazing", "Mowing", "Abandonment",
    "Urban built-up", "Road density",
    "Disturbance frequency", "Disturbance severity")) %>%
  mutate(variable_new = fct_relevel(variable_new, rev)) %>%
  mutate(response_var_new = factor(case_when(response_var == "non_native_percent" ~
    "Alien species",
  response_var == "invasive_percent" ~
    "Invasive species")))





alien_dat %>%
  filter(model_id == "native") %>%
  # pull(predictor) %>% unique() %>%
  print(n = 27)

alien_dat %>%
  pull(model_id) %>% unique()

# # Bubble chart showing partial R2



ggplot(alien_dat %>%
  filter(response_var == "non_native_percent" |
    response_var == "invasive_percent" & !scale %in% c(0.0001, 0.001, 0.01)) %>%
  mutate(r2_partial = (ifelse(r2_partial > 0, r2_partial, NA))),
aes(x = as.factor(scale),
  y = variable_new,
  colour = as.factor(scale),
  size = r2_partial)) +
  geom_point() +
  geom_text(aes(label = round(r2_partial, 2)), colour = "black", size = 4) +
  # scale_x_discrete(position = "top") +
  scale_size_continuous(range = c(2, 17)) +
  scale_color_manual(values = c("#FDE725", "#8FD644", "#3ABA76",
    "#2A768E", "#443A83", "#450559")) +
  #  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom",
    legend.key = element_rect(fill = NA, color = NA),
    panel.background = element_blank(),
    panel.grid = element_line(colour = "grey80"),
    axis.ticks = element_blank(),
    axis.text.y = element_text(colour = "black", size = 15),
    axis.text.x = element_text(colour = "black", size = 15)) +
  labs(x = NULL, y = NULL,
    size = bquote("Variance explained, partial R"^"2")) +
  guides(colour = FALSE, size = guide_legend(override.aes = list(colour = "grey"))) +
  theme(legend.title = element_text(size = 11),
    legend.text = element_text(size = rel(1)),
    strip.text = element_text(size = 15)) +
  facet_wrap("response_var_new", scales = "free_x")




fig_alien <- ggplot(alien_dat %>%
  filter(response_var == "non_native_percent") %>% mutate(r2_partial = (ifelse(r2_partial > 0, r2_partial, NA))),
aes(x = as.factor(scale),
  y = variable_new,
  colour = as.factor(scale),
  size = r2_partial)) +
  geom_point() +
  #  geom_text(aes(label = r2_part),  colour = "black",  size = 4) +
  # scale_x_discrete(position = "top") +
  scale_size_continuous(range = c(2, 17)) +
  scale_color_manual(values = c("#FDE725", "#8FD644", "#3ABA76",
    "#2A768E", "#443A83", "#450559")) +
  #  labs(x = NULL, y = NULL) +
  theme(legend.position = "top",
    legend.key = element_rect(fill = NA, color = NA),
    panel.background = element_blank(),
    panel.grid = element_line(colour = "grey80"),
    axis.ticks = element_blank(),
    axis.text.y = element_text(colour = "black", size = 15),
    axis.text.x = element_text(colour = "black", size = 15)) +
  labs(x = NULL, y = NULL,
    size = bquote("Variance explained, partial R"^"2")) +
  guides(colour = FALSE, size = guide_legend(override.aes = list(colour = "grey"))) +
  theme(legend.title = element_text(size = 14),
    legend.text = element_text(size = rel(1.2)))

fig_alien





fig_inv <- ggplot(alien_dat %>%
  filter(response_var == "invasive_percent") %>%
  # filter(!scale %in% c(0.0001, 0.001, 0.01)) %>%
  mutate(r2_partial = (ifelse(r2_partial > 0, r2_partial, NA))),
aes(x = as.factor(scale),
  y = variable_new,
  colour = as.factor(scale),
  size = r2_partial)) +
  geom_point() +
  geom_text(aes(label = r2_partial), colour = "black", size = 4) +
  # scale_x_discrete(position = "top") +
  scale_size_continuous(range = c(2, 17)) +
  scale_color_manual(values = c("#FDE725", "#8FD644", "#3ABA76",
    "#2A768E", "#443A83", "#450559")) +
  # scale_color_brewer(palette =  "Paired") +
  #  labs(x = NULL, y = NULL) +
  theme(legend.position = "top",
    legend.key = element_rect(fill = NA, color = NA),
    panel.background = element_blank(),
    panel.grid = element_line(colour = "grey80"),
    axis.ticks = element_blank(),
    axis.text.y = element_text(colour = "black", size = 15),
    axis.text.x = element_text(colour = "black", size = 15)) +
  labs(x = NULL, y = NULL,
    size = bquote("Variance explained, partial R"^"2")) +
  guides(colour = FALSE, size = guide_legend(override.aes = list(colour = "grey"))) +
  theme(legend.title = element_text(size = 14),
    legend.text = element_text(size = rel(1.2)))



fig_inv




?facet_wrap
