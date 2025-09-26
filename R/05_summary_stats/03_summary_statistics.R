# Purpose: Summary statistics across spatial scales and habitat types for the 
# (1) number of sampled plots (Table S3, Fig.1D,E,F),
# (2) number of plots containing alien species (Table S3),
# (3) minimum, maximum, and mean values for total species richness, 
# native species richness, and the percentage of aliens (Table S4).

# Load packages
library(tidyverse)
library(sjPlot)
library(emmeans) 
library(multcomp) 
library(car)

# Load data ------------------------------------------------
climate_pc <- read_csv("data/Clima_PC.csv") |>
  dplyr::select(series, pca1_clima) |>
  unique()

sp_data <- read_csv("data/database_analysis_summary.csv") %>%
  mutate(
    non_native_percent = non_native / total_species,
    invasive_percent = invasive / total_species,
    archaeophyte_percent = archaeophyte / total_species,
    neophyte_percent = neophyte / total_species,
    p_a_non_native = ifelse(non_native > 0, 1, 0),
    p_a_invasive = ifelse(invasive > 0, 1, 0),
    p_a_neophyte = ifelse(neophyte > 0, 1, 0)
  ) %>%
  left_join(climate_pc, by = "series")


alien_data <- sp_data %>%
  # filter(scale==100) %>%
  left_join(
    read_csv("data/header_data_prepared.csv") %>%
      filter(subplot == "x") %>%
      dplyr::select(-subplot),
    by = "series"
  ) %>%
  dplyr::select(-subplot) %>%
  mutate(dataset = factor(dataset)) %>%
  mutate(
    habitat_broad = fct_relevel(
      habitat_broad,
      c("saline", "complex", "dry", "wet", "mesic", "fringe", "alpine")
    ),
    habitat_group = fct_relevel(
      habitat_group,
      c(
        "saline",
        "pody",
        "sandy",
        "xeric",
        "rocky",
        "meso-xeric",
        "heats",
        "wet",
        "mesic",
        "fringe",
        "alpine"
      )
    )
  )

write_csv(alien_data, "data/alien_dataset_all.csv")


# Plot counts and % -----
## Table S3 -----
# How many plots contain aliens? Number of plots where aliens occur:

###-> summary data across all dataset ----

plot_proportion <- sp_data %>%
  filter(type == "p_a", !scale == 0.0001) %>%
  count(scale, name = "Plot_number") %>%
  left_join(
    sp_data |>
      filter(type == "p_a") %>%
      filter(non_native > 0) %>%
      count(scale, name = "non_native_plot_n"),
    by = c("scale")
  ) %>%
  mutate(
    percent_plots_alien = round(non_native_plot_n * 100 / Plot_number, 2)
  ) %>%
  left_join(
    sp_data |>
      filter(type == "p_a") %>%
      filter(invasive > 0) %>%
      count(scale, name = "invasive_plot_n"),
    by = c("scale")
  ) %>%
  mutate(
    percent_plots_invasive = round(invasive_plot_n * 100 / Plot_number, 2)
  ) %>%
  left_join(
    sp_data |>
      filter(type == "p_a") %>%
      filter(archaeophyte > 0) %>%
      count(scale, name = "archaeophyte_plot_n"),
    by = c("scale")
  ) %>%
  mutate(
    percent_plots_archaeophyte = round(
      archaeophyte_plot_n * 100 / Plot_number,
      2
    )
  ) %>%
  left_join(
    sp_data |>
      filter(type == "p_a") %>%
      filter(neophyte > 0) %>%
      count(scale, name = "neophyte_plot_n"),
    by = c("scale")
  ) %>%
  mutate(
    percent_plots_neophyte = round(neophyte_plot_n * 100 / Plot_number, 2)
  ) %>%
  mutate(habitat_broad = "all") %>%
  relocate(habitat_broad, .before = "scale")


plot_proportion


###> summary data by habitats ----

plot_proportion_habitatas <- alien_data %>%
  filter(type == "p_a", !scale == 0.0001) %>%
  group_by(habitat_broad, scale) %>%
  count(scale, name = "Plot_number") %>%
  left_join(
    alien_data |>
      filter(type == "p_a") %>%
      filter(non_native > 0) %>%
      count(habitat_broad, scale, name = "non_native_plot_n"),
    by = c("habitat_broad", "scale")
  ) %>%
  mutate(
    percent_plots_alien = round(non_native_plot_n * 100 / Plot_number, 2)
  ) %>%
  left_join(
    alien_data |>
      filter(type == "p_a") %>%
      filter(invasive > 0) %>%
      count(habitat_broad, scale, name = "invasive_plot_n"),
    by = c("habitat_broad", "scale")
  ) %>%
  mutate(
    percent_plots_invasive = round(invasive_plot_n * 100 / Plot_number, 2)
  ) %>%
  left_join(
    alien_data |>
      filter(type == "p_a") %>%
      filter(archaeophyte > 0) %>%
      count(habitat_broad, scale, name = "archaeophyte_plot_n"),
    by = c("habitat_broad", "scale")
  ) %>%
  mutate(
    percent_plots_archaeophyte = round(
      archaeophyte_plot_n * 100 / Plot_number,
      2
    )
  ) %>%
  left_join(
    alien_data |>
      filter(type == "p_a") %>%
      filter(neophyte > 0) %>%
      count(habitat_broad, scale, name = "neophyte_plot_n"),
    by = c("habitat_broad", "scale")
  ) %>%
  mutate(
    percent_plots_neophyte = round(neophyte_plot_n * 100 / Plot_number, 2)
  ) %>%
  bind_rows(plot_proportion) %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))


plot_proportion_habitatas

# Minimum, maximum, and mean values ----------
## Table S4 ----
# for total species richness, native species richness, and the percentage of alien

###> summary data by habitats ----
summary_table_habitatas <- alien_data %>% 
  filter(total_species>0, !scale==0.0001, type == "p_a") %>% 
  dplyr::select(scale, habitat_broad, # habitat_group, 
                total_species, native, 
                non_native_percent, invasive_percent, 
                archaeophyte_percent, neophyte_percent) %>% 
  mutate(non_native_percent=non_native_percent*100, 
         invasive_percent=invasive_percent*100, 
         archaeophyte_percent=archaeophyte_percent*100, 
         neophyte_percent=neophyte_percent*100) %>% 
  group_by(habitat_broad, scale) %>% 
  summarise(across(everything(),
                   list(min = ~round(min(.x), 2),
                        max = ~round(max(.x), 2),
                        mean = ~round(mean(.x), 2)))) %>% 
  ungroup()

###-> summary data across all dataset ----
summary_table_all <- alien_data %>% 
  filter(total_species>0, !scale==0.0001, type == "p_a") %>% 
  dplyr::select(scale, 
                total_species, native, 
                non_native_percent, invasive_percent, 
                archaeophyte_percent, neophyte_percent) %>% 
  mutate(non_native_percent=non_native_percent*100, 
         invasive_percent=invasive_percent*100, 
         archaeophyte_percent=archaeophyte_percent*100, 
         neophyte_percent=neophyte_percent*100) %>% 
  group_by(scale) %>% 
  summarise(across(everything(),
                   list(min = ~round(min(.x), 2),
                        max = ~round(max(.x), 2),
                        mean = ~round(mean(.x), 2)))) %>% 
  ungroup()  %>% 
  mutate(habitat_broad="all") %>% 
  relocate(habitat_broad, .before = "scale") %>% 
  bind_rows(summary_table_habitatas) %>% 
  left_join(plot_proportion_habitatas, by=c("habitat_broad" , "scale")) 

summary_table_all

# save Table S3_S4-----
write_csv(summary_table_all, "results/summary_table_habitatas_TableS3_S4.csv")

##  Fig. 1 (D-F)-----
## Fig.1 D - Aliens
plot_proportion %>%
  ggplot(aes(y = factor(scale), x = percent_plots_alien)) +
  geom_bar(
    position = "stack",
    stat = "identity",
    colour = "black",
    fill = "forestgreen"
  ) +
  geom_text(aes(label = non_native_plot_n), hjust = -0.1, size = 3) +
  #  coord_flip() +
  theme_bw() +
  labs(
    y = expression(paste(
      "Grain size, ",
      m^{
        2
      }
    )),
    x = "% plots with alien species"
  ) +
  theme(
    axis.text.y = element_text(colour = "black", size = 9),
    axis.text.x = element_text(colour = "black", size = 9),
    axis.title = element_text(size = 13),
    legend.position = "none",
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.y = element_blank()
  ) +
  xlim(-1, 85)


## Fig.1 G - Invasives
plot_proportion %>%
  ggplot(aes(y = factor(scale), x = percent_plots_invasive)) +
  geom_bar(
    position = "stack",
    stat = "identity",
    colour = "black",
    fill = "brown"
  ) +
  geom_text(aes(label = invasive_plot_n), hjust = -0.1, size = 3) +
  #  coord_flip() +
  theme_bw() +
  labs(
    y = expression(paste(
      "Grain size, ",
      m^{
        2
      }
    )),
    x = "% plots with invasive species"
  ) +
  theme(
    axis.text.y = element_text(colour = "black", size = 9),
    axis.text.x = element_text(colour = "black", size = 9),
    axis.title = element_text(size = 13),
    legend.position = "none",
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.y = element_blank()
  ) +
  xlim(-1, 47)


# ## Fig.1 F - Neophytes

plot_proportion %>%
  ggplot(aes(y = factor(scale), x = percent_plots_neophyte)) +
  geom_bar(
    position = "stack",
    stat = "identity",
    colour = "black",
    fill = "slateblue"
  ) +
  geom_text(aes(label = neophyte_plot_n), hjust = -0.1, size = 3) +
  #  coord_flip() +
  theme_bw() +
  labs(
    y = expression(paste(
      "Grain size, ",
      m^{
        2
      }
    )),
    x = "% plots with neophytes"
  ) +
  theme(
    axis.text.y = element_text(colour = "black", size = 9),
    axis.text.x = element_text(colour = "black", size = 9),
    axis.title = element_text(size = 13),
    legend.position = "none",
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.y = element_blank()
  ) +
  xlim(-1, 45)

# ##  Fig. 1E -  Archaeophyte
plot_proportion %>%
  ggplot(aes(y = factor(scale), x = percent_plots_archaeophyte)) +
  geom_bar(
    position = "stack",
    stat = "identity",
    colour = "black",
    fill = "cadetblue"
  ) +
  geom_text(aes(label = archaeophyte_plot_n), hjust = -0.1, size = 3) +
  #  coord_flip() +
  theme_bw() +
  labs(
    y = expression(paste(
      "Grain size, ",
      m^{
        2
      }
    )),
    x = "% plots with archaeophytes"
  ) +
  theme(
    axis.text.y = element_text(colour = "black", size = 9),
    axis.text.x = element_text(colour = "black", size = 9),
    axis.title = element_text(size = 13),
    legend.position = "none",
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.y = element_blank()
  ) +
  xlim(-1, 77)


