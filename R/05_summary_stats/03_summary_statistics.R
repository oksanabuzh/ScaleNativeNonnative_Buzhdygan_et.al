# Purpose: Summary statistics for vegetation data

# Load packages
library(tidyverse)
library(sjPlot)
library(emmeans) # for posthoc test
library(multcomp) # for posthoc tests wit the letters
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

## summary data across all dataset ----

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


## summary data by habitats ----

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


##  Fig. 1 (D, E, F)-----
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


## Fig.1 E - Invasives
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

# ##  Archaeophyte

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


# Habitat effects-----------------------------------------------------------------------

summary_table_habitatas <- alien_data %>%
  filter(total_species > 0, !scale == 0.0001, type == "p_a") %>%
  dplyr::select(
    scale,
    habitat_broad, # habitat_group,
    total_species,
    native,
    non_native_percent,
    invasive_percent,
    archaeophyte_percent,
    neophyte_percent
  ) %>%
  mutate(
    non_native_percent = non_native_percent * 100,
    invasive_percent = invasive_percent * 100,
    archaeophyte_percent = archaeophyte_percent * 100,
    neophyte_percent = neophyte_percent * 100
  ) %>%
  group_by(habitat_broad, scale) %>%
  summarise(across(
    everything(),
    list(
      min = ~ round(min(.x), 2),
      max = ~ round(max(.x), 2),
      mean = ~ round(mean(.x), 2)
    )
  )) %>%
  ungroup()


summary_table_all <- alien_data %>%
  filter(total_species > 0, !scale == 0.0001, type == "p_a") %>%
  dplyr::select(
    scale,
    total_species,
    native,
    non_native_percent,
    invasive_percent,
    archaeophyte_percent,
    neophyte_percent
  ) %>%
  mutate(
    non_native_percent = non_native_percent * 100,
    invasive_percent = invasive_percent * 100,
    archaeophyte_percent = archaeophyte_percent * 100,
    neophyte_percent = neophyte_percent * 100
  ) %>%
  group_by(scale) %>%
  summarise(across(
    everything(),
    list(
      min = ~ round(min(.x), 2),
      max = ~ round(max(.x), 2),
      mean = ~ round(mean(.x), 2)
    )
  )) %>%
  ungroup() %>%
  mutate(habitat_broad = "all") %>%
  relocate(habitat_broad, .before = "scale") %>%
  bind_rows(summary_table_habitatas) %>%
  left_join(plot_proportion_habitatas, by = c("habitat_broad", "scale"))

summary_table_all

# Table S3_S4-----
write_csv(summary_table_all, "results/summary_table_habitatas_TableS3_S4.csv")

# Pairwise comparisons among grassland types --------------------
## Presence/absence data---------------
alien_data_100 <- alien_data %>%
  filter(type == "p_a" & scale == 100)


alien_data_100

alien_data_100$habitat_group
alien_data_100$habitat_broad

### native SR ----

mod2h <- glm(
  native ~ habitat_broad,
  family = poisson(),
  data = alien_data_100 %>% filter(type == "p_a" & scale == 100)
)

# check_convergence(mod2h)
car::Anova(mod2h)
summary(mod2h)

Table_mod2h <- emmeans(
  mod2h,
  list(pairwise ~ habitat_broad)
)$`pairwise differences of habitat_broad` %>%
  as.tibble()

Table_mod2h_means <- emmeans(
  mod2h,
  list(pairwise ~ habitat_broad)
)$`emmeans of habitat_broad` %>%
  as.tibble()
Table_mod2h_means


emmeans_m2_habitat <- cld(
  emmeans(mod2h, list(pairwise ~ habitat_broad)),
  Letters = letters
) %>%
  arrange(habitat_broad)
emmeans_m2_habitat


#         saline    complex       dry       wet       mesic        fringe       alpine
col = c(
  "#4e3910",
  "#CC6600",
  "yellow3",
  "#CC99FF",
  "#0066FF",
  "#00B200",
  "#006600"
)
col2 = c(
  "#4e3910",
  "#CC6600",
  "yellow",
  "#CC99FF",
  "#0066FF",
  "#00B200",
  "#006600"
)

alien_data_100 %>%
  filter(type == "p_a" & scale == 100) %>%
  ggplot(aes(habitat_broad, native, col = habitat_broad)) +
  geom_boxplot(alpha = 0, lwd = 0.6, outlier.shape = NA) +
  geom_point(
    aes(fill = habitat_broad),
    pch = 21,
    size = 2,
    alpha = 0.4,
    stroke = 0.8,
    position = position_jitterdodge(jitter.width = 0.6, jitter.height = 0)
  ) +
  scale_color_manual(values = col) +
  scale_fill_manual(values = col2) +
  labs(
    y = "Native species richness",
    x = 'Grassland type',
    col = "Grassland type",
    fill = "Grassland type"
  ) +
  # guides(shape = "none") +
  theme_bw() +
  geom_text(
    data = emmeans_m2_habitat,
    aes(
      x = habitat_broad,
      y = c(42, 40, 97, 50, 90, 85, 60),
      label = emmeans_m2_habitat$.group
    ),
    vjust = 0.5,
    hjust = 0.7,
    size = 4,
    col = "black",
    position = position_dodge(0)
  )


### non_native_percent ----

mod1h <- glm(
  non_native_percent ~ habitat_broad,
  weights = total_species,
  family = binomial,
  data = alien_data_100
)

#check_convergence(mod1h)
Anova(mod1h)
summary(mod1h)
# plot_model(mod1h,type = "pred", terms=c("scale","habitat_group"),  show.data=T)

Table_mod1h <- emmeans(
  mod1h,
  list(pairwise ~ habitat_broad)
)$`pairwise differences of habitat_broad` %>%
  as.tibble()
Table_mod1h

Table_mod1h_means <- emmeans(
  mod1h,
  list(pairwise ~ habitat_broad)
)$`emmeans of habitat_broad` %>%
  as.tibble()
Table_mod1h_means


emmeans_m1_habitat <- cld(
  emmeans(mod1h, list(pairwise ~ habitat_broad)),
  Letters = letters
) %>%
  arrange(habitat_broad)
emmeans_m1_habitat


alien_data_100 %>%
  ggplot(aes(habitat_broad, non_native_percent, col = habitat_broad)) +
  geom_boxplot(alpha = 0, lwd = 0.6, outlier.shape = NA) +
  geom_point(
    aes(fill = habitat_broad),
    pch = 21,
    size = 2,
    alpha = 0.4,
    stroke = 0.8,
    position = position_jitterdodge(jitter.width = 0.6, jitter.height = 0)
  ) +
  scale_color_manual(values = col) +
  scale_fill_manual(values = col2) +
  labs(
    y = "Proportion of alien species",
    x = 'Grassland type',
    col = "Grassland type",
    fill = "Grassland type"
  ) +
  # guides(shape = "none") +
  theme_bw() +
  geom_text(
    data = emmeans_m1_habitat,
    aes(
      x = habitat_broad,
      y = c(0.36, 0.31, 0.36, 0.1, 0.18, 0.05, 0.05),
      label = emmeans_m1_habitat$.group
    ),
    vjust = 0.5,
    hjust = 0.5,
    size = 4,
    col = "black",
    position = position_dodge(0)
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#------------------------------------------------------------#

### invasive_percent ----

mod3h <- glm(
  invasive_percent ~ habitat_broad,
  weights = total_species,
  family = binomial,
  data = alien_data_100
)

#check_convergence(mod1h)
Anova(mod3h)
summary(mod3h)
# plot_model(mod1h,type = "pred", terms=c("scale","habitat_group"),  show.data=T)

Table_mod3h <- emmeans(
  mod3h,
  list(pairwise ~ habitat_broad)
)$`pairwise differences of habitat_broad` %>%
  as.tibble()
Table_mod3h

Table_mod3h_means <- emmeans(
  mod3h,
  list(pairwise ~ habitat_broad)
)$`emmeans of habitat_broad` %>%
  as.tibble()
Table_mod3h_means


emmeans_m3_habitat <- cld(
  emmeans(mod3h, list(pairwise ~ habitat_broad)),
  Letters = letters
) %>%
  arrange(habitat_broad)
emmeans_m3_habitat


?emmeans


alien_data_100 %>%
  ggplot(aes(habitat_broad, invasive_percent, col = habitat_broad)) +
  geom_boxplot(alpha = 0, lwd = 0.6, outlier.shape = NA) +
  geom_point(
    aes(fill = habitat_broad),
    pch = 21,
    size = 2,
    alpha = 0.4,
    stroke = 0.8,
    position = position_jitterdodge(jitter.width = 0.6, jitter.height = 0)
  ) +
  scale_color_manual(values = col) +
  scale_fill_manual(values = col2) +
  labs(
    y = "Proportion of invasive species",
    x = 'Grassland type',
    col = "Grassland type",
    fill = "Grassland type"
  ) +
  # guides(shape = "none") +
  theme_bw() +
  geom_text(
    data = emmeans_m3_habitat,
    aes(
      x = habitat_broad,
      y = c(0.055, 0.05, 0.11, 0.03, 0.07, 0.01, 0.01),
      label = emmeans_m3_habitat$.group
    ),
    vjust = 0.5,
    hjust = 0.5,
    size = 4,
    col = "black",
    position = position_dodge(0)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.11)
  )

### neophyte_percent ----

mod4h <- glm(
  neophyte_percent ~ habitat_broad,
  weights = total_species,
  family = binomial,
  data = alien_data_100
)

#check_convergence(mod1h)
Anova(mod4h)
summary(mod4h)
# plot_model(mod1h,type = "pred", terms=c("scale","habitat_group"),  show.data=T)

Table_mod4h <- emmeans(
  mod4h,
  list(pairwise ~ habitat_broad)
)$`pairwise differences of habitat_broad` %>%
  as.tibble()
Table_mod4h

Table_mod4h_means <- emmeans(
  mod4h,
  list(pairwise ~ habitat_broad)
)$`emmeans of habitat_broad` %>%
  as.tibble()
Table_mod4h_means


emmeans_m4_habitat <- cld(
  emmeans(mod4h, list(pairwise ~ habitat_broad)),
  Letters = letters
) %>%
  arrange(habitat_broad)
emmeans_m4_habitat


alien_data_100 %>%
  ggplot(aes(habitat_broad, neophyte_percent, col = habitat_broad)) +
  geom_boxplot(alpha = 0, lwd = 0.6, outlier.shape = NA) +
  geom_point(
    aes(fill = habitat_broad),
    pch = 21,
    size = 2,
    alpha = 0.4,
    stroke = 0.8,
    position = position_jitterdodge(jitter.width = 0.6, jitter.height = 0)
  ) +
  scale_color_manual(values = col) +
  scale_fill_manual(values = col2) +
  labs(
    y = "Proportion of neophytes",
    x = 'Grassland type',
    col = "Grassland type",
    fill = "Grassland type"
  ) +
  # guides(shape = "none") +
  theme_bw() +
  geom_text(
    data = emmeans_m4_habitat,
    aes(
      x = habitat_broad,
      y = c(0.06, 0.06, 0.11, 0.04, 0.07, 0.01, 0.01),
      label = emmeans_m4_habitat$.group
    ),
    vjust = 0.5,
    hjust = 0.5,
    size = 4,
    col = "black",
    position = position_dodge(0)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.11)
  )


### archaeophyte_percent ----

mod5h <- glm(
  archaeophyte_percent ~ habitat_broad,
  weights = total_species,
  family = binomial,
  data = alien_data_100
)

#check_convergence(mod1h)
Anova(mod5h)
summary(mod5h)
# plot_model(mod1h,type = "pred", terms=c("scale","habitat_group"),  show.data=T)

Table_mod5h <- emmeans(
  mod5h,
  list(pairwise ~ habitat_broad)
)$`pairwise differences of habitat_broad` %>%
  as.tibble()
Table_mod5h

Table_mod5h_means <- emmeans(
  mod5h,
  list(pairwise ~ habitat_broad)
)$`emmeans of habitat_broad` %>%
  as.tibble()
Table_mod4h_means


emmeans_mod5h_habitat <- cld(
  emmeans(mod5h, list(pairwise ~ habitat_broad)),
  Letters = letters
) %>%
  arrange(habitat_broad)
emmeans_mod5h_habitat


alien_data_100 %>%
  ggplot(aes(habitat_broad, archaeophyte_percent, col = habitat_broad)) +
  geom_boxplot(alpha = 0, lwd = 0.6, outlier.shape = NA) +
  geom_point(
    aes(fill = habitat_broad),
    pch = 21,
    size = 2,
    alpha = 0.4,
    stroke = 0.8,
    position = position_jitterdodge(jitter.width = 0.6, jitter.height = 0)
  ) +
  scale_color_manual(values = col) +
  scale_fill_manual(values = col2) +
  labs(
    y = "Proportion of archaeophytes",
    x = 'Grassland type',
    col = "Grassland type",
    fill = "Grassland type"
  ) +
  # guides(shape = "none") +
  theme_bw() +
  geom_text(
    data = emmeans_mod5h_habitat,
    aes(
      x = habitat_broad,
      y = c(0.3, 0.33, 0.36, 0.07, 0.12, 0.05, 0.05),
      label = emmeans_mod5h_habitat$.group
    ),
    vjust = 0.5,
    hjust = 0.5,
    size = 4,
    col = "black",
    position = position_dodge(0)
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#---------------------------------------------------#
# save posthoc tables -----
post_hoc_table <- Table_mod1h %>%
  dplyr::select(-df) %>%
  left_join(Table_mod2h %>% dplyr::select(-df), by = "1") %>%
  setNames(c(
    "Habitats",
    "estimate",
    "SE",
    "z-ratio",
    "p-value",
    "estimate2",
    "SE2",
    "z-ratio2",
    "p-value2"
  ))

write_csv(post_hoc_table, "results/habitat_posthoc_TableS5.csv")


emmeans_table <- Table_mod1h_means %>%
  dplyr::select(-df) %>%
  left_join(Table_mod2h_means %>% dplyr::select(-df), by = "habitat_broad") %>%
  setNames(c(
    "habitats",
    "emmean",
    "SE",
    "asymp.LCL",
    "asymp.UCL",
    "emmean2",
    "SE2",
    "asymp.LCL2",
    "asymp.UCL2"
  ))

write_csv(emmeans_table, "results/emmeans_TableS5.csv")


dev.off()

# Plant Cover data --------------------------------------------------------------------
alien_data_100_cover <- alien_data %>%
  filter(type == "cover" & scale == 100)


alien_data_100_cover

### non_native_percent ---------------------------------------------------------

# zero-one inflated beta family in glmmTMB
# zero-one inflated as data contains 0 or/and 1
# mod2_cover <- glmmTMB(non_native_percent ~ habitat_broad,
#                      family = beta_family(link = "logit"), zi = ~1,
#                      data = alien_data_100_cover)

# asin(sqrt) transformation
mod2_cover <- lm(
  non_native_percent ~ habitat_broad,
  data = alien_data_100_cover %>%
    mutate(non_native_percent = asin(sqrt(non_native_percent)))
)

par(mfrow = c(2, 2))
plot(mod2_cover)
par(mfrow = c(1, 1))

#check_convergence(mod2_cover)
Anova(mod2_cover)
summary(mod2_cover)
# plot_model(mod2_cover,type = "pred", terms=c("scale","habitat_group"),  show.data=T)

Table_mod2_cover <- emmeans(
  mod2_cover,
  list(pairwise ~ habitat_broad)
)$`pairwise differences of habitat_broad` %>%
  as.tibble()
Table_mod2_cover

Table_mod2_cover_means <- emmeans(
  mod2_cover,
  list(pairwise ~ habitat_broad)
)$`emmeans of habitat_broad` %>%
  as.tibble()
Table_mod2_cover_means


emmeans_mod2_cover_habitat <- cld(
  emmeans(mod2_cover, list(pairwise ~ habitat_broad)),
  Letters = letters
) %>%
  arrange(habitat_broad)
emmeans_mod2_cover_habitat


alien_data_100_cover %>%
  ggplot(aes(habitat_broad, non_native_percent, col = habitat_broad)) +
  geom_boxplot(alpha = 0, lwd = 0.6, outlier.shape = NA) +
  geom_point(
    aes(fill = habitat_broad),
    pch = 21,
    size = 2,
    alpha = 0.4,
    stroke = 0.8,
    position = position_jitterdodge(jitter.width = 0.6, jitter.height = 0)
  ) +
  scale_color_manual(values = col) +
  scale_fill_manual(values = col2) +
  labs(
    y = "Proportion of cover contributed by alien species",
    x = 'Grassland type',
    col = "Grassland type",
    fill = "Grassland type"
  ) +
  # guides(shape = "none") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# geom_text(data=emmeans_mod2_cover_habitat,
#            aes(x=habitat_broad, y=c(0.36, 0.16, 0.27, 0.04, 0.1, 0.02, 0.02),
#                label=emmeans_mod2_cover_habitat$.group),vjust=0.5, hjust=0.5,
#            size=4, col="black" , position=position_dodge(0))

#------------------------------------------------------------#

### invasive_percent ----

mod3_cover <- lm(
  invasive_percent ~ habitat_broad,
  data = alien_data_100_cover %>%
    mutate(invasive_percent = asin(sqrt(invasive_percent)))
)


par(mfrow = c(2, 2))
plot(mod3_cover)
par(mfrow = c(1, 1))

#check_convergence
Anova(mod3_cover)
summary(mod3_cover)

Table_mod3_cover <- emmeans(
  mod3_cover,
  list(pairwise ~ habitat_broad)
)$`pairwise differences of habitat_broad` %>%
  as.tibble()
Table_mod3_cover

Table_mod3_cover_means <- emmeans(
  mod3_cover,
  list(pairwise ~ habitat_broad)
)$`emmeans of habitat_broad` %>%
  as.tibble()
Table_mod3_cover_means


emmeans_m3_habitat <- cld(
  emmeans(mod3_cover, list(pairwise ~ habitat_broad)),
  Letters = letters
) %>%
  arrange(habitat_broad)
emmeans_m3_habitat


alien_data_100_cover %>%
  ggplot(aes(habitat_broad, invasive_percent, col = habitat_broad)) +
  geom_boxplot(alpha = 0, lwd = 0.6, outlier.shape = NA) +
  geom_point(
    aes(fill = habitat_broad),
    pch = 21,
    size = 2,
    alpha = 0.4,
    stroke = 0.8,
    position = position_jitterdodge(jitter.width = 0.6, jitter.height = 0)
  ) +
  scale_color_manual(values = col) +
  scale_fill_manual(values = col2) +
  labs(
    y = "Proportion of cover contributed by invasive species",
    x = 'Grassland type',
    col = "Grassland type",
    fill = "Grassland type"
  ) +
  # guides(shape = "none") +
  theme_bw() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.077)
  )
# geom_text(data=emmeans_m3_habitat,
#           aes(x=habitat_broad, y=c(0.015, 0.017, 0.081, 0.01, 0.03, 0.005, 0.005),
#               label=emmeans_m3_habitat$.group),vjust=0.5, hjust=0.5,
#          size=4, col="black" , position=position_dodge(0))

### neophyte_percent ----

mod4_cover <- lm(
  neophyte_percent ~ habitat_broad,
  data = alien_data_100_cover %>%
    mutate(neophyte_percent = asin(sqrt(invasive_percent)))
)


par(mfrow = c(2, 2))
plot(mod4_cover)
par(mfrow = c(1, 1))

#check_convergence
Anova(mod4_cover)
summary(mod4_cover)
# plot_model(mod2_cover,type = "pred", terms=c("scale","habitat_group"),  show.data=T)

Table_mod4_cover <- emmeans(
  mod4_cover,
  list(pairwise ~ habitat_broad)
)$`pairwise differences of habitat_broad` %>%
  as.tibble()
Table_mod4_cover

Table_mod4_cover_means <- emmeans(
  mod4_cover,
  list(pairwise ~ habitat_broad)
)$`emmeans of habitat_broad` %>%
  as.tibble()
Table_mod4_cover_means


emmeans_m4_cover_habitat <- cld(
  emmeans(mod4_cover, list(pairwise ~ habitat_broad)),
  Letters = letters
) %>%
  arrange(habitat_broad)
emmeans_m4_cover_habitat


alien_data_100_cover %>%
  ggplot(aes(habitat_broad, neophyte_percent, col = habitat_broad)) +
  geom_boxplot(alpha = 0, lwd = 0.6, outlier.shape = NA) +
  geom_point(
    aes(fill = habitat_broad),
    pch = 21,
    size = 2,
    alpha = 0.4,
    stroke = 0.8,
    position = position_jitterdodge(jitter.width = 0.6, jitter.height = 0)
  ) +
  scale_color_manual(values = col) +
  scale_fill_manual(values = col2) +
  labs(
    y = "Proportion of cover contributed by neophytes",
    x = 'Grassland type',
    col = "Grassland type",
    fill = "Grassland type"
  ) +
  # guides(shape = "none") +
  theme_bw() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.077)
  )
# geom_text(data=emmeans_m4_cover_habitat,
#           aes(x=habitat_broad, y=c(0.015, 0.005, 0.083, 0.007, 0.04, 0.005, 0.005),
#               label=emmeans_m4_cover_habitat$.group),vjust=0.5, hjust=0.5,
#           size=4, col="black" , position=position_dodge(0))

### archaeophyte_percent ----

mod5_cover <- lm(
  archaeophyte_percent ~ habitat_broad,
  data = alien_data_100_cover %>%
    mutate(neophyte_percent = asin(sqrt(archaeophyte_percent)))
)


par(mfrow = c(2, 2))
plot(mod5_cover)
par(mfrow = c(1, 1))

#check_convergence(mod2_cover)
Anova(mod5_cover)
summary(mod5_cover)
# plot_model(mod2_cover,type = "pred", terms=c("scale","habitat_group"),  show.data=T)

Table_mod5_cover <- emmeans(
  mod5_cover,
  list(pairwise ~ habitat_broad)
)$`pairwise differences of habitat_broad` %>%
  as.tibble()
Table_mod5_cover

Table_mod5_cover_means <- emmeans(
  mod5_cover,
  list(pairwise ~ habitat_broad)
)$`emmeans of habitat_broad` %>%
  as.tibble()
Table_mod4_cover_means


emmeans_mod5_cover_habitat <- cld(
  emmeans(mod5_cover, list(pairwise ~ habitat_broad)),
  Letters = letters
) %>%
  arrange(habitat_broad)
emmeans_mod5_cover_habitat


alien_data_100_cover %>%
  ggplot(aes(habitat_broad, archaeophyte_percent, col = habitat_broad)) +
  geom_boxplot(alpha = 0, lwd = 0.6, outlier.shape = NA) +
  geom_point(
    aes(fill = habitat_broad),
    pch = 21,
    size = 2,
    alpha = 0.4,
    stroke = 0.8,
    position = position_jitterdodge(jitter.width = 0.6, jitter.height = 0)
  ) +
  scale_color_manual(values = col) +
  scale_fill_manual(values = col2) +
  labs(
    y = "Proportion of cover contributed by archaeophytes",
    x = 'Grassland type',
    col = "Grassland type",
    fill = "Grassland type"
  ) +
  # guides(shape = "none") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
# geom_text(data=emmeans_mod5_cover_habitat,
#           aes(x=habitat_broad, y=c(0.18, 0.18, 0.09, 0.02, 0.05, 0.015, 0.015),
#               label=emmeans_mod5_cover_habitat$.group),vjust=0.5, hjust=0.5,
#          size=4, col="black" , position=position_dodge(0))
