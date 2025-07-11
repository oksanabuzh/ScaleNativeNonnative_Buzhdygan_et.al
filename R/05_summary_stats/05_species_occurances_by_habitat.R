# Purpose: Summary statistics for vegetation data 

# Load packages
library(tidyverse)

# Load data ------------------------------------------------

species_categorized <- read_csv("data-raw/database_analysis_categorized.csv") %>% 
  left_join(read_csv("data/header_data_prepared.csv") %>% 
              dplyr::select(series, subplot, habitat_group, habitat_broad),
            by=c("series", "subplot"))
str(species_categorized)
summary(species_categorized)

# how many species in total are there?
species_categorized %>% distinct(species) %>% nrow()
#1082 species in total

# at scales 10 and 100 response is cover and we need p/a 
species_categorized %>% 
  filter(response == "p/a", scale == 100 | scale==10)

#add p/a for 100 and 10 m2
species_data <- species_categorized %>%
  filter(response == "cover", scale == 100 | scale==10) %>%
  mutate(response = "p/a", value = 1) %>% 
  bind_rows(species_categorized)

species_data

total_plots <- read_csv("results/summary_table_habitatas_TableS3_S4.csv") %>% 
  filter(!habitat_broad=="all" & scale==100) %>% 
  dplyr::select(habitat_broad, Plot_number)


sp_data <- species_data %>% 
  filter(scale==100) %>% 
  filter(response=="p/a" ) %>% 
  group_by(scale, species, naturalisation_level, introduction_time, habitat_broad) %>% 
  count() %>% 
  ungroup() %>% 
  left_join(total_plots, by = "habitat_broad") %>%
  mutate(Frequency_total = n / 191 * 100) %>% # total plot number at 100 m2
  mutate(Frequency = n / Plot_number * 100) %>% #  plot number within each habitat at 100 m2
    mutate(habitat_broad=case_when(habitat_broad=="saline" ~ "saline (n=21)",
                   habitat_broad=="complex" ~ "complex (n=2)",
                   habitat_broad=="dry" ~ "dry (n=135)",
                   habitat_broad=="mesic" ~ "mesic (n=13)",
                   habitat_broad=="fringe" ~ "fringe (n=7)",
                   habitat_broad=="wet" ~ "wet (n=2)",
                   habitat_broad=="alpine" ~ "alpine (n=11)"))

summary(sp_data)


habitat_order <- c("saline (n=21)", "complex (n=2)", "dry (n=135)", "wet (n=2)", 
                "mesic (n=13)", "fringe (n=7)", "alpine (n=11)") 
# archaeophytes -----
archaeophytes <- sp_data %>% 
  filter(introduction_time=="archaeophyte") 


## by scale -----

# order species 
arch_species_order <- archaeophytes %>%
  summarize(total_freq = sum(Frequency_total, na.rm = TRUE), .by="species") %>%
  arrange(total_freq) %>%
  pull(species) %>%
  unique() 

arch_Plot <- archaeophytes %>%
  mutate(species = factor(species, levels = arch_species_order)) %>% 
  mutate(
    habitat_broad = fct_relevel(habitat_broad, habitat_order)) %>% 
    ggplot(aes(Frequency, species)) +
  geom_point() +
  geom_vline(xintercept = 0, color = "gray33", linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 1,
             fill="darkcyan", pch=21) +
  geom_errorbarh(aes(xmin = 0, xmax = Frequency),col="darkcyan", 
                 linetype = "solid",
                 position = position_dodge(width = 0.5), height = 0.1) +
  theme_bw() +
  theme(legend.key=element_blank(), 
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 7),
        axis.title = element_text(size = 10),
        plot.margin = margin(2, 2, 10, 2)) +
  facet_wrap(~habitat_broad, nrow = 1) +
  labs(x="Frequency of occurence, %", y="Archaeophyte species")

arch_Plot


archaeophytes %>%
  mutate(species = factor(species, levels = arch_species_order)) %>% 
  filter(is.na(species))

## by habitat -----
arch_species_order2 <- archaeophytes %>%
  #  filter(habitat_broad == "dry") %>%
  arrange(Frequency) %>%
  pull(species)

arch_Plot_habitat <- archaeophytes %>%
  filter(scale == 100) %>% 
  mutate(habitat_broad=fct_relevel(habitat_broad, 
                                   c("saline", "complex", "dry", "wet", 
                                     "mesic", "fringe", "alpine"))) %>% 
  mutate(species = factor(species, levels = arch_species_order)) %>%
  ggplot(aes(Frequency, species))+
  geom_point() +
  geom_vline(xintercept = 0, color = "gray33", linetype = "dashed") +
  geom_point(position = position_dodge(width = dodge_width <- 0.5), size = 1,
             fill="darkcyan", pch=21) +
  geom_errorbarh(aes(xmin = 0, xmax = Frequency),col="darkcyan", 
                 linetype = "solid",
                 position = position_dodge(width = dodge_width), height = 0.1) +
  theme_bw()+
  theme(legend.key=element_blank(), 
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 7),
        axis.title = element_text(size = 10),
        plot.margin = margin(2, 2, 10, 2)) +
  facet_wrap(~habitat_broad, nrow=1)+
  labs(x="Frequency of occurence, %", y="Archaeophyte species")

arch_Plot_habitat

# neophytes -----
neophytes <- sp_data %>% 
  filter(introduction_time=="neophyte") 


# order species by perc at scale = 100
neoph_species_order <- neophytes %>%
  filter(scale == 100) %>%
  arrange(Frequency) %>%
  pull(species)


neoph_Plot <- neophytes %>%
  mutate(species = factor(species, levels = neoph_species_order)) %>%
  ggplot(aes(Frequency, species))+
  geom_point() +
  geom_vline(xintercept = 0, color = "gray33", linetype = "dashed") +
  geom_point(position = position_dodge(width = dodge_width <- 0.5), size = 1,
             fill="slateblue", pch=21) +
  geom_errorbarh(aes(xmin = 0, xmax = Frequency),col="slateblue", 
                 linetype = "solid",
                 position = position_dodge(width = dodge_width), height = 0.1) +
  theme_bw()+
  theme(legend.key=element_blank(), 
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 7),
        axis.title = element_text(size = 10),
        plot.margin = margin(2, 2, 10, 2)) +
  facet_wrap(~scale, nrow=1)+
  scale_x_continuous(breaks = seq(0,13, by=3))+
  labs(x="Frequency of occurence, %", y="Neophyte species")

neoph_Plot

# invasive -----
invasives <- sp_data %>% 
  filter(naturalisation_level=="invasive") 



# order species by perc at scale = 100
inv_species_order <- invasives %>%
  filter(scale == 100) %>%
  arrange(Frequency) %>%
  pull(species)


inv_Plot <- invasives %>%
  mutate(species = factor(species, levels = inv_species_order)) %>%
  ggplot(aes(Frequency, species))+
  geom_point() +
  geom_vline(xintercept = 0, color = "gray33", linetype = "dashed") +
  geom_point(position = position_dodge(width = dodge_width <- 0.5), size = 1,
             fill="brown", pch=21) +
  geom_errorbarh(aes(xmin = 0, xmax = Frequency),col="brown", 
                 linetype = "solid",
                 position = position_dodge(width = dodge_width), height = 0.1) +
  theme_bw()+
  theme(legend.key=element_blank(), 
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 7),
        axis.title = element_text(size = 10),
        plot.margin = margin(2, 2, 10, 2)) +
  facet_wrap(~scale, nrow=1)+
  labs(x="Frequency of occurence, %", y="Invasive species") 

inv_Plot

# combine plots ---------
library(patchwork)


combined_plot <- (arch_Plot / neoph_Plot / inv_Plot) + 
  plot_layout(ncol = 1, heights = c(0.60, 0.26, 0.14)) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.margin = margin(1, 2, 5, 2),
        plot.tag = element_text(face = 'bold', size=10),
        plot.tag.position  = c(0.05, 0.99))


print(combined_plot)


ggsave("results/Species_frequency_occurence.png", combined_plot, width = 8, height = 9, dpi = 150)

