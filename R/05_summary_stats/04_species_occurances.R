# Purpose: Summary statistics for vegetation data 

# Load packages
library(tidyverse)

# Load data ------------------------------------------------

species_categorized <- read_csv("data-raw/database_analysis_categorized.csv")  
str(species_categorized)

# how many species in total are there?
species_categorized %>% distinct(species) %>% nrow()
#1082 species in total

# at scales 10 and 100 response is cover and we need p/a 
species_categorized %>% 
  filter(response == "p/a", scale == 100 | scale==10)

#add p/a for 100 and 10 m2
species_data <- species_data %>%
  filter(response == "cover", scale == 100 | scale==10) %>%
  mutate(response = "p/a", value = 1) %>% 
  bind_rows(species_categorized)

species_data

total_plots <- read_csv("results/summary_table_habitatas_TableS3_S4.csv") %>% 
  filter(habitat_broad=="all") %>% 
  dplyr::select(scale, Plot_number)


sp_data <- species_data %>% 
  filter(!scale==0.0001) %>% 
  filter(response=="p/a" ) %>% 
  group_by(scale, species, naturalisation_level, introduction_time) %>% 
  count() %>% 
  ungroup() %>% 
  left_join(total_plots, by = "scale") %>%
  mutate(Frequency = n / Plot_number * 100) 

# archaeophytes -----
archaeophytes <- sp_data %>% 
  filter(introduction_time=="archaeophyte") 


# order species by perc at scale = 100
arch_species_order <- archaeophytes %>%
  filter(scale == 100) %>%
  arrange(Frequency) %>%
  pull(species)


arch_Plot <- archaeophytes %>%
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
  facet_wrap(~scale, nrow=1)+
  labs(x="Frequency of occurence, %", y="Archaeophyte species")

arch_Plot

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

