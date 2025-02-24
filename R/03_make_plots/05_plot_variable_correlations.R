library(tidyverse)
library(lme4)
library(performance)
library(car)
library(piecewiseSEM)




sp <- read_csv("data/database_analysis_summary.csv")
header_data <-  read_csv("data/header_data_prepared.csv")%>% 
  filter(subplot=="x") %>%
 dplyr::select(-subplot)

disturb_data <- read_csv("data-raw/Disturbn_commun_mean.csv") %>% 
  filter(scale==100) %>%
  dplyr::select(-scale)


climate_pc <- read_csv("data/Clima_PC.csv") %>% 
  group_by(series) %>% 
  summarise(pca1_clima=mean(pca1_clima)) %>% 
  ungroup()


sp2 <- sp |>
  mutate(
    non_native_percent = non_native / total_species,
    invasive_percent = invasive / total_species,
    p_a_non_native = ifelse(non_native > 0, 1, 0),
    p_a_invasive = ifelse(invasive > 0, 1, 0)
  )


sp3 <- left_join(sp2, climate_pc,
                 by = "series")

print(sp3, n =13)


alien_dat <- sp3 |>
  left_join(header_data, by = c("series")) %>% 
  left_join(disturb_data, by = c("series")) %>% 
  mutate(dataset=factor(dataset),
         cover_grv_stone=(cover_gravel + cover_stones)/100,
         roads=roads/100,
         built_up=built_up_2km/100,
         cover_litter= cover_litter/100,
         cover_herbs_sum=cover_herbs_sum/100) 


print(alien_dat, n =13)

str(alien_dat)
names(alien_dat)

summary(alien_dat)


variabl <- alien_dat %>% 
  filter(type == "p_a" & scale == 100) %>% 
 dplyr::select(series, pca1_clima , altitude, # lon,
         pH , Corg,
         heat_index, #inclination, 
         microrelief ,  
         cover_grv_stone,  # cover_gravel, cover_stones, 
         cover_herbs_sum,  cover_litter,  cover_shrub_total,
         grazing_intencity , mowing ,  abandonment , # burning ,  
         #  economic_use, 
         built_up,  roads , # footprint_values , 
         Disturbance.Frequency , Disturbance.Severity) %>% 
  group_by(series) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  ungroup() %>% 
  rename("Altitude"= "altitude", 
                      "Climate PC"= "pca1_clima", 
                      "Soil pH"= "pH", 
                      "Soil humus"= "Corg", 
                      "Microrelief"= "microrelief", 
                      "Gravel & stone cover"= "cover_grv_stone",
                      "Herb cover"= "cover_herbs_sum",
                      "Shrub cover"= "cover_shrub_total",
                      "Litter cover"= "cover_litter", 
                      "Grazing"= "grazing_intencity", 
                      "Mowing"= "mowing", 
                      "Abandonment"= "abandonment", 
                      "Heat stress"= "heat_index",
                      "Urban built-up"= "built_up",  
                      "Road density"= "roads", 
                      "Disturbance frequency"= "Disturbance.Frequency", 
                      "Disturbance severity"= "Disturbance.Severity"
         )


str(variabl)

corl1 <- round(cor(variabl %>% dplyr::select(-series),
                   method = c("pearson"), use = "pairwise.complete.obs"), 2)

corl1

library(ggcorrplot)
ggcorrplot(corl1,
           hc.order = F, type = "lower",
           lab = TRUE, lab_size = 4,
           colors = c("red", "white", "blue"))

