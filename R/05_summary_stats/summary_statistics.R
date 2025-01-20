
# Purpose: Summary statistics for vegetation data 

# Load packages
library(tidyverse)


# Load individual data set ------------------------------------------------

# data from the vegetation surveys
# select only vascular plants and calculate mean value for species that are
# present in multiple layers
species_list <- read_csv("data-raw/database_analysis.csv") |> 
  filter(group == "VP") |>
  summarize(
    value = mean(value, na.rm = TRUE),
    .by = c(dataset, species, series, subplot, scale, response, value)
  )  %>% 
  filter(scale=="100") %>% 
  distinct(species)

# classification of non-native species
non_native_species <- read_csv("data-raw/non_native_species.csv") |> 
 dplyr::select(taxon_tpl, naturalisation_level)



# Add classification to vegetation data -----------------------------------

alien_species <- species_list |> 
  left_join(non_native_species, by = c("species" = "taxon_tpl")) %>% 
  mutate(alien_level=case_when(is.na(naturalisation_level) ~ "native", 
                               .default = naturalisation_level))


alien_species %>% 
  group_by( alien_level) %>% 
  count() %>% 
  mutate(prcnt_out_of_total=n/1233*100,
         prcnt_out_of_alien=n/82*100) %>% 
  rename(species_numner=n) %>% 
  mutate(prcnt_out_of_total=case_when( alien_level=="casual" ~ NA, 
                                       alien_level=="invasive" ~ NA,
                                       alien_level=="naturalised" ~ NA,
                                      .default = prcnt_out_of_total)) %>% 
   mutate(prcnt_out_of_alien=case_when( alien_level=="native" ~ NA,
                                       .default = prcnt_out_of_alien)) 






# Cheek data available per scale -----------------------------------------------------------------



results <- read_csv("data/model_results_summary.csv") 

str(results)

results%>% 
  filter(response_var=="invasive_percent") %>% 
  count(model_id, scale)


results%>% 
  filter(model_id=="climate") %>% 
  count(response_var, scale)

results%>% pull(model_id) %>% unique()       

results%>% 
  filter(model_id=="disturbance") %>% 
  count(response_var, scale)

results%>% 
  filter(model_id=="climate_native") %>% 
  count(response_var, scale)

results%>% 
  filter(model_id=="native") %>% 
  count(response_var, scale)

# 1) bring the variables to the same scale
# 2) check_convergence(mod1, tolerance=1.3)
# 3) 2 models: mod 1 and 2 (below) per scale and response variable
#      response variables  (only based on p/a  data:
# non_native_percent = non_native / total_species,
# invasive_percent = invasive / total_species,
# 
# 4) use optimiser only for invasive_percent
#  control = glmerControl(optimizer = "bobyqa",
#                  optCtrl = list(maxfun = 50000)),



sp <- read_csv("data/database_analysis_summary.csv")

header2 <-  read_csv("data-raw/headers.csv") %>% 
  dplyr::select(series, habitat_broad) %>% 
  group_by(series) %>% 
  summarise(habitat_broad=unique(habitat_broad)) %>% 
  ungroup()

str(header2)

print(sp, n =13)

header_data <-  read_csv("data/header_data_prepared.csv")%>% 
  filter(subplot=="x") %>%
  dplyr::select(-subplot) %>% 
  left_join(header2, by="series")

header_data

header_data %>% 
  filter(is.na(roads))

names(header_data)

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

alien_dat$dataset

str(alien_dat)
names(alien_dat)

summary(alien_dat)

# How many plots contain alien or invasive species
# Alien:

alien_dat |> filter(type == "p_a") %>% 
  filter(total_species>0,
         !scale==0.0001) %>% 
  count(scale, name = "Plot_number") %>% 
  left_join(alien_dat |> filter(type == "p_a") %>% 
              filter(non_native_percent>0) %>% 
              count(scale, name = "non_native"),
            by=c("scale")) %>% 
  mutate(percent_plots=non_native*100/Plot_number)%>% 
  ggplot(aes(y=factor(scale), x=percent_plots)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill="forestgreen")+
  #  coord_flip() +
  theme_bw() +
  labs(y = expression(paste("Grain size, ", m^{2})), x ="% plots with alien species") +
  theme(axis.text.y=element_text(colour = "black", size=9),
        axis.text.x=element_text(colour = "black", size=9),
        axis.title=element_text(size=13),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank()) 


# Invasive:

alien_dat |> filter(type == "p_a") %>% 
  filter(total_species>0,
         !scale==0.0001) %>% 
  count(scale, name = "Plot_number") %>% 
  left_join(alien_dat |> filter(type == "p_a") %>%
              filter(invasive_percent>0) %>% 
              count(scale, name = "invasive"),
            by=c("scale")) %>% 
  mutate(percent_plots=invasive*100/Plot_number) %>% 
  ggplot(aes(y=factor(scale), x=percent_plots)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill="brown")+
  #  coord_flip() +
  theme_bw() +
  labs(y = expression(paste("Grain size, ", m^{2})), x ="% plots with invasive species") +
  theme(axis.text.y=element_text(colour = "black", size=9),
        axis.text.x=element_text(colour = "black", size=9),
        axis.title=element_text(size=13),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank()) 


# Habitat -----------------------------------------------------------------------
names(alien_dat)

alien_dat <- alien_dat %>% 
  mutate(habitat_broad=fct_relevel(habitat_broad, 
                                   c("saline", "complex", "dry", "wet", 
                                     "mesic", "fringe", "alpine")),
         habitat_group=fct_relevel(habitat_group, 
                                   c("saline", "pody", "sandy", "xeric",
                                     "rocky", "meso-xeric", "heats" , "wet", 
                                     "mesic", "fringe", "alpine"))) %>% 
  arrange(habitat_group)


alien_dat


## non_native_percent ----
mod1h<- glm(non_native_percent ~ 
              habitat_broad, # habitat_group ,
            #  (1|dataset),
            weights = total_species,
            family = binomial, 
            data = alien_dat %>%  filter(type == "p_a" &  scale == 100) )

#check_convergence(mod1h)
Anova(mod1h)
summary(mod1h)

library(sjPlot)
# plot_model(mod1h,type = "pred", terms=c("scale","habitat_group"),  show.data=T)



emmeans_m1_habitat <- cld(emmeans(mod1h, list(pairwise ~ habitat_broad)), 
                          Letters = letters) %>% arrange(habitat_broad)
emmeans_m1_habitat





#         saline    complex       dry       wet       mesic        fringe       alpine
col = c("#4e3910", "#CC6600",  "yellow3", "#CC99FF", "#0066FF" ,  "#00B200",  "#006600")
col2 = c("#4e3910", "#CC6600",  "yellow", "#CC99FF", "#0066FF" ,  "#00B200",  "#006600")

ggplot(alien_dat %>%  filter(type == "p_a" &  scale == 100), 
       aes(habitat_broad, non_native_percent, col=habitat_broad))+
  geom_boxplot(alpha=0, lwd=0.6, outlier.shape = NA)+
  geom_point( aes(fill=habitat_broad), pch=21,
              size=2, alpha=0.4, stroke = 0.8, 
              position=position_jitterdodge(jitter.width = 0.6, 
                                            jitter.height = 0)) +
  scale_color_manual(values = col)+
  scale_fill_manual(values = col2)+
  labs(y="Proportion of alien species", x='Grassland type', 
       col="Grassland type",
       fill="Grassland type") +
  # guides(shape = "none") +
  theme_bw()+
  geom_text(data=emmeans_m1_habitat,
            aes(x=habitat_broad, y=c(0.36, 0.31, 0.36, 0.1, 0.18, 0.05, 0.05),
                label=emmeans_m1_habitat$.group),vjust=0.5, hjust=0.5, 
            size=4, col="black" , position=position_dodge(0))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))



## invasive_percent ----

alien_dat$dataset


alien_dat %>%  filter(type == "p_a" &  scale == 100) %>% 
  pull(dataset)


mod2h<- glm(native ~ 
              habitat_broad, # + #  habitat_group +
            #  (1|dataset),
            family = poisson(), 
            data = alien_dat %>%  filter(type == "p_a" &  scale == 100))

# check_convergence(mod2h)
car::Anova(mod2h)
summary(mod2h)


emmeans_m2_habitat <- cld(emmeans(mod2h, list(pairwise ~ habitat_broad)), 
                          Letters = letters) %>% arrange(habitat_broad)
emmeans_m2_habitat





#         saline    complex       dry       wet       mesic        fringe       alpine
col = c("#4e3910", "#CC6600",  "yellow3", "#CC99FF", "#0066FF" ,  "#00B200",  "#006600")
col2 = c("#4e3910", "#CC6600",  "yellow", "#CC99FF", "#0066FF" ,  "#00B200",  "#006600")


ggplot(alien_dat %>%  filter(type == "p_a" &  scale == 100), 
       aes(habitat_broad, native, col=habitat_broad))+
  geom_boxplot(alpha=0, lwd=0.6, outlier.shape = NA)+
  geom_point( aes(fill=habitat_broad), pch=21,
              size=2, alpha=0.4, stroke = 0.8, 
              position=position_jitterdodge(jitter.width = 0.6, 
                                            jitter.height = 0)) +
  scale_color_manual(values = col)+
  scale_fill_manual(values = col2)+
  labs(y="Native species richness", x='Grassland type', 
       col="Grassland type",
       fill="Grassland type") +
  # guides(shape = "none") +
  theme_bw()+
  geom_text(data=emmeans_m2_habitat,aes(x=habitat_broad, 
                                        y=c(42, 40, 97, 50, 90, 85, 60),
                                        label=emmeans_m2_habitat$.group),vjust=0.5, hjust=0.7, 
            size=4, col="black" , position=position_dodge(0))




            

