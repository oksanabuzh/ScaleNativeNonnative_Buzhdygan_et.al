# Purpose: Summary statistics for vegetation data 

# Load packages
library(tidyverse)


# Load data ------------------------------------------------
sp_data <- read_csv("data/database_analysis_summary.csv") %>% 
  mutate(
    non_native_percent = non_native / total_species,
    invasive_percent = invasive / total_species,
    archaeophyte_percent =archaeophyte/total_species,
    neophyte  = neophyte/total_species,
    p_a_non_native = ifelse(non_native > 0, 1, 0),
    p_a_invasive = ifelse(invasive > 0, 1, 0),
    p_a_neophyte = ifelse(neophyte > 0, 1, 0)) %>% 
  left_join(climate_pc, by = "series")


# How many plots contain aliens?
## Number of plots where aliens occur:

plot_proportion <- sp_data %>%  
  filter(type == "p_a") %>% 
  filter(total_species>0,
         !scale==0.0001) %>% 
  count(scale, name = "Plot_number") %>% 
  left_join(sp_data |> filter(type == "p_a") %>% 
              filter(non_native>0) %>% 
              count(scale, name = "non_native"),
            by=c("scale")) %>% 
  mutate(percent_plots_alien=non_native*100/Plot_number)%>% 
  left_join(sp_data |> filter(type == "p_a") %>% 
              filter(invasive>0) %>% 
              count(scale, name = "invasive"),
            by=c("scale")) %>% 
  mutate(percent_plots_invasive=invasive*100/Plot_number)%>% 
  left_join(sp_data |> filter(type == "p_a") %>% 
              filter(archaeophyte>0) %>% 
              count(scale, name = "archaeophyte"),
            by=c("scale")) %>% 
  mutate(percent_plots_archaeophyte=archaeophyte*100/Plot_number) %>% 
  left_join(sp_data |> filter(type == "p_a") %>% 
              filter(neophyte>0) %>% 
              count(scale, name = "neophyte"),
            by=c("scale")) %>% 
  mutate(percent_plots_neophyte=neophyte*100/Plot_number) 


plot_proportion

write.csv(plot_proportion, "results/plot_proportions_TableS3.csv")

#  Plots:
# Alien 
plot_proportion  %>% 
  ggplot(aes(y=factor(scale), x=percent_plots_alien)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill="forestgreen")+
  geom_text(aes(label=non_native), hjust=-0.1, size=3) +
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
        axis.ticks.y = element_blank()) +
  xlim(-1, 85)




# Invasive:
plot_proportion  %>% 
  ggplot(aes(y=factor(scale), x=percent_plots_invasive)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill="brown")+
  geom_text(aes(label=invasive), hjust=-0.1, size=3) +
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
        axis.ticks.y = element_blank()) +
  xlim(-1, 47)


# Neophytes:

plot_proportion  %>% 
  ggplot(aes(y=factor(scale), x=percent_plots_neophyte)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill="slateblue")+
  geom_text(aes(label=neophyte), hjust=-0.1, size=3) +
  #  coord_flip() +
  theme_bw() +
  labs(y = expression(paste("Grain size, ", m^{2})), x ="% plots with neophytes") +
  theme(axis.text.y=element_text(colour = "black", size=9),
        axis.text.x=element_text(colour = "black", size=9),
        axis.title=element_text(size=13),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank()) +
  xlim(-1, 45)


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



## native SR ----

alien_dat$dataset


alien_dat %>%  filter(type == "p_a" &  scale == 100) %>% 
  pull(dataset)


mod2h<- glm(native ~ 
              habitat_broad, # +   habitat_group +
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




            

