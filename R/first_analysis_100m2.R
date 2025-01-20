library(tidyverse)
library(lme4)
library(performance)
library(car)
library(piecewiseSEM)

library(emmeans)
library(multcomp)



# Check results from the model -------


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


header_data <-  read_csv("data/header_data_prepared.csv")%>% 
  filter(subplot=="x") %>%
  dplyr::select(-subplot) %>% 
  left_join(header2, by="series")




header_data

print(sp, n =13)

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



# Cheek data available per scale -----------------------------------------------------------------

# How many plots contain alien or invasive species


# Alien:

alien_dat |> filter(type == "p_a") %>% 
  filter(total_species>0) %>% 
  count(scale, name = "Plot_number") %>% 
  left_join(alien_dat |> filter(type == "p_a") %>% 
              filter(non_native_percent>0) %>% 
              count(scale, name = "non_native"),
            by=c("scale")) %>% 
  mutate(percent_plots=non_native*100/Plot_number)%>% 
  ggplot(aes(y=factor(scale), x=percent_plots)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill="forestgreen")+
  #  coord_flip() +
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
  filter(total_species>0) %>% 
  count(scale, name = "Plot_number") %>% 
  left_join(alien_dat |> filter(type == "p_a") %>%
              filter(invasive_percent>0) %>% 
              count(scale, name = "invasive"),
            by=c("scale")) %>% 
  mutate(percent_plots=invasive*100/Plot_number) %>% 
  ggplot(aes(y=factor(scale), x=percent_plots)) + 
  geom_bar(position="stack", stat="identity", colour = "black", fill="brown")+
#  coord_flip() +
  labs(y = expression(paste("Grain size, ", m^{2})), x ="% plots with invasive species") +
 theme(axis.text.y=element_text(colour = "black", size=9),
        axis.text.x=element_text(colour = "black", size=9),
        axis.title=element_text(size=13),
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks =  element_line(colour = "black"),
        axis.ticks.y = element_blank()) 

  
  



# Drivers -----------------------------------------------------------------

mod22 <- MASS::glmmPQL(non_native_percent ~ 
                pca1_clima + 
                pH + 
                microrelief +  
                heat_index + 
                cover_litter +   
                cover_herbs_sum + 
                cover_grv_stone + 
                built_up + 
                grazing_intencity +  mowing +  
                abandonment,
              random = ~ 1|dataset,
              weights = total_species,
              family = quasibinomial,
              data = alien_dat |> filter(type == "p_a" & 
                                           scale == 100))


MuMIn::r.squaredGLMM(mod22)["theoretical", ] 
#sessionInfo()


Anova(mod22)
summary(mod22)


mod1 <- glmer(non_native_percent ~ #non_native_percent ~ 
              pca1_clima + 
              pH + 
                microrelief +  
              heat_index + 
              cover_litter + 
              cover_herbs_sum + 
            cover_grv_stone + 
            built_up + 
             grazing_intencity +  mowing +  
             abandonment  +  
                (1|dataset),
  weights = total_species,
  family = binomial, 
 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
  data = alien_dat %>%  filter(type == "p_a" & scale == 100))


MuMIn::r.squaredGLMM(mod1)["theoretical", ] 

# allFit(mod1, maxfun = 1e5)

check_convergence(mod1, tolerance=1.3)   

check_convergence(mod1) 
check_overdispersion(mod1)

check_collinearity(mod1)

Anova(mod1)
summary(mod1)

piecewiseSEM::coefs(mod1) 

#################################################

mod2 <- glmer(invasive_percent/non_native_percent ~ 
                roads + 
                Disturbance.Frequency +
                Disturbance.Severity +
                (1|series),
            #  weights = total_species,
              family = binomial, 
              control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
              data = alien_dat %>%  filter(type == "p_a" & scale == 0.0001))



mod2 <- glmer(non_native_percent ~ #invasive_percent ~
                roads + 
                Disturbance.Frequency +
                Disturbance.Severity +
                (1|series),
              weights = total_species,
              family = binomial, 
             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)),
              data = alien_dat %>%  filter(type == "p_a" & scale == 0.0001))


#allFit(mod1, maxfun = 1e5)
#summary(allFit(mod1, maxfun = 1e5))
check_convergence(mod2)

check_overdispersion(mod2)
check_collinearity(mod2)

Anova(mod2)
summary(mod2)
piecewiseSEM::coefs(mod2)


##########################################
mod2_ud <- glm(invasive_percent ~ 
                           roads + 
                           Disturbance.Frequency +
                           Disturbance.Severity,
                         weights = total_species,
                         family = quasibinomial,
                         data = alien_dat |> filter(type == "p_a" & 
                                                      scale == 1))



mod2_ud <- MASS::glmmPQL(non_native_percent ~ 
                           roads + 
                           Disturbance.Frequency +
                           Disturbance.Severity ,
                         random = ~ 1|series,
                         weights = total_species,
                         family = quasibinomial,
                         data = alien_dat |> filter(type == "p_a" & 
                                                      scale == 0.0001))


MuMIn::r.squaredGLMM(mod2_ud)["theoretical", ] 

car::Anova(mod2_ud)
piecewiseSEM::coefs(mod2_ud)

summary(mod2_ud)

#####
mod2_ud <- MASS::glmmPQL(non_native_percent ~ 
                           roads + # footprint_values + 
                           Disturbance.Frequency +
                           Disturbance.Severity,
                         random = ~ 1|series,
                         weights = total_species,
                         family = quasibinomial(link = "logit"),
                         data = alien_dat |> filter(type == "p_a" & 
                                                      scale == 0.0001
                         )
)

car::Anova(mod2_ud)



piecewiseSEM::coefs(mod2_ud)


##########

# Species richness -------------------------------------------------------------



names(alien_dat)

## non_native_percent -----

### native SR ----

mod1s <- MASS::glmmPQL(non_native_percent ~ 
                             pca1_clima +
                           native,
                         random = ~ 1|series,
                         weights = total_species,
                         family = quasibinomial,
                         data = alien_dat |> filter(type == "p_a" & 
                                                      scale == 0.0001
                         )
)

car::Anova(mod1s)
summary(mod1s)



mod1s<- glmer(non_native_percent ~ 
             pca1_clima +
                native +
                (1|series),
              weights = total_species,
              family = binomial, 
              data = alien_dat %>%  filter(type == "p_a" &  scale == 0.0001))
         #     filter(!non_native_percent==0) )


check_convergence(mod1s)
check_overdispersion(mod1s)

Anova(mod1s)
summary(mod1s)

# library(sjPlot)
plot_model(mod1s,type = "pred", terms=c("native"),  show.data=T)


mod1s_pred <- get_model_data(mod1s,type = "pred", terms="native[0:94, by=0.001]")

ggplot(mod1s_pred, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  # geom_boxplot(alpha=0, lwd=0.6, outlier.shape = NA)+
  geom_point(data=alien_dat %>%  filter(type == "p_a" &  scale == 100) %>% 
               filter(!non_native_percent==0), 
             aes(native, non_native_percent),
             col="forestgreen", size=2, alpha=0.6) +
  labs(y="% of alien species", x='Number of native species') +
      # size='Climate gradient (PC)') +
  # guides(shape = "none") +
  theme_bw()+
  geom_line(linetype=1, linewidth=1) 


### native cover ----

a <- alien_dat %>%  filter(type == "cover" &  scale == 100) %>% 
  select (series, native)

b <- alien_dat %>%  filter(type == "p_a"&  scale == 100) %>% 
  select (dataset, series, pca1_clima, non_native_percent, invasive_percent, total_species) %>% 
  left_join(a, by=c("series"))


mod2s<- glmer(non_native_percent ~ 
              #  pca1_clima +
                native +
                (1|dataset),
              weights = total_species,
              family = binomial, 
              data = b #%>%filter(!non_native_percent==0)
              ) 

check_convergence(mod2s)
Anova(mod2s)
summary(mod2s)

# library(sjPlot)
plot_model(mod2s,type = "pred", terms=c("native"),  show.data=T)


mod2s_pred <- get_model_data(mod2s,type = "pred", terms="native[0:155, by=0.001]")

ggplot(mod2s_pred, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  # geom_boxplot(alpha=0, lwd=0.6, outlier.shape = NA)+
  geom_point(data=b %>% 
               filter(!non_native_percent==0), 
             aes(native, non_native_percent),
             col="forestgreen", size=2, alpha=0.6) +
  labs(y="% of alien species", x='Cover of native species') +
  # size='Climate gradient (PC)') +
  # guides(shape = "none") +
  theme_bw()+
  geom_line(linetype=1, linewidth=1) 


## invasive_percent ----


### native SR ----
mod1s_inv<- glmer(invasive_percent ~ 
               # pca1_clima +
                native +
                (1|dataset),
              weights = total_species,
              family = binomial, 
              data = alien_dat %>%  filter(type == "p_a" &  scale == 100) %>% 
                filter(!invasive_percent==0) ) 

check_convergence(mod1s_inv)
Anova(mod1s_inv)
summary(mod1s_inv)

# library(sjPlot)
plot_model(mod1s_inv,type = "pred", terms=c("native"),  show.data=T)

mod1s_inv_pred <- get_model_data(mod1s_inv,type = "pred", terms="native[5:80, by=0.001]")

ggplot(mod1s_inv_pred, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  # geom_boxplot(alpha=0, lwd=0.6, outlier.shape = NA)+
  geom_point(data=alien_dat %>%  filter(type == "p_a" &  scale == 100) %>% 
               filter(!invasive_percent==0), 
             aes(native, invasive_percent),
             col="brown", size=2, alpha=0.6) +
  labs(y="% of invasive species", x='Number of native species') +
  # size='Climate gradient (PC)') +
  # guides(shape = "none") +
  theme_bw()+
  geom_line(linetype=1, linewidth=1) 


### native cover ----

a <- alien_dat %>%  filter(type == "cover" &  scale == 100) %>% 
  select (series, native)

b <- alien_dat %>%  filter(type == "p_a"&  scale == 100) %>% 
  select (dataset, series, pca1_clima, non_native_percent, invasive_percent, total_species) %>% 
  left_join(a, by=c("series"))


mod2s_inv<- glmer(invasive_percent ~ 
              #  pca1_clima +
                native +
                (1|dataset),
              weights = total_species,
              family = binomial, 
              data = b %>% 
                filter(!invasive_percent==0))


check_convergence(mod2s_inv)
Anova(mod2s_inv)
summary(mod2s_inv)

# library(sjPlot)
plot_model(mod2s_inv,type = "pred", terms=c("native"),  show.data=T)


mod2s_inv_pred <- get_model_data(mod2s_inv,type = "pred", terms="native[2:155, by=0.001]")


ggplot(mod2s_inv_pred, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  # geom_boxplot(alpha=0, lwd=0.6, outlier.shape = NA)+
  geom_point(data=b %>% 
               filter(!invasive_percent==0), 
             aes(native, invasive_percent),
             col="brown", size=2, alpha=0.6) +
  labs(y="% of invasive species", x='Cover of native species') +
  # size='Climate gradient (PC)') +
  # guides(shape = "none") +
  theme_bw()+
  geom_line(linetype=1, linewidth=1) 


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
col = c("#4e3910", "#CC6600",  "#e3c28b", "#CC99FF", "#0066FF" ,  "#00B200",  "#006600")


ggplot(alien_dat %>%  filter(type == "p_a" &  scale == 100), 
       aes(habitat_broad, non_native_percent, col=habitat_broad))+
  geom_boxplot(alpha=0, lwd=0.6, outlier.shape = NA)+
  geom_point( size=2, alpha=0.4, stroke = 0.8, 
              position=position_jitterdodge(jitter.width = 0.6, 
                                            jitter.height = 0)) +
  scale_color_manual(values = col)+
  labs(y="Proportion of alien species", x='Climate gradient (PC)', col="Grassland type") +
  # guides(shape = "none") +
  theme_bw()+
  geom_text(data=emmeans_m1_habitat,aes(x=habitat_broad, y=c(0.35, 0.27, 0.24, 0.1, 0.17, 0.05, 0.05),
                                        label=emmeans_m1_habitat$.group),vjust=0.5, hjust=0.5, 
            size=4, col="black" , position=position_dodge(0))



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
Anova(mod2h)
summary(mod2h)


emmeans_m2_habitat <- cld(emmeans(mod2h, list(pairwise ~ habitat_broad)), 
                          Letters = letters) %>% arrange(habitat_broad)
emmeans_m2_habitat





#         saline    complex       dry       wet       mesic        fringe       alpine
col = c("#4e3910", "#CC6600",  "#e3c28b", "#CC99FF", "#0066FF" ,  "#00B200",  "#006600")


ggplot(alien_dat %>%  filter(type == "p_a" &  scale == 100), 
       aes(habitat_broad, native, col=habitat_broad))+
  geom_boxplot(alpha=0, lwd=0.6, outlier.shape = NA)+
  geom_point( size=2, alpha=0.4, stroke = 0.8, 
              position=position_jitterdodge(jitter.width = 0.6, 
                                            jitter.height = 0)) +
  scale_color_manual(values = col)+
  labs(y="Native species richness", x='Climate gradient (PC)', col="Grassland type") +
  # guides(shape = "none") +
  theme_bw()+
  geom_text(data=emmeans_m2_habitat,aes(x=habitat_broad, y=c(60, 40, 100, 90, 100, 50, 55),
                                        label=emmeans_m2_habitat$.group),vjust=0.5, hjust=0.7, 
            size=4, col="black" , position=position_dodge(0))


