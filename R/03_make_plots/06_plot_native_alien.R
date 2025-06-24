# Purpose: Plot relationship between alien and native richness and cover

# Load necessary libraries
library(tidyverse)
library(lme4)
library(performance)
library(car)
library(piecewiseSEM)
library(emmeans)
library(multcomp)
 


sjPlot::set_theme(base = theme_bw(),
          axis.textsize.x = 0.9, axis.textsize.y = 0.9, axis.textcolor = "black",
          axis.title.color = "black", axis.title.size = 1.2,
          geom.linetype = 1) #legend.pos = "None", 


sp <- read_csv("data/database_analysis_summary.csv")

print(sp, n =13)


climate_pc <- read_csv("data/Clima_PC.csv") %>% 
  group_by(series) %>% 
  summarise(pca1_clima=mean(pca1_clima)) %>% 
  ungroup()

header_data <- read_csv("data/header_data_prepared.csv")

sp2 <- sp |>
  mutate(
    non_native_percent = non_native / total_species,
    invasive_percent = invasive / total_species,
    neophyte_percent = neophyte / total_species,
    p_a_non_native = ifelse(non_native > 0, 1, 0),
    p_a_invasive = ifelse(invasive > 0, 1, 0),
    p_a_neophyte = ifelse(neophyte > 0, 1, 0)
  )


alien_dat <- left_join(sp2, climate_pc,
  by = "series") %>% 
  left_join(header_data, by = c("series", "subplot"))

print(alien_dat, n =13)
str(alien_dat)
names(alien_dat)

summary(alien_dat)

# Scale -------------------------------------------------------------

## Alien ----

# data points:

alien_dat %>% 
  filter(type == "p_a") %>% 
  filter(!scale==0.0001) %>% 
  ggplot(aes(log(scale), non_native_percent)) +
  geom_point(col="forestgreen", size=0.7, alpha=0.5,
             position = position_jitter(height = 0, width = 0.5)) +
  # scale_x_log10() +
  stat_summary(fun.data = mean_se, geom='errorbar', width=0.5, color='black')+
  stat_summary(fun.y = mean, geom="point", size=2, , color='black') + 
  stat_summary(geom = "line", fun = mean, col="black") +
  labs(y="Proportion of alien species", 
       x=expression(paste('Grain size, ', m^{2}))) +
  # guides(shape = "none") +
  theme(axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=8),
        axis.title=element_text(size=11), 
        legend.text = element_text(size=10)) +
  scale_x_continuous(breaks=c(-6.907755, -4.605170, -2.302585,  0.000000,  2.302585,  4.605170),
                     labels=c("0.001", "0.01", "0.1", "1", "10", "100"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))




# mean +- Variation

alien_dat %>% 
  filter(type == "p_a") %>% 
  filter(!scale==0.0001) %>% 
ggplot(aes(log(scale), non_native_percent)) +
 # quantiles:
  # stat_summary(
    # fun = mean, fun.min = ~quantile(.x, probs = .05), fun.max = ~quantile(.x, probs = .95), 
    # color = "red") +
  # SD:
# stat_summary( 
#    fun.y = mean, fun.ymin = function(x) mean(x) - sd(x), fun.ymax = function(x) mean(x) + sd(x),
#     col="forestgreen") +
  # 95% CI
  # stat_summary(fun.data = mean_sdl, col="blue") +
  # SE
  stat_summary(fun.data = mean_se, geom='errorbar', width=0.5, color='forestgreen')+
  stat_summary(fun.y = mean, geom="point", size=2, , color='forestgreen') + 
    stat_summary(geom = "line", fun = mean, col="forestgreen") +
 # geom_point(col="forestgreen", size=2, alpha=0.6) +
 # scale_x_log10() +
  labs(y="Proportion of alien species", 
       x=expression(paste('Grain size, ', m^{2}))) +
  # guides(shape = "none") +
  theme(axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=8),
        axis.title=element_text(size=11), 
        legend.text = element_text(size=10)) +
  scale_x_continuous(breaks=c(-6.907755, -4.605170, -2.302585,  0.000000,  2.302585,  4.605170),
                     labels=c("0.001", "0.01", "0.1", "1", "10", "100"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))


# SD:

alien_dat %>% 
  filter(type == "p_a") %>% 
  filter(!scale==0.0001) %>% 
  ggplot(aes(log(scale), non_native_percent)) +
  # quantiles:
  # stat_summary(
  # fun = mean, fun.min = ~quantile(.x, probs = .05), fun.max = ~quantile(.x, probs = .95), 
  # color = "red") +
  # SD:
   stat_summary( 
      fun.y = mean, fun.ymin = function(x) mean(x) - sd(x), fun.ymax = function(x) mean(x) + sd(x),
       col="forestgreen") +
  # 95% CI
  # stat_summary(fun.data = mean_sdl, col="blue") +
  # SE
#stat_summary(fun.data = mean_se, geom='errorbar', width=0.5, color='forestgreen')+
  stat_summary(fun.y = mean, geom="point", size=2, , color='forestgreen') + 
  stat_summary(geom = "line", fun = mean, col="forestgreen") +
  # geom_point(col="forestgreen", size=2, alpha=0.6) +
  # scale_x_log10() +
  labs(y="Proportion of alien species", 
       x=expression(paste('Grain size, ', m^{2}))) +
  # guides(shape = "none") +
  theme(axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=8),
        axis.title=element_text(size=11), 
        legend.text = element_text(size=10)) +
  scale_x_continuous(breaks=c(-6.907755, -4.605170, -2.302585,  0.000000,  2.302585,  4.605170),
                     labels=c("0.001", "0.01", "0.1", "1", "10", "100"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

## Invasive ----



# data points:
alien_dat %>% 
  filter(!scale %in% c(0.0001)) %>% 
  filter(type == "p_a") %>% 
  ggplot(aes(log(scale), invasive_percent)) +
  geom_point(col="brown", size=0.7, alpha=0.5,
             position = position_jitter(height = 0, width = 0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', width=0.5, color='black')+
  stat_summary(fun.y = mean, geom="point", size=2,  color='black') + 
  stat_summary(geom = "line", fun = mean, col="black") +
   # scale_x_log10() +
  labs(y="Proportion of invasive species", 
       x=expression(paste('Grain size, ', m^{2}))) +
  # guides(shape = "none") +
  theme(axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=8),
        axis.title=element_text(size=11), 
        legend.text = element_text(size=10)) +
  scale_x_continuous(breaks=c(-6.907755, -4.605170, -2.302585,  0.000000,  2.302585,  4.605170),
                     labels=c("0.001", "0.01", "0.1", "1", "10", "100"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# mean +- Variation
alien_dat %>% 
  filter(!scale==0.0001) %>% 
  filter(type == "p_a") %>% 
  ggplot(aes(log(scale), invasive_percent)) +
  # SD:
#  stat_summary( 
 #   fun.y = mean, fun.ymin = function(x) mean(x) - sd(x), fun.ymax = function(x) mean(x) + sd(x),
 # col="brown") +
#  stat_summary(fun.data = mean_se, geom='errorbar', width=0.5, , col="brown")+
  
  stat_summary(fun.data = mean_se, geom='errorbar', width=0.5, color='brown')+
  stat_summary(fun.y = mean, geom="point", size=2,  color='brown') + 
  stat_summary(geom = "line", fun = mean, col="brown") +
  # scale_x_log10() +
  labs(y="Proportion of invasive species", 
       x=expression(paste('Grain size, ', m^{2}))) +
  # guides(shape = "none") +
  theme(axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=8),
        axis.title=element_text(size=11), 
        legend.text = element_text(size=10)) +
  scale_x_continuous(breaks=c(-6.907755, -4.605170, -2.302585,  0.000000,  2.302585,  4.605170),
                     labels=c("0.001", "0.01", "0.1", "1", "10", "100"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))

# SD

# mean +- Variation
alien_dat %>% 
  filter(!scale==0.0001) %>% 
  filter(type == "p_a") %>% 
  ggplot(aes(log(scale), invasive_percent)) +
  # SD:
  stat_summary( 
    fun.y = mean, fun.ymin = function(x) mean(x) - sd(x), fun.ymax = function(x) mean(x) + sd(x),
    col="brown") +
 # stat_summary(fun.data = mean_se, geom='errorbar', width=0.5, , col="brown")+
  
#   stat_summary(fun.data = mean_se, geom='errorbar', width=0.5, color='brown')+
#  stat_summary(fun.y = mean, geom="point", size=2,  color='brown') + 
  stat_summary(geom = "line", fun = mean, col="brown") +
  # scale_x_log10() +
  labs(y="Proportion of invasive species", 
       x=expression(paste('Grain size, ', m^{2}))) +
  # guides(shape = "none") +
  theme(axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=8),
        axis.title=element_text(size=11), 
        legend.text = element_text(size=10)) +
  scale_x_continuous(breaks=c(-6.907755, -4.605170, -2.302585,  0.000000,  2.302585,  4.605170),
                     labels=c("0.001", "0.01", "0.1", "1", "10", "100"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))



#------------------------------------------------------------------------------#

## Invasive ----



# data points:
alien_dat %>% 
  filter(!scale %in% c(0.0001)) %>% 
  filter(type == "p_a") %>% 
  ggplot(aes(log(scale), neophyte_percent)) +
  geom_point(col="slateblue", size=0.7, alpha=0.5,
             position = position_jitter(height = 0, width = 0.5)) +
  stat_summary(fun.data = mean_se, geom='errorbar', width=0.5, color='black')+
  stat_summary(fun.y = mean, geom="point", size=2,  color='black') + 
  stat_summary(geom = "line", fun = mean, col="black") +
  # scale_x_log10() +
  labs(y="Proportion of neophytes", 
       x=expression(paste('Grain size, ', m^{2}))) +
  # guides(shape = "none") +
  theme(axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=8),
        axis.title=element_text(size=11), 
        legend.text = element_text(size=10)) +
  scale_x_continuous(breaks=c(-6.907755, -4.605170, -2.302585,  0.000000,  2.302585,  4.605170),
                     labels=c("0.001", "0.01", "0.1", "1", "10", "100"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# mean +- Variation
alien_dat %>% 
  filter(!scale==0.0001) %>% 
  filter(type == "p_a") %>% 
  ggplot(aes(log(scale), neophyte_percent)) +
  # SD:
  #  stat_summary( 
  #   fun.y = mean, fun.ymin = function(x) mean(x) - sd(x), fun.ymax = function(x) mean(x) + sd(x),
  # col="brown") +
  #  stat_summary(fun.data = mean_se, geom='errorbar', width=0.5, , col="brown")+
  
  stat_summary(fun.data = mean_se, geom='errorbar', width=0.5, color='slateblue')+
  stat_summary(fun.y = mean, geom="point", size=2,  color='slateblue') + 
  stat_summary(geom = "line", fun = mean, col="slateblue") +
  # scale_x_log10() +
  labs(y="Proportion of neophytes", 
       x=expression(paste('Grain size, ', m^{2}))) +
  # guides(shape = "none") +
  theme(axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=8),
        axis.title=element_text(size=11), 
        legend.text = element_text(size=10)) +
  scale_x_continuous(breaks=c(-6.907755, -4.605170, -2.302585,  0.000000,  2.302585,  4.605170),
                     labels=c("0.001", "0.01", "0.1", "1", "10", "100"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))

# SD

# mean +- Variation
alien_dat %>% 
  filter(!scale==0.0001) %>% 
  filter(type == "p_a") %>% 
  ggplot(aes(log(scale), neophyte_percent)) +
  # SD:
  stat_summary( 
    fun.y = mean, fun.ymin = function(x) mean(x) - sd(x), fun.ymax = function(x) mean(x) + sd(x),
    col="slateblue") +
  # stat_summary(fun.data = mean_se, geom='errorbar', width=0.5, , col="brown")+
  
  #   stat_summary(fun.data = mean_se, geom='errorbar', width=0.5, color='brown')+
  #  stat_summary(fun.y = mean, geom="point", size=2,  color='brown') + 
  stat_summary(geom = "line", fun = mean, col="slateblue") +
  # scale_x_log10() +
  labs(y="Proportion of neophytes", 
       x=expression(paste('Grain size, ', m^{2}))) +
  # guides(shape = "none") +
  theme(axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=8),
        axis.title=element_text(size=11), 
        legend.text = element_text(size=10)) +
  scale_x_continuous(breaks=c(-6.907755, -4.605170, -2.302585,  0.000000,  2.302585,  4.605170),
                     labels=c("0.001", "0.01", "0.1", "1", "10", "100"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))

# Species richness -------------------------------------------------------------



names(alien_dat)

## non_native_percent -----

### native SR ----
mod1s<- glmer(non_native_percent ~ 
              pca1_clima +
                native +
                (1|dataset),
              weights = total_species,
              family = binomial, 
              data = alien_dat %>%  filter(type == "p_a" &  scale == 100) %>% 
                filter(!non_native_percent==0)) 

check_convergence(mod1s)
Anova(mod1s)
summary(mod1s)
check_overdispersion(mod1s)

library(sjPlot)
plot_model(mod1s,type = "pred", terms=c("native"),  show.data=T)


mod1s_pred <- get_model_data(mod1s,type = "pred", terms="native[0:94, by=0.001]")


ggplot(mod1s_pred, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  # geom_boxplot(alpha=0, lwd=0.6, outlier.shape = NA)+
  geom_point(data=alien_dat %>%  filter(type == "p_a" &  scale == 100) %>% 
               filter(!non_native_percent==0), 
             aes(native, non_native_percent),
             col="forestgreen", size=2, alpha=0.6) +
  labs( y ="Proportion of alien species", x='Native species richness (SR)') +
  theme(axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=8),
        axis.title=element_text(size=11), 
        legend.text = element_text(size=10)) +
 # theme_bw()+
  geom_line(linetype=1, linewidth=1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))


### native cover ----

cover_data <- alien_dat %>%  filter(type == "p_a"&  scale == 100) %>% 
  dplyr::select (dataset, series, pca1_clima, non_native_percent, 
                 invasive_percent, total_species) %>% 
  left_join(a, by=c("series"))


mod2s<- glmer(non_native_percent ~ 
              #  pca1_clima +
                native +
                (1|dataset),
              weights = total_species,
              family = binomial, 
              data = cover_data 
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
  labs(y="Proportion of alien species", x='Cover of native species') +
  # size='Climate gradient (PC)') +
  # guides(shape = "none") +
  theme_bw()+
  geom_line(linetype=1, linewidth=1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))


## invasive_percent ----

mod1s_inv<- glmer(invasive_percent ~ 
                pca1_clima +
                poly(native, 2) +
                 #  native +
                (1|dataset),
              weights = total_species,
              family = binomial, 
              data = alien_dat %>%  filter(type == "p_a" &  scale == 100) %>% 
                filter(!invasive_percent==0) %>% 
                filter(!invasive_percent>0.2))  # remove outlier

check_convergence(mod1s_inv)
check_overdispersion(mod1s_inv)
Anova(mod1s_inv)
summary(mod1s_inv)

# library(sjPlot)
plot_model(mod1s_inv,type = "pred", terms=c("native"),  show.data=T)

mod1s_inv_pred <- get_model_data(mod1s_inv,type = "pred", terms="native")

ggplot(mod1s_inv_pred, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  # geom_boxplot(alpha=0, lwd=0.6, outlier.shape = NA)+
  geom_point(data=alien_dat %>%  
               filter(type == "p_a" &  scale == 100) %>% 
               filter(!invasive_percent==0) %>% 
               filter(!invasive_percent>0.2), # remove outlier
             aes(native, invasive_percent),
             col="brown", size=2, alpha=0.6) +
  labs(y ="Proportion of invasive species", 
       x='Native species richness (SR)') +
  theme(axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=8),
        axis.title=element_text(size=11), 
        legend.text = element_text(size=10)) +
  # theme_bw()+
  geom_line(linetype=1, linewidth=1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))



### native cover ----


cover__inv <- alien_dat %>%  filter(type == "p_a"&  scale == 100) %>% 
  dplyr::select (dataset, series, pca1_clima, non_native_percent, invasive_percent, total_species) %>% 
  left_join(a, by=c("series"))


mod2s_inv<- glmer(invasive_percent ~ 
              # pca1_clima +
                native +
                (1|dataset),
              weights = total_species,
              family = binomial, 
              data = cover__inv %>% 
                filter(!invasive_percent==0) %>% 
                filter(!invasive_percent>0.2))


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
               filter(!invasive_percent==0) %>% 
               filter(!invasive_percent>0.2),
             aes(native, invasive_percent),
             col="brown", size=2, alpha=0.6) +
  labs(y="Proportion of invasive species", x='Cover of native species') +
  # size='Climate gradient (PC)') +
  # guides(shape = "none") +
  theme_bw()+
  geom_line(linetype=1, linewidth=1)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))



## neophyte_percent ----


### native SR ----
mod1s_neoph<- glmer(neophyte_percent ~ 
                     pca1_clima +
                    poly(native, 2) +
                    #  native +
                    (1|dataset),
                  weights = total_species,
                  family = binomial, 
                  data = alien_dat %>%  filter(type == "p_a" &  scale == 100) %>% 
                    filter(!neophyte_percent==0) %>% 
                    filter(!neophyte_percent>0.2))  # remove outlier

check_convergence(mod1s_neoph)
check_overdispersion(mod1s_neoph)
Anova(mod1s_neoph)
summary(mod1s_neoph)

# library(sjPlot)
plot_model(mod1s_neoph,type = "pred", terms=c("native"),  show.data=T)

mod1s_neoph_pred <- get_model_data(mod1s_neoph,type = "pred", terms="native")

ggplot(mod1s_neoph_pred, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  # geom_boxplot(alpha=0, lwd=0.6, outlier.shape = NA)+
  geom_point(data=alien_dat %>%  
               filter(type == "p_a" &  scale == 100) %>% 
               filter(!neophyte_percent==0) %>% 
               filter(!neophyte_percent>0.2), # remove outlier
             aes(native, neophyte_percent),
             col="slateblue", size=2, alpha=0.6) +
  labs(y ="Proportion of neophytes", 
       x='Native species richness (SR)') +
  theme(axis.text.x = element_text(size=8), 
        axis.text.y = element_text(size=8),
        axis.title=element_text(size=11), 
        legend.text = element_text(size=10)) +
  # theme_bw()+
  geom_line(linetype=1, linewidth=1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))



### native cover ----


cover_neoph <- alien_dat %>%  filter(type == "p_a"&  scale == 100) %>% 
  dplyr::select (dataset, series, pca1_clima, non_native_percent, neophyte_percent, total_species) %>% 
  left_join(a, by=c("series"))


mod2s_neoph<- glmer(neophyte_percent ~ 
                    # pca1_clima +
                    native +
                    (1|dataset),
                  weights = total_species,
                  family = binomial, 
                  data = cover_neoph %>% 
                    filter(!neophyte_percent==0) %>% 
                    filter(!neophyte_percent>0.2))


check_convergence(mod2s_neoph)
Anova(mod2s_neoph)
summary(mod2s_neoph)

# library(sjPlot)
plot_model(mod2s_neoph,type = "pred", terms=c("native"),  show.data=T)


mod2s_neoph_pred <- get_model_data(mod2s_neoph,type = "pred", terms="native[2:155, by=0.001]")


ggplot(mod2s_neoph_pred, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  # geom_boxplot(alpha=0, lwd=0.6, outlier.shape = NA)+
  geom_point(data=cover_neoph %>% 
               filter(!neophyte_percent==0) %>% 
               filter(!neophyte_percent>0.2),
             aes(native, neophyte_percent),
             col="slateblue", size=2, alpha=0.6) +
  labs(y="Proportion of neophytes", x='Cover of native species') +
  # size='Climate gradient (PC)') +
  # guides(shape = "none") +
  theme_bw()+
  geom_line(linetype=1, linewidth=1)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))



