# Purpose: Test relationships of drivers effects (slopes) with scale

library(broom)
library(dplyr)
library(purrr)

# Load the data and prepare it ----------------------------------------------
results <- read_csv("data/model_results_summary_OB.csv")%>% 
  filter(model_id=="climate" | model_id=="disturbance") %>% 
  bind_rows(results <- read_csv("data/model_results_summary_OB.csv")%>% 
              filter(model_id%in% c("builtup_250m", "builtup_500m") & 
                       predictor %in% c("builtup_1000m", "cropland_1000m",
                                        "builtup_250m",  "cropland_250m",
                                        "builtup_500m",  "cropland_500m")))


read_csv("data/model_results_summary.csv")%>% 
  filter(response_var=="invasive_percent") %>% 
  count(model_id, scale)

# (1) Alien ----
# Run models without and with the polinomial:
models_run_alien <- results %>% 
  filter(response_var=="non_native_percent") %>% 
 filter(!scale==0.0001) %>% 
  mutate(predictor = as_factor(predictor)) %>% 
# group_by(predictor) %>% 
  split(f = as.factor(.$predictor)) %>% 
 # group_split(predictor) %>% 
lapply(function(df) {
  model_list=list(
    mod1=lm(slope ~ log(scale), data=df),
    mod2=lm(slope ~ poly(log(scale), 2), data=df))  
})


# select best model using the logLik test:

# which statistics to use form glance():
stats <- c("statistic", "p.value", # "statistics" and "p.value" are for the F-statistics
           "AIC", "logLik") # "statistics"

# extract p-value from model comparison using LogLik test: anova(mod1, mod2)
pval <- function(x, y) anova(x, y, test = "Chisq")$"Pr(>Chi)"[2]

# 
alien_select <- models_run_alien %>%
#setNames(vars) %>%
  map_dfr(with, c(m1 = glance(mod1)[stats] ,
                  m2 = glance(mod2)[stats],
                  logLik_pval = pval(mod1, mod2)), 
          .id = "response")  %>% 
  mutate(final_model=case_when(logLik_pval<0.05 ~ "m2_poly",
                               .default ="m1")) %>% 
  mutate(final_mod_formula=case_when(logLik_pval<0.05 ~ "slope ~ poly(log(scale), 2)",
                               .default ="slope ~ log(scale)")) %>% 
  rename(predictor=response) %>% 
  mutate(response_var="non_native_percent", .after=predictor) 

alien_select

# (2) Invasive ----
# Run models without and with the polinomial:
models_run_invasive <- results %>% 
  filter(response_var=="invasive_percent") %>% 
  filter(!scale %in% c(0.0001, 0.001, 0.01)) %>% 
  mutate(predictor = as_factor(predictor)) %>% 
  # group_by(predictor) %>% 
  split(f = as.factor(.$predictor)) %>% 
  # group_split(predictor) %>% 
  lapply(function(df) {
    model_list=list(
      mod1=lm(slope ~ log(scale), data=df),
      mod2=lm(slope ~ poly(log(scale), 2), data=df))  
  })


# select best model based on the logLik test

invasive_select <- models_run_invasive %>%
  #setNames(vars) %>%
  map_dfr(with, c(m1 = glance(mod1)[stats] ,
                  m2 = glance(mod2)[stats],
                  logLik_pval = pval(mod1, mod2)), 
          .id = "response")  %>% 
  mutate(final_model=case_when(logLik_pval<0.05 ~ "m2_poly",
                               .default ="m1")) %>% 
  mutate(final_mod_formula=case_when(logLik_pval<0.05 ~ "slope ~ poly(log(scale), 2)",
                                     .default ="slope ~ log(scale)")) %>% 
  rename(predictor=response) %>% 
  mutate(response_var="invasive_percent", .after=predictor)


invasive_select


# (3) Neophites ----
# Run models without and with the polinomial:
models_run_neophyte <- results %>% 
  filter(response_var=="neophyte_percent") %>% 
  filter(!scale==0.0001) %>% 
 # filter(!scale %in% c(0.0001, 0.001, 0.01)) %>% 
  mutate(predictor = as_factor(predictor)) %>% 
  # group_by(predictor) %>% 
  split(f = as.factor(.$predictor)) %>% 
  # group_split(predictor) %>% 
  lapply(function(df) {
    model_list=list(
      mod1=lm(slope ~ log(scale), data=df),
      mod2=lm(slope ~ poly(log(scale), 2), data=df))  
  })


# select best model based on the logLik test

neophyte_select <- models_run_neophyte %>%
  #setNames(vars) %>%
  map_dfr(with, c(m1 = glance(mod1)[stats] ,
                  m2 = glance(mod2)[stats],
                  logLik_pval = pval(mod1, mod2)), 
          .id = "response")  %>% 
  mutate(final_model=case_when(logLik_pval<0.05 ~ "m2_poly",
                               .default ="m1")) %>% 
  mutate(final_mod_formula=case_when(logLik_pval<0.05 ~ "slope ~ poly(log(scale), 2)",
                                     .default ="slope ~ log(scale)")) %>% 
  rename(predictor=response) %>% 
  mutate(response_var="neophyte_percent", .after=predictor)


neophyte_select

# Merge tables: 

Table <- bind_rows(alien_select, invasive_select, neophyte_select) %>% 
  mutate(variable_new=
               fct_recode(predictor,
                          "Climate PC"= "pca1_clima", 
                          "Soil pH"= "pH", 
                          "Heat index"= "heat_index",
                          "Microrelief"= "microrelief", 
                          "Gravel & stone cover"= "cover_gravel_stones",
                          "Herb cover"= "cover_herbs_sum",
                          "Litter cover"= "cover_litter", 
                          "Grazing"= "grazing_intencity", 
                          "Mowing"= "mowing", 
                          "Abandonment"= "abandonment", 
                          "Urban built-up (1000 m)"= "builtup_1000m",  
                          "Croplands cover (1000 m)"= "cropland_1000m",
                          "Urban built-up (250 m)"= "builtup_250m",  
                          "Croplands cover (250 m)"= "cropland_250m",
                           "Urban built-up (500 m)"= "builtup_500m",  
                          "Croplands cover (500 m)"= "cropland_500m",
                          "Road density"= "roads", 
                          "Disturbance frequency"= "Disturbance.Frequency", 
                          "Disturbance severity"= "Disturbance.Severity"),
         .before=predictor) %>% 
  mutate(variable_new =fct_relevel(variable_new, 
                                   "Climate PC", "Soil pH",   "Heat index", 
                                   "Microrelief", "Gravel & stone cover",
                                   "Herb cover", "Litter cover", 
                                   "Grazing", "Mowing", "Abandonment", 
                                   "Croplands cover (250 m)", "Urban built-up (250 m)",
                                   "Croplands cover (500 m)", "Urban built-up (500 m)",
                                   "Croplands cover (1000 m)", "Urban built-up (1000 m)",
                                   "Road density",  
                                   "Disturbance frequency", "Disturbance severity")) %>% 
  arrange(response_var, variable_new)


Table %>% 
  print(n=Inf)

write_csv(Table, "results/Model_selection_Table.csv")


