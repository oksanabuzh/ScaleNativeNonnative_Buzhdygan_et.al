# Purpose: Plot the correlation coefficient of each slope with scale for the
# different drivers

# Load necessary libraries
library(tidyverse)

library(jtools)



# Load the data and prepare it ----------------------------------------------
results <- read_csv("data/model_results_summary.csv")%>% 
  filter(model_id=="climate" | model_id=="disturbance") 

# to transform back slopes
#  mutate(slope=case_when(
 #   predictor=="cover_gravel_stones"~ slope*100,
  #  predictor=="roads"~ slope*100,
   # predictor=="built_up_2km"~ slope*100,
    #predictor=="cover_litter"~ slope*100,
    #predictor=="cover_herbs_sum"~ slope*100,
    #.default = slope), .after=slope)
    

# Add information whether the models have just one predictor or whether
# they were run with climate or SR
results <- results |>
  mutate(
    significance = ifelse(p_value_chisq < 0.05, "significant", "not significant")) 
# filter(!is.na(std_slope))

# Subset only models with climate as a secondary predictor OR the model with
# only climate as predictor
#results <- results |>
#  filter(secondary_variable == "pca1_clima" | variable_of_interest == "pca1_clima"
#         | variable_of_interest == "altitude" # | variable_of_interest == "total_species"
#        )



# Reorder Predictor variables based on the standardized effect of the small scale
variable_order <- results |>
  filter(response_var == "non_native_percent" & scale == 100) |>
  arrange(-std_slope) |>
  pull(predictor) |> unique()

# did I miss any predictors?
missing_vars <- setdiff(results$predictor |> unique(), variable_order)
variable_order <- c(variable_order, missing_vars)

results <- results |>
  distinct() |> 
  mutate(
    response_var = factor(response_var,
                          levels =
                            c(
                              "non_native_percent", "invasive_percent" 
                            )
    ),
    predictor = factor(predictor, levels = variable_order)
  )



results <- results %>% mutate(variable_new=
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
                                           "Urban built-up"= "built_up_2km",  
                                           "Road density"= "roads", 
                                           "Disturbance frequency"= "Disturbance.Frequency", 
                                           "Disturbance severity"= "Disturbance.Severity"
                                )) %>% 
  mutate(variable_new =fct_relevel(variable_new, 
                                   "Climate PC", "Soil pH",   "Heat index", 
                                   "Microrelief", "Gravel & stone cover",
                                   "Herb cover", "Litter cover", 
                                   "Grazing", "Mowing", "Abandonment", 
                                   "Urban built-up","Road density",  
                                   "Disturbance frequency", "Disturbance severity")) %>% 
  mutate(variable_new =fct_relevel(variable_new, rev)) %>%
  mutate(response_var_new=factor(case_when(response_var=="non_native_percent"~
                                             "Proportion of alien species",
                                           response_var=="invasive_percent"~
                                             "Proportion of invasive species")))

# Plots of scale-dependency of driver effects -----


# Alien species ----
# Predicted effects with scale 


model_type_alien <- read_csv("results/Model_selection_Table.csv") %>% 
  filter(response_var=="non_native_percent") %>% 
# dplyr::select(variable_new, predictor, final_model) %>% 
  mutate(my.formula=case_when(final_model== "m1" ~ "y ~ x",
                                     .default ="y ~ poly(x, 2)"))%>% 
  mutate(p_value = case_when(final_model == "m1" ~ m1.p.value,
                             TRUE ~ m2.p.value)) %>% 
  mutate(signif=case_when(p_value < 0.05  ~ "Sign",
                               p_value >= 0.05 & p_value < 0.095 ~ "Marg",
                          .default ="Nonsig")) %>% 
  dplyr::select(variable_new, predictor, my.formula, final_model, signif, p_value) 


ggplot(results %>% 
         filter(response_var=="non_native_percent") %>% 
         filter(!scale==0.0001),
       aes(log(scale), slope)) +
  mapply(function(x, z, signif) 
    geom_smooth(method="lm", 
                data=function(d) subset(d, predictor==z), 
                formula = x, 
                fill="forestgreen", col="forestgreen",  alpha=0.07,
                linetype = ifelse(signif == "Sign"| signif == "Marg",
                                  "solid", "dashed"),
                linewidth = case_when(
                  signif == "Sign" ~ 1.1,
                  signif == "Nonsig" ~ 0.5,
                  .default = 0.4)),
    model_type_alien$my.formula, 
    model_type_alien$predictor, 
    model_type_alien$signif) +
  geom_point(col = "gray22", size = 2, alpha = 0.6) +
  labs(y = "Slope of the driver effect on proportion of alien species", 
       x = expression(paste('Grain size, ', m^{2}))) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 7, colour = "black"), 
        axis.text.y = element_text(size = 6, colour = "black"),
        axis.title = element_text(size = 13, colour = "black"), 
        legend.text = element_text(size = 10, colour = "black")) +
  facet_wrap(~reorder(variable_new, desc(variable_new)), scales = "free", nrow = 7) +
  #   facet_wrap(~variable_new, scales = "free", nrow = 7) +
  scale_x_continuous(breaks=c(-6.907755, -4.605170, -2.302585,  0.000000,  2.302585,  4.605170),
                     labels=c("0.001", "0.01", "0.1", "1", "10", "100"))


# Invasive species ----
# Predicted effects with scale 


model_type_invas <- read_csv("results/Model_selection_Table.csv") %>% 
  filter(response_var=="invasive_percent") %>% 
  mutate(my.formula=case_when(final_model== "m1" ~ "y ~ x",
                              .default ="y ~ poly(x, 2)")) %>% 
  mutate(p_value = case_when(final_model == "m1" ~ m1.p.value,
                             TRUE ~ m2.p.value)) %>% 
  mutate(signif=case_when(p_value < 0.05  ~ "Sign",
                          p_value >= 0.05 & p_value < 0.095 ~ "Marg",
                          .default ="Nonsig")) %>% 
  dplyr::select(variable_new, predictor, my.formula, final_model, signif, p_value)  


results %>% pull(predictor) %>% unique()



ggplot(results %>% 
         filter(response_var == "invasive_percent") %>% 
         filter(!scale %in% c(0.0001, 0.001, 0.01)), 
       aes(log(scale), slope)) +
  mapply(function(x, z, signif) 
    geom_smooth(method = "lm",  
                data = function(d) subset(d, predictor == z), 
                formula = x, 
                fill = "brown", col = "brown", alpha = 0.07,
                linetype = ifelse(signif == "Sign"| signif == "Marg",
                                  "solid", "dashed"),
                linewidth = case_when(
                  signif == "Sign" ~ 1.1,
                  signif == "Nonsig" ~ 0.5,
                  .default = 0.4)),
    model_type_invas$my.formula, 
    model_type_invas$predictor, 
    model_type_invas$signif) +
  geom_point(col = "gray22", size = 2, alpha = 0.6) +
  labs(y = "Slope of the driver effect on proportion of invasive species", 
       x = expression(paste('Grain size, ', m^{2}))) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 7, colour = "black"), 
        axis.text.y = element_text(size = 6, colour = "black"),
        axis.title = element_text(size = 13, colour = "black"), 
        legend.text = element_text(size = 10, colour = "black")) +
  facet_wrap(~reorder(variable_new, desc(variable_new)), scales = "free", nrow = 7) +
  scale_x_continuous(breaks = c(-2.302585, 0.000000, 2.302585, 4.605170),
                     labels = c("0.1", "1", "10", "100"))



# END ----------




correlation_ <- results %>%
  dplyr::select(response_var_new, variable_new, scale, slope) %>% 
  # filter(response_var=="non_native_percent"| response_var=="binary_non_native") %>% 
  summarize(
    correlation_P = Hmisc::rcorr(slope, scale, type = "spearman")$P[,1],
    #  correlation_r = Hmisc::rcorr(slope, scale, type = "spearman")$r[,1],
    .by = c(response_var_new, variable_new)) %>% 
  drop_na()


correlation <- results %>%
  dplyr::select(response_var_new, variable_new, scale, slope) %>% 
  summarize(
    correlation = cor(slope, scale, use = "pairwise.complete.obs", 
                      method = "spearman"),
    .by = c(response_var_new, variable_new)
  ) %>% 
  left_join(correlation_, by=c("response_var_new", "variable_new")) %>% 
  mutate(
    significance = ifelse(correlation_P < 0.051, "significant", "not significant"),
    significance_marg = ifelse(correlation_P < 0.08, "marginally significant", "not significant")) 



#write_csv(correlation %>% 
#            select(-significance, -significance_marg), 
#          "results/Scale_dependancy_Table_S6.csv")



Cor_fig <- ggplot(correlation, aes(x = correlation, y = variable_new)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8,
           aes(color = ifelse(correlation < 0, "neg", "pos"),
               fill = ifelse(correlation < 0, "neg", "pos") ,
               alpha = significance)) +
  #  geom_point(aes(color = ifelse(correlation < 0, "neg", "pos"),
  #                 fill = ifelse(correlation < 0, "neg", "pos")),
  #            size=2.5, pch=23) +
  scale_color_manual(values = c("neg" = "red", "pos" = "blue")) +
  scale_fill_manual(values = c("neg" = "red", "pos" = "blue")) +
  scale_alpha_manual(values = c("significant" = 1, "not significant" = 0.4)) +
  #  geom_segment( aes(x=0, xend=correlation, y=variable_new, yend=variable_new,
  #                    color = ifelse(correlation < 0, "neg", "pos"),
  #                    fill = ifelse(correlation < 0, "neg", "pos"))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~response_var_new, nrow = 1) +
  theme_bw() +
  labs(
    x = "Correlation of driver effect with scale",
    y = "Driver"
  ) +
  theme(text = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.y = element_text(size=10) ,
        axis.text.x = element_text(size=9)
  )



Cor_fig


# ggsave("img/scale_correl_plot_Fig_3CD.png", Cor_fig, width = 20, height = 20, units = "cm")


