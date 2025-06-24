# Purpose: Plot standardized effects of the predictors on the response variables
dev.off()

# Load necessary libraries
library(tidyverse)

# Load the data and prepare it ----------------------------------------------
results <- read_csv("data/model_results_summary_OB.csv") %>% 
  filter(model_id=="climate" | model_id=="disturbance") %>% 
    bind_rows(results <- read_csv("data/model_results_summary.csv")%>% 
                filter(model_id%in% c("builtup_250m", "builtup_500m") & 
                         predictor %in% c("builtup_1000m", "cropland_1000m",
                                          "builtup_250m",  "cropland_250m",
                                          "builtup_500m",  "cropland_500m"))) 

str(results)

names(results)


unique(results$model_id)
unique(results$response_var)

write_csv(results %>% 
            filter(!scale==0.0001) %>% 
            dplyr::select(-status, -r2_partial), 
          "results/GLMM_results_Table_S5.csv")



results <- results |>
  filter(response_var %in% c("non_native_percent", "invasive_percent", "neophyte_percent"))


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

    filter(response_var == "non_native_percent" & scale == 10) |>
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
          "non_native_percent", "invasive_percent", "neophyte_percent"
        )
    ),
    predictor = factor(predictor, levels = variable_order)
  )


unique(results$predictor)
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
                               # "Urban built-up"= "built_up_2km",  
                               "Urban built-up (1000 m)"= "builtup_1000m",  
                               "Croplands cover (1000 m)"= "cropland_1000m",
                               "Urban built-up (250 m)"= "builtup_250m",  
                               "Croplands cover (250 m)"= "cropland_250m",
                                "Urban built-up (500 m)"= "builtup_500m",  
                               "Croplands cover (500 m)"= "cropland_500m",
                               "Road density"= "roads", 
                                "Disturbance frequency"= "Disturbance.Frequency", 
                                "Disturbance severity"= "Disturbance.Severity"
                                                   )) %>% 
  mutate(variable_new =fct_relevel(variable_new, 
             "Climate PC", "Soil pH",   "Heat index", 
             "Microrelief", "Gravel & stone cover",
             "Herb cover", "Litter cover", 
             "Grazing", "Mowing", "Abandonment", "Road density",  
            "Croplands cover (250 m)", "Croplands cover (500 m)", "Croplands cover (1000 m)", 
               "Urban built-up (250 m)","Urban built-up (500 m)", "Urban built-up (1000 m)",
             "Disturbance frequency", "Disturbance severity")) %>% 
  mutate(variable_new =fct_relevel(variable_new, rev)) %>%
  mutate(response_var_new=factor(case_when(response_var=="non_native_percent"~
                                   "Alien species, %",
                                 response_var=="invasive_percent"~
                                   "Invasive species, %",
                                 response_var=="neophyte_percent"~
                                   "Neophytes, %")))

  

results$response_var_new

std_effect_plot <- results %>% 
  filter(scale==100)|> 
  #filter(!scale==0.0001 & !scale==0.001)|> 
  ggplot(aes(x = std_slope, y = variable_new #variable_of_interest
             )) +
  geom_point(aes(
    color = ifelse(std_slope < 0, "blue", "red"),
    alpha = significance), 
    size=4) +
 # geom_text(aes(label=std_slope), size=6) +
# geom_segment( aes(x=0, xend=std_slope, y=variable_new, yend=variable_new,
#                    color = ifelse(std_slope < 0, "blue", "red"),
#                   alpha = significance) ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c( "red", "blue")) +
  facet_grid(~ response_var_new ) +
  scale_x_continuous(limits = c(-0.86, 0.45)) +
  scale_alpha_manual(values = c("significant" = 1, "not significant" = 0.4)) +
  labs(
    x = "Standardized effect of the driver",
    y = "Driver" #, title = "Scale (plot size, m2)"
  ) +
  theme_bw() +
  theme(text = element_text(size = 12, colour = "black"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size=10),
    axis.text.y = element_text(size=10, colour = "black") ,
    axis.text.x = element_text(size=9, colour = "black")
  )



std_effect_plot

#ggsave("img/std_effect_plot_Fig_3AB.png", std_effect_plot, width = 20, height = 20, 
#       units = "cm")



## for the supplementary ----
# we plot only for alien species. For invasive species there is no standardised effcets

std_effect_plot_suppl <- results %>% 
 # filter(scale==100)|> 
 filter(!scale==0.0001  & response_var=="non_native_percent")|> 
  ggplot(aes(x = std_slope, y = variable_new #variable_of_interest
  )) +
#   geom_text(aes(label=std_slope), size=6) +
  geom_point(aes(
    color = ifelse(std_slope < 0, "blue", "red"),
    alpha = significance), 
    size=4) +
  # geom_text(aes(label=std_slope), size=6) +
  # geom_segment( aes(x=0, xend=std_slope, y=variable_new, yend=variable_new,
  #                    color = ifelse(std_slope < 0, "blue", "red"),
  #                   alpha = significance) ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c( "red", "blue")) +
  facet_grid(~ scale , scales = "free_x") +
 # scale_x_continuous(limits = c(-1.3, 1)) +
  scale_alpha_manual(values = c("significant" = 1, "not significant" = 0.4)) +
  labs(
    x = "Standardized effect of the driver",
    y = "Driver" , title = "Scale (plot size, m2)"
  ) +
  theme_bw() +
  theme(text = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size=10),
        axis.text.y = element_text(size=10) ,
        axis.text.x = element_text(size=7)
  )


std_effect_plot_suppl

# ggsave("img/std_effect_plot_Fig_S4.png", std_effect_plot_suppl, width = 20, height = 20, 
#       units = "cm")


# END ------------------------------

