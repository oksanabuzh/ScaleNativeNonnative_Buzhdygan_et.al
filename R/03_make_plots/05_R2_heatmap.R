# Figure S8: Variance explained by each predictor (R2) as heatmap 

# Packages
library(tidyverse)

# Prepare data
alien_dat <- read_csv("data/model_results_summary.csv") %>% 
  filter(model_id=="climate" | model_id=="disturbance") %>% 
  bind_rows(read_csv("data/model_results_summary.csv")%>% 
              filter(model_id%in% c("builtup_250m", "builtup_500m") & 
                       predictor %in% c("builtup_1000m", "cropland_1000m",
                                        "builtup_250m",  "cropland_250m",
                                        "builtup_500m",  "cropland_500m"))) %>% 
  bind_rows(read_csv("data/model_results_summary.csv")%>% 
              filter(model_id =="climate_native" & 
                       predictor=="native" & remove_zeroes==TRUE)) %>% 
  filter(!scale==0.0001) %>% 
  dplyr::select(response_var, scale, predictor, r2_partial) %>% 
  mutate(variable_new=
           fct_recode(predictor,
                      "Native diversity"= "native", 
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
                      "Disturbance severity"= "Disturbance.Severity"
           )) %>% 
  mutate(variable_new =fct_relevel(variable_new, 
                                   "Native diversity", "Climate PC", "Soil pH",   "Heat index", 
                                   "Microrelief", "Gravel & stone cover",
                                   "Herb cover", "Litter cover", 
                                   "Grazing", "Mowing", "Abandonment", 
                                   "Croplands cover (250 m)", "Croplands cover (500 m)", "Croplands cover (1000 m)", 
                                   "Urban built-up (250 m)","Urban built-up (500 m)", "Urban built-up (1000 m)",
                                   "Road density",  
                                   "Disturbance frequency", "Disturbance severity")) %>% 
  mutate(variable_new =fct_relevel(variable_new, rev)) %>% 
  mutate(response_var_new = case_when(
    response_var == "non_native_percent" ~ "Alien species, %",
    response_var == "archaeophyte_percent" ~ "Archaeophytes, %",
    response_var == "neophyte_percent" ~ "Neophytes, %",
    response_var == "invasive_percent" ~ "Invasive species, %"
  )) %>%
  mutate(response_var_new = factor(response_var_new)) %>%
  mutate(response_var_new = fct_relevel(response_var_new,
                                        "Alien species, %",
                                        "Archaeophytes, %",
                                        "Neophytes, %",
                                        "Invasive species, %"
  ))



# data for plot
data_to_plot <- alien_dat %>%
  filter(
    response_var == "non_native_percent" |
      response_var == "archaeophyte_percent" |
      response_var == "neophyte_percent" &
      !scale %in% c(0.0001, 0.001, 0.01)|
      response_var == "invasive_percent" &
      !scale %in% c(0.0001, 0.001, 0.01)
  ) %>%
  mutate(
    r2_partial = round(r2_partial, 2)
  ) |>
  mutate(
    r2_partial = ifelse(near(r2_partial, 0, 0.001), NA, r2_partial)
  )

# Add color for the text -> if the value of r2 is high, the
# text color should be white, otherwise not legible on the fill
# color scale
data_to_plot$color_text <- ifelse(data_to_plot$r2_partial > 0.5,
  "white", "black")

fig_S8 <- ggplot(data_to_plot,
  aes(
    x = as.factor(scale),
    y = variable_new,
    fill = r2_partial
  )
) +
  geom_tile() +
  facet_grid(. ~ response_var_new, scales = "free_x", space = "free") +
  labs(
    fill = bquote("Variance explained, partial R"^"2"),
    x = bquote("Grain size m"^"2"),
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(color = "black", size = 12),
    axis.text = element_text(color = "black", size = 12),
  ) +
  scale_fill_distiller(
    palette = 2,
    direction = 1,
    na.value = "white"
  ) +
  geom_text(aes(label = r2_partial, color = color_text)) +
  scale_color_manual(values = c("black", "white"), guide = FALSE)


fig_S8

ggsave(filename = "results/figS8.png", fig_S8,
  width = 16, height = 10, units = "cm")
