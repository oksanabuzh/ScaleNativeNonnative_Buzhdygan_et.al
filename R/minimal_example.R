library(dplyr)
library(readr)

sp <- read_csv("data/database_analysis_summary.csv")

header2 <-  read_csv("data-raw/headers.csv") %>% 
  dplyr::select(series, habitat_broad) %>% 
  group_by(series) %>% 
  summarise(habitat_broad=unique(habitat_broad)) %>% 
  ungroup()

header_data <-  read_csv("data/header_data_prepared.csv")%>% 
  filter(subplot=="x") %>%
  dplyr::select(-subplot) %>% 
  left_join(header2, by="series")

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


alien_dat <- sp3 |>
  left_join(header_data, by = c("series")) %>% 
  left_join(disturb_data, by = c("series")) %>% 
  mutate(dataset=factor(dataset),
         cover_grv_stone=(cover_gravel + cover_stones)/100,
         roads=roads/100,
         built_up=built_up_2km/100,
         cover_litter= cover_litter/100,
         cover_herbs_sum=cover_herbs_sum/100) 

mod <- MASS::glmmPQL(invasive_percent ~ 
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
                       random = ~ 1|series,
                       weights = total_species,
                       family = quasibinomial,
                       data = alien_dat |> filter(type == "p_a" & 
                                                    scale == 0.1))

MuMIn::r.squaredGLMM(mod)["theoretical", ] 

# print session info and copy it to clipboard
devtools::session_info() |> clipr::write_clip()
