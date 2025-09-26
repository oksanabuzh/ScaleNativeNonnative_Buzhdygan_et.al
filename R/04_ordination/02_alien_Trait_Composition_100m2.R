# Purpose: Non-metric multidimensional scaling (NMDS) of the groups of
# alien species (sampled at the 100-m2 plots) based on their time of introduction (residence time),
# geographic origin (native range), and invasion status
# Produces Table S8 and Fig. S10

library(tidyverse)
library(vegan)
library(ggplot2)
library(ggrepel)
library(devtools)

# Load the data and prepare it ----------------------------------------------

header_data <- read_csv("data/header_data_prepared.csv") %>%
  filter(subplot == "x") %>%
  dplyr::select(-subplot)

names(header_data)

disturb_data <- read_csv("data-raw/Disturbn_commun_mean.csv") %>%
  filter(scale == 100) %>%
  dplyr::select(-scale, -subplot)

climate_pc <- read_csv("data/Clima_PC.csv") %>%
  group_by(series) %>%
  summarise(pca1_clima = mean(pca1_clima)) %>%
  ungroup()

# Species composition of allien species -----
com <- read_csv("data/non-native_matrix.csv") %>%
  separate(plot_name, c("series", "subplot", "scale"), "-") %>%
  filter(scale == 100) %>%
  dplyr::select(-scale, -subplot) %>%
  left_join(header_data, by = c("series")) %>%
  left_join(disturb_data, by = c("series")) %>%
  left_join(climate_pc, by = c("series")) %>%
  # mutate(dataset=factor(dataset)) %>%
  drop_na()

names(com)

# Traits of the alien species----
traits_st <- read_csv("data/non-native_trait_matrix.csv") %>%
  dplyr::select(-naturalisation_category)
names(traits_st)

trait <- com[1:87] %>% # select only species
  pivot_longer(-c("series"), names_to = "taxon_tpl", values_to = "abund") %>%
  distinct(taxon_tpl) %>%
  left_join(traits_st, by = "taxon_tpl") %>%
  magrittr::set_rownames(.$taxon_tpl) %>%
  as.matrix()

row.names(trait)
str(trait)
trait[, 2:4]

Com_comp <- com[1:87] %>%
  dplyr::select(-series) %>%
  as.matrix()

# check dimensions of community composition matrix and of trait matrix
dim(Com_comp)
dim(trait)

identical(row.names(trait), row.names(t(Com_comp)))

# check if there are NAs
which(is.na(Com_comp))
# check if there are species with zero appearance
rowSums(Com_comp[, -c(1)])
colSums(Com_comp[, -c(1)])

# (1) Functional composition -----
# Calculate funct. composition matrix ----

## 'functcomp' takes trait matrix and abundance matrix and for each site computes
# the community-level weighted means of each trait (waited by abundance)

library(FD)

# functcomp  - function for calculation of functional composition matrix

FNComp <- functcomp(trait[, 2:4], Com_comp, CWM.type = "all") #  bin.num - indicates binary traits to be treated as continuous

FNComp
head(FNComp)
str(FNComp)

FuncComp <- FNComp %>%
  tibble(series = com$series) %>%
  relocate(series) %>%
  rename(
    Archaeophyte = "introduction_time_archaeophyte",
    Neophyte = "introduction_time_neophyte",
    Origin_NrAm = "origin_NrAm",
    Origin_As = "origin_As",
    Origin_As = "origin_As",
    Origin_Eu = "origin_Eu",
    Origin_Eu.As = "origin_Eu.As",
    Origin_Eu.Med = "origin_Eu.Med",
    Origin_IT = "origin_IT",
    Origin_Med = "origin_Med",
    Origin_Med.As = "origin_Med.As",
    Origin_Med.IT = "origin_Med.IT",
    Origin_unknown = "origin_unknown",
    Casual = "naturalisation_level_casual",
    Invasive = "naturalisation_level_invasive",
    Naturalised = "naturalisation_level_naturalised"
    #  Agriophyte ="naturalisation_category_agriophyte",
    #  Colonophyte ="naturalisation_category_colonophyte",
    #  Ephemerophyte ="naturalisation_category_ephemerophyte",
    # Epoecophyte ="naturalisation_category_epoecophyte",
    #  Ergasiophigophyte ="naturalisation_category_ergasiophigophyte",
    # Hemiepoecophyte ="naturalisation_category_hemiepoecophyte"
  ) %>%
  dplyr::select(-Origin_unknown)

names(FuncComp)

str(FuncComp)


header <- header_data %>%
  mutate(cover_grv_stone = cover_gravel + cover_stones) %>%
  rename(build_up = built_up_2km) %>%
  dplyr::select(
    series, 
    pH, 
    heat_index, 
    microrelief,
    cover_grv_stone,
    cover_herbs_sum, 
    cover_litter,
    grazing_intencity,
    mowing,
    abandonment, 
    roads,
    builtup_1000m,
    cropland_1000m,
    builtup_250m,
    cropland_250m,
    builtup_500m,
    cropland_500m
  )

variabl <- FuncComp %>%
  left_join(header, by = c("series")) %>%
  left_join(disturb_data, by = c("series")) %>%
  left_join(climate_pc, by = c("series")) %>%
  drop_na()


names(variabl)

# PERMANOVA -----

set.seed(11)

PERM_mod1 <- adonis2(
  FuncComp %>% dplyr::select(-series) ~ #variabl[, 2:21] ~
    pca1_clima +
      pH +
      heat_index + #
      microrelief +
      cover_grv_stone +
      cover_herbs_sum + # cover_shrub_total+
      cover_litter +
      grazing_intencity +
      mowing +
      abandonment + # burning +
      builtup_500m +
      cropland_500m +
      roads +
      Disturbance.Frequency +
      Disturbance.Severity,
  data = variabl,
  permutations = 1000,
  method = "bray"
) #, strata=factor(variabl$series))

PERM_mod1


# write.csv(PERM_mod1, "results/PERMANOVA_FuncTraits_Table_S8.csv")

# NMDS -----
names(variabl)

set.seed(11)
nmds1 <- vegan::metaMDS(
  FuncComp %>% dplyr::select(-series), 
  distance = "gow",
  k = 2,
  trymax = 100
)

nmds1 # the stress value shows how easy it was to condense multidimensional data into two dimensional space, below 0.2 is generally good
vegan::stressplot(nmds1, main = "Shepard plot")

# Quick plots:
plot(nmds1)
vegan::scores(nmds1)


# envfit -----
# posthoc fit of environmental variables

set.seed(1)

fit1 <- vegan::envfit(
  nmds1 ~
    pca1_clima +
      pH + 
      heat_index +
      microrelief +
      cover_grv_stone +
      cover_herbs_sum + 
      cover_litter +
      grazing_intencity +
      mowing +
      abandonment + 
      builtup_500m +
      cropland_500m +
      roads +
      Disturbance.Frequency +
      Disturbance.Severity,
  data = variabl,
  perm = 1000
) 


fit1

# Quick plots:
set.seed(1)
plot(nmds1, display = "species", scaling = "species")
plot(fit1)
ggvegan::autoplot(nmds1)


# plots ------

# extract species scores
species.scores <- scores(nmds1, display = 'sp') %>%
  as_tibble(rownames = 'species')

species.scores

sites.scores <- scores(nmds1, display = 'sites') %>%
  as_tibble(rownames = 'sites.scores')

sites.scores


# Assign colours for the traits:
names(variabl[, 2:16])

col = c(
  "blue",
  "blue",
  #   "limegreen", "limegreen",
  "orange",
  "orange",
  "orange",
  "orange",
  "orange",
  "orange",
  "orange",
  "orange",
  "orange",
  "orange",
  "red",
  "red",
  "red"
)

p1 <- ggplot(data = sites.scores, aes(x = NMDS1, y = NMDS2)) +
  geom_vline(xintercept = 0, col = "grey", linetype = "dashed") +
  geom_hline(yintercept = 0, col = "grey", linetype = "dashed") +
  scale_shape_manual(values = c(19, 1)) +
  geom_point(
    data = species.scores %>%
      filter(!species == "Origin_unknown"),
    size = 2,
    pch = 19,
    colour = col
  ) + #, colour=Col, fill=Col
  geom_text_repel(
    data = species.scores %>%
      filter(!species == "Origin_unknown"),
    aes(x = NMDS1, y = NMDS2, label = species),
    size = 3.6,
    colour = col
  ) +
  theme(
    axis.title = element_text(size = 13, face = "bold", colour = "black"),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "grey30", linewidth = 0.5),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 13, colour = "black")
  )

p1


#  add standardised scores:
coord_cont <- as.data.frame(scores(fit1, "vectors")) %>%
  cbind(stand = fit1$vectors$arrows) %>%
  cbind(manual = fit1$vectors$arrows * sqrt(fit1$vectors$r)) %>%
  cbind(permanona_R2 = fit1$vectors$arrows * sqrt(PERM_mod1$R2)) %>%
  mutate(Variables = rownames(scores(fit1, "vectors"))) %>%
  mutate(
    Variables_new = fct_recode(
      Variables,
      ClimatePC = "pca1_clima",
      # Soil.C="Corg",
      Heat.stress = "heat_index",
      Stones = "cover_grv_stone",
      Herb.covr = "cover_herbs_sum",
      Litter = "cover_litter",
      Grazing = "grazing_intencity",
      Mowing = "mowing",
      Builtup = "builtup_500m",
      Croplans = "cropland_500m",
      Roads = "roads",
      Distr.Frqnc = "Disturbance.Frequency",
      Distr.Sevr = "Disturbance.Severity"
    )
  ) %>%
  filter(
    Variables_new %in%
      c(
        "ClimatePC",
        "pH",
        "Stones",
        "Herb.covr",
        "Builtup",
        "Distr.Sevr",
        "Distr.Frqnc"
      )
  )

coord_cont

ordiArrowMul(fit1)

# rescale  all arrows to fill an ordination plot,
# where fill =  shows proportion of plot to be filled by the arrows
coord_cont_standrd <- coord_cont %>%
  mutate(
    stand.NMDS1 = stand.NMDS1 * ordiArrowMul(fit1, rescale = TRUE, fill = 0.4)
  ) %>%
  mutate(
    stand.NMDS2 = stand.NMDS2 * ordiArrowMul(fit1, rescale = TRUE, fill = 0.4)
  )

coord_cont_standrd


# standardized driver vectors fitted posthoc to the NMDS
ggplot(data = sites.scores, aes(x = NMDS1, y = NMDS2)) +
  geom_vline(xintercept = 0, col = "grey", linetype = "dashed") +
  geom_hline(yintercept = 0, col = "grey", linetype = "dashed") +
  geom_segment(
    aes(x = 0, y = 0, xend = stand.NMDS1, yend = stand.NMDS2),
    data = coord_cont_standrd,
    linewidth = 0.7,
    alpha = 1,
    colour = "darkgray",
    arrow = arrow(length = unit(0.03, "npc"))
  ) +
  # xlim(-1,0.9)+ylim(-0.95,0.7)
  geom_text(
    data = coord_cont_standrd,
    aes(
      x = stand.NMDS1 + 0.02 * sign(stand.NMDS1),
      y = stand.NMDS2 + 0.02 * sign(stand.NMDS2),
      label = Variables_new
    ),
    colour = "black",
    fontface = "bold",
    size = 4,
    vjust = c(1, 1, 1, 1, 1, 1, 1),
    hjust = c(0, 0, 0.7, 0, 1, 0, 0)
  ) +
  geom_point(
    data = species.scores %>%
      filter(!species == "Origin_unknown"),
    size = 2,
    pch = 19,
    colour = col
  ) + #, colour=Col, fill=Col
  geom_text_repel(
    data = species.scores %>%
      filter(!species == "Origin_unknown"),
    aes(x = NMDS1, y = NMDS2, label = species),
    size = 3.6,
    colour = col
  ) +
  theme(
    axis.title = element_text(size = 13, face = "bold", colour = "black"),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "grey30", size = 0.5),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 13, colour = "black")
  )


factor(coord_cont$Variables_new)