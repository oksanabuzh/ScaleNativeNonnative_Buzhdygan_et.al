# Purpose: Run ordination for 100 m2 plots
library(tidyverse)
library(vegan)
library(ggrepel)

# Load the data and prepare it ----------------------------------------------

header_data <- read_csv("data/header_data_prepared.csv") %>%
  filter(subplot == "x") %>%
  dplyr::select(-subplot)

names(header_data)


header <- header_data %>%
  mutate(cover_grv_stone = cover_gravel + cover_stones) %>%
  rename(build_up = built_up_2km) %>%
  dplyr::select(series, # altitude,
    pH, # Corg,
    heat_index, #
    microrelief,
    cover_grv_stone,
    cover_herbs_sum, # cover_shrub_total,
    cover_litter,
    grazing_intencity, mowing, abandonment, # burning ,
    # economic_use,
    build_up, roads)


disturb_data <- read_csv("data-raw/Disturbn_commun_mean.csv") %>%
  filter(scale == 100) %>%
  dplyr::select(-scale, -subplot)

climate_pc <- read_csv("data/Clima_PC.csv") %>%
  group_by(series) %>%
  summarise(pca1_clima = mean(pca1_clima)) %>%
  ungroup()


traits_st <- read_csv("data/non-native_trait_matrix.csv")


# Species composition of allien species
com <- read_csv("data/non-native_matrix.csv") %>%
  separate(plot_name, c("series", "subplot", "scale"), "-") %>%
  filter(scale == 100) %>%
  dplyr::select(-scale, -subplot) %>%
  left_join(header, by = c("series")) %>%
  left_join(disturb_data, by = c("series")) %>%
  left_join(climate_pc, by = c("series")) %>%
  # mutate(dataset=factor(dataset)) %>%
  drop_na()

names(com)
# write.csv(com, "data/non-native_matr_OB.csv")



#  Species composition ------


# PERMANOVA -----

set.seed(1)

PERM_mod2 <- adonis2(com[, 2:87] ~
  pca1_clima +
  # altitude +
  pH +
  heat_index +
  microrelief +
  cover_grv_stone +
  cover_herbs_sum + # cover_shrub_total+
  cover_litter +
  grazing_intencity + mowing +
  abandonment + # burning +
  build_up + roads +
  Disturbance.Frequency +
  Disturbance.Severity,
data = com,
permutations = 1000, method = "bray") # , strata=factor(variabl$series))

PERM_mod2


write.csv(PERM_mod2, "results/PERMANOVA_Spec_Table_S4_A.csv")
# NMDS -----

set.seed(100)
nmds2 <- vegan::metaMDS(com[, 2:87],
  # com %>% select(-series),
  distance = "gow", k = 2, trymax = 100)

nmds2 # the stress value shows how easy it was to condense multidimensional data into two dimensional space, below 0.2 is generally good


vegan::stressplot(nmds2, main = "Shepard plot")
plot(nmds2)

# set.seed(1)


plot(nmds2, display = "species")



# envfit -----
set.seed(1)

fit2 <- vegan::envfit(nmds2 ~ pca1_clima +
  # altitude, # lon,
  pH + # Corg +
  heat_index + #
  microrelief +
  cover_grv_stone +
  cover_herbs_sum + # cover_shrub_total+
  cover_litter +
  grazing_intencity + mowing + abandonment + # burning +
  build_up + roads +
  Disturbance.Frequency +
  Disturbance.Severity,
data = com, perm = 1000) # , strata=factor(variabl$series)) #



fit2


set.seed(1)
plot(nmds2, display = "species",
  scaling = "species")
plot(fit2)


library(ggvegan)
library(ggplot2)

autoplot(nmds2)




# plots ------

# extract species scores
species.scores2 <- scores(nmds2, display = 'sp') %>%
  as_tibble(rownames = 'species') %>%
  mutate(short_name = str_c(str_split_i(species, '\\s', 1) %>% str_sub(., 1, 4), # make short species names Genus species -> Gen.spe
    str_split_i(species, '\\s', 2) %>% str_sub(., 1, 4), sep = '.')) %>%
  left_join(traits_st %>% mutate(species = taxon_tpl) %>%
    dplyr::select(species, naturalisation_level),
  by = "species")




species.scores2

sites.scores2 <- scores(nmds2, display = 'sites') %>%
  as_tibble(rownames = 'sites.scores')

sites.scores2


species.scores2 <- na.omit(species.scores2)
species.scores2$naturalisation_level <- as.factor(species.scores2$naturalisation_level)




p2 <- ggplot(data = species.scores2,
  aes(x = NMDS1, y = NMDS2)) +
  geom_vline(xintercept = 0, col = "grey", linetype = "dashed") +
  geom_hline(yintercept = 0, col = "grey", linetype = "dashed") +
  # scale_shape_manual(values=c( 19, 1))+
  #  geom_point (data = species.scores2, size = 1, pch=19, aes(col=naturalisation_level)) +
  geom_text_repel(data = species.scores2,
    aes(x = NMDS1, y = NMDS2, label = short_name, colour = naturalisation_level),
    max.overlaps = 100) +
  scale_color_manual(values = c("#619CFF", "#F8766D", "#00BA38")) +
  xlim(-0.02, 0.02) + # ylim(-0.95,0.7)
  theme(axis.title = element_text(size = 13, face = "bold", colour = "black"),
    panel.background = element_blank(), panel.border = element_rect(fill = NA,
      colour = "grey30", size = 0.5),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 13, colour = "black"))

# scale_color_manual(values=c("#619CFF", "#F8766D", "#00BA38"))

p2




#  add standardised scores:
coord_cont2 <- as.data.frame(scores(fit2, "vectors")) %>%
  cbind(stand = fit2$vectors$arrows) %>%
  cbind(manual = fit2$vectors$arrows * sqrt(fit2$vectors$r)) %>%
  # cbind( permanona_R2=fit2$vectors$arrows*sqrt(PERM_mod2$R2)) %>%
  mutate(Variables = rownames(scores(fit2, "vectors"))) %>%
  mutate(Variables_new =
    fct_recode(Variables,
      ClimatePC = "pca1_clima",
      # Soil.C="Corg",
      Heat.stress = "heat_index",
      Stones = "cover_grv_stone",
      Herb.covr = "cover_herbs_sum",
      Litter = "cover_litter",
      Grazing = "grazing_intencity",
      Mowing = "mowing",
      Builtup = "build_up",
      Roads = "roads",
      Distr.Frqnc = "Disturbance.Frequency",
      Distr.Sevr = "Disturbance.Severity"))

coord_cont2

ordiArrowMul(fit2)


# rescale  all arrows to fill an ordination plot,
# where fill =  shows proportion of plot to be filled by the arrows
coord_cont_standrd2 <- coord_cont2 %>%
  mutate(stand.NMDS1 = stand.NMDS1 * ordiArrowMul(fit2, rescale = TRUE, fill = 0.2)) %>%
  mutate(stand.NMDS2 = stand.NMDS2 * ordiArrowMul(fit2, rescale = TRUE, fill = 0.2))

coord_cont_standrd2




# standardized
ggplot(data = species.scores2,
  aes(x = NMDS1, y = NMDS2)) +
  geom_vline(xintercept = 0, col = "grey", linetype = "dashed") +
  geom_hline(yintercept = 0, col = "grey", linetype = "dashed") +
  geom_segment(aes(x = 0, y = 0, xend = stand.NMDS1, yend = stand.NMDS2),
    data = coord_cont_standrd2, linewidth = 0.7, alpha = 1,
    colour = "gray55",
    arrow = arrow(length = unit(0.03, "npc"))) +
  geom_text(data = coord_cont_standrd2,
    aes(x = stand.NMDS1 + 0.001 * sign(stand.NMDS1),
      y = stand.NMDS2 + 0.001 * sign(stand.NMDS2),
      label = Variables_new),
    colour = "black", fontface = "bold", size = 4) +

  geom_text_repel(data = species.scores2,
    aes(x = NMDS1, y = NMDS2, label = short_name, colour = naturalisation_level),
    max.overlaps = 100 # , colour="#00BA38"
  ) +
  scale_color_manual(values = c("#619CFF", "#F8766D", "#00BA38")) +
  xlim(-0.031, 0.033) + # ylim(-0.95,0.7)
  theme(axis.title = element_text(size = 13, face = "bold", colour = "black"),
    panel.background = element_blank(), panel.border = element_rect(fill = NA,
      colour = "grey30", size = 0.5),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 13, colour = "black"),
    legend.position = "none")

factor(coord_cont$Variables_new)
#  ClimatePC   pH          Heat.stress    Stones
#  Herb.cov    Litter      Grazing        Mowing
#  Builtup     Roads       Dist.Freqnc    Dist.Sev


geom_text(data = coord_cont_standrd2,
  aes(x = stand.NMDS1 + 0.001 * sign(stand.NMDS1),
    y = stand.NMDS2 + 0.001 * sign(stand.NMDS2),
    label = Variables_new),
  colour = "black", fontface = "bold", size = 4,
  vjust = c(1, -0.1, 0.8, 0.8,
    1.7, 1, 1, 0.8,
    1, 0.8, 0.2, 0.7),
  hjust = c(0.5, 0, 0.9, 0.5,
    0.8, 0, 0, 0,
    0.1, 0.3, -0.1, 0.3)) +




  # unstandardized
  p2 +
  xlim(-0.5, 0.25) + # ylim(-0.95,0.7)
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
    data = coord_cont2, linewidth = 0.7, alpha = 1,
    colour = "darkgray",
    arrow = arrow(length = unit(0.03, "npc"))) +
  geom_text(data = coord_cont2,
    aes(x = NMDS1 + 0.01 * sign(NMDS1), y = NMDS2 + 0.01 * sign(NMDS2),
      label = Variables_new),
    colour = "black", fontface = "bold", size = 4,
    vjust = c(-0.3, 0.7, 0.7, 0.5,
      0.9, 0.9, 0.5, 0,
      0.5, 0.5, 0, 0),
    hjust = c(0.5, 0, 0.7, 0.9,
      0.9, 1, 0.3, 0.5,
      0.8, 0, 1, 0.3)) # adjust text positions


factor(coord_cont$Variables_new)
