# Purpose: Test for spatial autocorrelation in residuals of plant invasion models
# using Moran's I statistic across different invasion metrics and spatial scales.

# helpful information:
# https://theoreticalecology.wordpress.com/2012/05/12/spatial-autocorrelation-in-statistical-models-friend-or-foe/
# https://rdrr.io/cran/DHARMa/man/testSpatialAutocorrelation.html

# Input:
#   - data-raw/Disturbn_commun_mean.csv: Disturbance data at community level
#   - data/alien_dataset_all.csv: Processed alien species data
#
# Output:
#   - results/SpatAutocorResid_TableS6.csv: Spatial autocorrelation test results

# Load libraries -----------------------------------------------------------
library(tidyverse)
library(car)
library(lme4)
library(performance)


# -----------------------------------------------------------------------------#
# Load and Prepare Data
# -----------------------------------------------------------------------------#

# read disturbance data
disturbance_data <- read_csv("data-raw/Disturbn_commun_mean.csv") %>%
  filter(scale == 100) %>%
  dplyr::select(-subplot, -scale)

alien_data_100 <- read_csv("data/alien_dataset_all.csv") %>%
  filter(type == "p_a" & scale == 100) %>%
  left_join(disturbance_data, by = c("series")) %>%
  mutate(
    grazing = log1p(grazing),
    cover_gravel_stones = cover_gravel_stones / 100,
    roads = roads / 100,
    builtup_1000m = log1p(builtup_1000m), 
    builtup_250m = log1p(builtup_250m), 
    builtup_500m = log1p(builtup_500m), 
    cropland_1000m = log1p(cropland_1000m), 
    cropland_250m = log1p(cropland_250m), 
    cropland_500m = log1p(cropland_500m), 
    cover_litter = cover_litter / 100,
    cover_herbs_sum = cover_herbs_sum / 100
  )

# -----------------------------------------------------------------------------#
# Response 1: non_native_percent ----------------------------------------------
# -----------------------------------------------------------------------------#

# Model 1: Full environmental model -------------------------------------------
# We test this model for three different buffer sizes of land use variables:
# 1000m (m1a), 500m (m1b), 250m (m1c)

# Model 1a: Land use variables at 1000m buffer
m1a <- glmer(
  non_native_percent ~
    pca1_clima +
      pH +
      microrelief +
      heat_index +
      cover_litter +
      cover_herbs_sum +
      cover_gravel_stones +
      builtup_1000m +
      cropland_1000m +
      grazing_intencity +
      mowing +
      abandonment +
      (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(m1a)
car::Anova(m1a)
vif(m1a)

# Model 1b: Land use variables at 500m buffer
m1b <- glmer(
  non_native_percent ~
    pca1_clima +
      pH +
      microrelief +
      heat_index +
      cover_litter +
      cover_herbs_sum +
      cover_gravel_stones +
      builtup_500m +
      cropland_500m +
      grazing_intencity +
      mowing +
      abandonment +
      (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(m1b)
car::Anova(m1b)
vif(m1b)

# Model 1c: Land use variables at 250m buffer
m1c <- glmer(
  non_native_percent ~
    pca1_clima +
      pH +
      microrelief +
      heat_index +
      cover_litter +
      cover_herbs_sum +
      cover_gravel_stones +
      builtup_250m +
      cropland_250m +
      grazing_intencity +
      mowing +
      abandonment +
      (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(m1c)
car::Anova(m1c)
vif(m1c)

# Morans's I tests -----------------------------------------------------------

# For lme4, re.form = NULL simulates residuals conditional on fitted random effects
# re.form specify which random effects to condition on when predicting.
# If NULL, include all random effects; if NA or ~0, include no random effects.
# https://rdrr.io/cran/DHARMa/man/testSpatialAutocorrelation.html

# (1) get randomized residuals.
res.sim_mod1a <- DHARMa::simulateResiduals(m1a, re.form = NULL)
res.sim_mod1b <- DHARMa::simulateResiduals(m1b, re.form = NULL)
res.sim_mod1c <- DHARMa::simulateResiduals(m1c, re.form = NULL)

# randomized residuals from DHARMa is better than deviance residuals,
# because  deviance residuals are not homogeneous)
# https://stats.stackexchange.com/questions/507934/testing-the-spatiale-autocorrelation-on-the-residuals-of-the-mixed-effect-logist?newreg=e8a0041e387743139c3e9885b71d62eb
# https://rdrr.io/cran/DHARMa/man/testSpatialAutocorrelation.html

residuals(m1a, type = "deviance")

# (2)  generate a matrix of inverse distance weights.
# In the matrix, entries for pairs of points that are close together
# are higher than for pairs of points that are far apart.
# We use latitude and longitude for each plot, generate a distance matrix,
# then take inverse of the matrix values and replace the diagonal entries with zero:

# First, we remove NA values in the variables used in the model
data_for_distance_matrix <- alien_data_100 %>%
  dplyr::select(
    series,
    lon,
    lat,
    pca1_clima,
    pH,
    microrelief,
    heat_index,
    cover_litter,
    cover_herbs_sum,
    cover_gravel_stones,
    builtup_250m,
    cropland_250m,
    grazing_intencity,
    mowing,
    abandonment
  ) %>%
  drop_na()

# Generate the inverse distance matrix
# In the matrix, each off-diagonal entry [i, j] in the matrix is
# equal to 1/(distance between point i and point j).
dist_matrix_m1 <- 1 /
  as.matrix(dist(data_for_distance_matrix %>% dplyr::select(lon, lat)))
diag(dist_matrix_m1) <- 0

# (3) calculate Moran’s I (DHARMa works using ape package)

# For the 100 m buffer model
autocor_mod1a <- DHARMa::testSpatialAutocorrelation(
  res.sim_mod1a,
  distMat = dist_matrix_m1
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model1a")

# For the 500 m buffer model
autocor_mod1b <- DHARMa::testSpatialAutocorrelation(
  res.sim_mod1b,
  distMat = dist_matrix_m1
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model1b")

# For the 250 m buffer model
autocor_mod1c <- DHARMa::testSpatialAutocorrelation(
  res.sim_mod1c,
  distMat = dist_matrix_m1
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model1c")


# Model 2: Disturbance model ---------------------------------------------------

mod2 <- glmer(
  non_native_percent ~
    pca1_clima +
      roads +
      Disturbance.Frequency +
      Disturbance.Severity +
      (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(mod2)
vif(mod2)
car::Anova(mod2)
check_overdispersion(mod2)

# Morans's I tests -----------------------------------------------------------

# (1) get randomized residuals
res.sim_mod2 <- DHARMa::simulateResiduals(mod2, re.form = NULL)

# (2)  matrix of inverse distance weights.
dist_matrix_m2 <- 1 / as.matrix(dist(alien_data_100 %>%  dplyr::select(lon, lat)))
diag(dist_matrix_m2) <- 0

# (3) calculate Moran’s I
autocor_mod2 <- DHARMa::testSpatialAutocorrelation(
  res.sim_mod2,
  distMat = dist_matrix_m2
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model2")


# Model 3: Climate model -------------------------------------------------------

mod3 <- glmer(
  non_native_percent ~ pca1_clima + native + (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(mod3)
car::Anova(mod3)

# Morans's I tests -----------------------------------------------------------

# (1) get randomized residuals.
res.sim_mod3 <- DHARMa::simulateResiduals(mod3, re.form = NULL)

# (2)  matrix of inverse distance weights.
dist_matrix_m3 <- 1 / as.matrix(dist(alien_data_100 %>%  dplyr::select(lon, lat)))
diag(dist_matrix_m3) <- 0

# (3) calculate Moran’s I
autocor_mod3 <- DHARMa::testSpatialAutocorrelation(
  res.sim_mod3,
  distMat = dist_matrix_m3
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model3")

# Combine results from all non_native_percent models ---------------------------
alien_autocor <- autocor_mod1a %>%
  bind_rows(autocor_mod1b, autocor_mod1c, autocor_mod2, autocor_mod3) %>%
  mutate(response = "non_native_percent")

# -----------------------------------------------------------------------------#
# Response 2: invasive_percent -------------------------------------------------
# -----------------------------------------------------------------------------#

# Model 1: Full environmental model --------------------------------------------
# Model 1: Full environmental model -------------------------------------------
# We test this model for three different buffer sizes of land use variables:
# 1000m (inv_m1a), 500m (inv_m1b), 250m (inv_m1c)

# Model 1a: Land use variables at 1000m buffer
inv_m1a <- glmer(
  invasive_percent ~
    pca1_clima +
      pH +
      microrelief +
      heat_index +
      cover_litter +
      cover_herbs_sum +
      cover_gravel_stones +
      builtup_1000m +
      cropland_1000m +
      grazing_intencity +
      mowing +
      abandonment +
      (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(inv_m1a)
car::Anova(inv_m1a)
vif(inv_m1a)

# Model 1b: Land use variables at 500m buffer
inv_m1b <- glmer(
  invasive_percent ~
    pca1_clima +
      pH +
      microrelief +
      heat_index +
      cover_litter +
      cover_herbs_sum +
      cover_gravel_stones +
      builtup_500m +
      cropland_500m +
      grazing_intencity +
      mowing +
      abandonment +
      (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(inv_m1b)
car::Anova(inv_m1b)
vif(inv_m1b)

# Model 1c: Land use variables at 250m buffer
inv_m1c <- glmer(
  invasive_percent ~
    pca1_clima +
      pH +
      microrelief +
      heat_index +
      cover_litter +
      cover_herbs_sum +
      cover_gravel_stones +
      builtup_250m +
      cropland_250m +
      grazing_intencity +
      mowing +
      abandonment +
      (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(inv_m1c)
car::Anova(inv_m1c)
vif(inv_m1c)

# Morans's I tests -----------------------------------------------------------

# (1) get randomized residuals.
res.sim_inv_mod1a <- DHARMa::simulateResiduals(inv_m1a, re.form = NULL)
res.sim_inv_mod1b <- DHARMa::simulateResiduals(inv_m1b, re.form = NULL)
res.sim_inv_mod1c <- DHARMa::simulateResiduals(inv_m1c, re.form = NULL)

# (2) calculate Moran’s I (DHARMa works using ape package)
autocor_inv_mod1a <- DHARMa::testSpatialAutocorrelation(
  res.sim_inv_mod1a,
  distMat = dist_matrix_m1
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model1a")

autocor_inv_mod1b <- DHARMa::testSpatialAutocorrelation(
  res.sim_inv_mod1b,
  distMat = dist_matrix_m1
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model1b")

autocor_inv_mod1c <- DHARMa::testSpatialAutocorrelation(
  res.sim_inv_mod1c,
  distMat = dist_matrix_m1
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model1c")

# Model 2: Disturbance model ---------------------------------------------------

inv_mod2 <- glmer(
  invasive_percent ~
    roads + Disturbance.Frequency + Disturbance.Severity + (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(inv_mod2)
vif(inv_mod2)
car::Anova(inv_mod2)
check_overdispersion(inv_mod2)

# Morans's I tests -----------------------------------------------------------
# (1) get randomized residuals.
res.sim_inv_mod2 <- DHARMa::simulateResiduals(inv_mod2, re.form = NULL)

# (2) calculate Moran’s I (DHARMa works using ape package)
autocor_inv_mod2 <- DHARMa::testSpatialAutocorrelation(
  res.sim_inv_mod2,
  distMat = dist_matrix_m2
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model2")

# Model 3: Climate model -------------------------------------------------------
inv_mod3 <- glmer(
  invasive_percent ~ pca1_clima + native + (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(inv_mod3)
car::Anova(inv_mod3)

# Morans's I tests -----------------------------------------------------------
# (1) get randomized residuals.
res.sim_inv_mod3 <- DHARMa::simulateResiduals(inv_mod3, re.form = NULL)

# (2) calculate Moran’s I (DHARMa works using ape package)
autocor_inv_mod3 <- DHARMa::testSpatialAutocorrelation(
  res.sim_inv_mod3,
  distMat = dist_matrix_m3
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model3")

# Combine results from all invasive_percent models ---------------------------
invasive_autocor <- autocor_inv_mod1a %>%
  bind_rows(
    autocor_inv_mod1b,
    autocor_inv_mod1c,
    autocor_inv_mod2,
    autocor_inv_mod3
  ) %>%
  mutate(response = "invasive_percent")

# -----------------------------------------------------------------------------#
# Response 3: neophyte_percent -------------------------------------------------
# -----------------------------------------------------------------------------#

# Model 1: Full environmental model -------------------------------------------

# Buffer size 1000m
neoph_m1a <- glmer(
  neophyte_percent ~
    pca1_clima +
      pH +
      microrelief +
      heat_index +
      cover_litter +
      cover_herbs_sum +
      cover_gravel_stones +
      builtup_1000m +
      cropland_1000m +
      grazing_intencity +
      mowing +
      abandonment +
      (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(neoph_m1a)
car::Anova(neoph_m1a)
vif(neoph_m1a)

# Buffer size 500m
neoph_m1b <- glmer(
  neophyte_percent ~
    pca1_clima +
      pH +
      microrelief +
      heat_index +
      cover_litter +
      cover_herbs_sum +
      cover_gravel_stones +
      builtup_500m +
      cropland_500m +
      grazing_intencity +
      mowing +
      abandonment +
      (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(neoph_m1b)
car::Anova(neoph_m1b)
vif(neoph_m1b)

# Buffer size 250m
neoph_m1c <- glmer(
  neophyte_percent ~
    pca1_clima +
      pH +
      microrelief +
      heat_index +
      cover_litter +
      cover_herbs_sum +
      cover_gravel_stones +
      builtup_250m +
      cropland_250m +
      grazing_intencity +
      mowing +
      abandonment +
      (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(neoph_m1c)
car::Anova(neoph_m1c)
vif(neoph_m1c)

# Morans's I tests -----------------------------------------------------------
# (1) get randomized residuals.
res.sim_neoph_mod1a <- DHARMa::simulateResiduals(neoph_m1a, re.form = NULL)
res.sim_neoph_mod1b <- DHARMa::simulateResiduals(neoph_m1b, re.form = NULL)
res.sim_neoph_mod1c <- DHARMa::simulateResiduals(neoph_m1c, re.form = NULL)

# (2) calculate Moran’s I (DHARMa works using ape package)
autocor_neoph_mod1a <- DHARMa::testSpatialAutocorrelation(
  res.sim_neoph_mod1a,
  distMat = dist_matrix_m1
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model1a")

autocor_neoph_mod1b <- DHARMa::testSpatialAutocorrelation(
  res.sim_neoph_mod1b,
  distMat = dist_matrix_m1
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model1b")

autocor_neoph_mod1c <- DHARMa::testSpatialAutocorrelation(
  res.sim_neoph_mod1c,
  distMat = dist_matrix_m1
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model1c")

# Model 2: Disturbance model ---------------------------------------------------

neoph_mod2 <- glmer(
  neophyte_percent ~
    roads + Disturbance.Frequency + Disturbance.Severity + (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(neoph_mod2)
vif(neoph_mod2)
car::Anova(neoph_mod2)
check_overdispersion(neoph_mod2)

# Morans's I tests -----------------------------------------------------------
# (1) get randomized residuals.
res.sim_neoph_mod2 <- DHARMa::simulateResiduals(neoph_mod2, re.form = NULL)

# (2) calculate Moran’s I (DHARMa works using ape package)
autocor_neoph_mod2 <- DHARMa::testSpatialAutocorrelation(
  res.sim_neoph_mod2,
  distMat = dist_matrix_m2
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model2")

# Model 3: Climate model ----------------------------------------------------
neoph_mod3 <- glmer(
  neophyte_percent ~ pca1_clima + native + (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(neoph_mod3)
car::Anova(neoph_mod3)

# Morans's I tests -----------------------------------------------------------
# (1) get randomized residuals.
res.sim_neoph_mod3 <- DHARMa::simulateResiduals(neoph_mod3, re.form = NULL)

# (2) calculate Moran’s I (DHARMa works using ape package)
autocor_neoph_mod3 <- DHARMa::testSpatialAutocorrelation(
  res.sim_neoph_mod3,
  distMat = dist_matrix_m3
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model3")

# Combine results from all neophyte_percent models ---------------------------
neophyte_autocor <- autocor_neoph_mod1a %>%
  bind_rows(
    autocor_neoph_mod1b,
    autocor_neoph_mod1c,
    autocor_neoph_mod2,
    autocor_neoph_mod3
  ) %>%
  mutate(response = "neophyte_percent")

# -----------------------------------------------------------------------------#
# Response 4: archaeophyte_percent ---------------------------------------------
# -----------------------------------------------------------------------------#

# Model 1: Full environmental model -------------------------------------------

# Buffer size 1000m
archaeoph_m1a <- glmer(
  archaeophyte_percent ~
    pca1_clima +
      pH +
      microrelief +
      heat_index +
      cover_litter +
      cover_herbs_sum +
      cover_gravel_stones +
      builtup_1000m +
      cropland_1000m +
      grazing_intencity +
      mowing +
      abandonment +
      (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(archaeoph_m1a)
car::Anova(archaeoph_m1a)
vif(archaeoph_m1a)

# Buffer size 500m
archaeoph_m1b <- glmer(
  archaeophyte_percent ~
    pca1_clima +
      pH +
      microrelief +
      heat_index +
      cover_litter +
      cover_herbs_sum +
      cover_gravel_stones +
      builtup_500m +
      cropland_500m +
      grazing_intencity +
      mowing +
      abandonment +
      (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(archaeoph_m1b)
car::Anova(archaeoph_m1b)
vif(archaeoph_m1b)

# Buffer size 250m
archaeoph_m1c <- glmer(
  archaeophyte_percent ~
    pca1_clima +
      pH +
      microrelief +
      heat_index +
      cover_litter +
      cover_herbs_sum +
      cover_gravel_stones +
      builtup_250m +
      cropland_250m +
      grazing_intencity +
      mowing +
      abandonment +
      (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(archaeoph_m1c)
car::Anova(archaeoph_m1c)
vif(archaeoph_m1c)

# Morans's I tests -----------------------------------------------------------
# (1) get randomized residuals.
res.sim_archaeoph_mod1a <- DHARMa::simulateResiduals(
  archaeoph_m1a,
  re.form = NULL
)
res.sim_archaeoph_mod1b <- DHARMa::simulateResiduals(
  archaeoph_m1b,
  re.form = NULL
)
res.sim_archaeoph_mod1c <- DHARMa::simulateResiduals(
  archaeoph_m1c,
  re.form = NULL
)

# (2) calculate Moran’s I (DHARMa works using ape package)
autocor_archaeoph_mod1a <- DHARMa::testSpatialAutocorrelation(
  res.sim_archaeoph_mod1a,
  distMat = dist_matrix_m1
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model1a")

autocor_archaeoph_mod1b <- DHARMa::testSpatialAutocorrelation(
  res.sim_archaeoph_mod1b,
  distMat = dist_matrix_m1
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model1b")

autocor_archaeoph_mod1c <- DHARMa::testSpatialAutocorrelation(
  res.sim_archaeoph_mod1c,
  distMat = dist_matrix_m1
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model1c")

# Model 2: Disturbance model ---------------------------------------------------
archaeoph_mod2 <- glmer(
  archaeophyte_percent ~
    roads + Disturbance.Frequency + Disturbance.Severity + (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(archaeoph_mod2)
vif(archaeoph_mod2)
car::Anova(archaeoph_mod2)
check_overdispersion(archaeoph_mod2)

# Morans's I tests -----------------------------------------------------------
# (1) get randomized residuals.
res.sim_archaeoph_mod2 <- DHARMa::simulateResiduals(
  archaeoph_mod2,
  re.form = NULL
)

# (2) calculate Moran’s I (DHARMa works using ape package)
autocor_archaeoph_mod2 <- DHARMa::testSpatialAutocorrelation(
  res.sim_archaeoph_mod2,
  distMat = dist_matrix_m2
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model2")

# Model 3: Climate model ----------------------------------------------------
archaeoph_mod3 <- glmer(
  archaeophyte_percent ~ pca1_clima + native + (1 | dataset),
  weights = total_species,
  family = binomial,
  data = alien_data_100,
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 50000)
  )
)

check_convergence(archaeoph_mod3)
car::Anova(archaeoph_mod3)

# Morans's I tests -----------------------------------------------------------
# (1) get randomized residuals.
res.sim_archaeoph_mod3 <- DHARMa::simulateResiduals(
  archaeoph_mod3,
  re.form = NULL
)

# (2) calculate Moran’s I
autocor_archaeoph_mod3 <- DHARMa::testSpatialAutocorrelation(
  res.sim_archaeoph_mod3,
  distMat = dist_matrix_m3
)[c("statistic", "p.value")] %>%
  as.data.frame() %>%
  mutate(stats = c("Obs.I", "Exp.I", "sd")) %>%
  pivot_wider(names_from = "stats", values_from = "statistic") %>%
  pivot_longer(everything(), names_to = "statistic", values_to = "values") %>%
  mutate(model = "model3")

# Combine results from all archaeophyte_percent models ---------------------------
archaeophyte_autocor <- autocor_archaeoph_mod1a %>%
  bind_rows(
    autocor_archaeoph_mod1b,
    autocor_archaeoph_mod1c,
    autocor_archaeoph_mod2,
    autocor_archaeoph_mod3
  ) %>%
  mutate(response = "archaeophyte_percent")

#------------------------------------------------------------------------------#
# Combine results from all responses ------------------------------------------
#------------------------------------------------------------------------------#
spatial_autocor <-
  bind_rows(
    alien_autocor,
    invasive_autocor,
    neophyte_autocor,
    archaeophyte_autocor
  ) %>%
  pivot_wider(names_from = "statistic", values_from = "values") %>%
  relocate(p.value, .after = "sd") %>%
  mutate(
    Obs.I = round(Obs.I, 4),
    Exp.I = round(Exp.I, 4),
    sd = round(sd, 4),
    p.value = round(p.value, 2)
  ) %>% 
  mutate(
    response = case_when(
      response == "non_native_percent" ~ "Alien species, %",
      response == "archaeophyte_percent" ~ "Archaeophytes, %",
      response == "neophyte_percent" ~ "Neophytes, %",
      response == "invasive_percent" ~ "Invasive species, %"
    )
  )

spatial_autocor

write_csv(spatial_autocor, "results/SpatAutocorResid_TableS6.csv")

