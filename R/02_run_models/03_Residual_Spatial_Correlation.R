# Purpose: Tests of Residual Spatial Correlation

# helpful information:
# https://theoreticalecology.wordpress.com/2012/05/12/spatial-autocorrelation-in-statistical-models-friend-or-foe/
# https://rdrr.io/cran/DHARMa/man/testSpatialAutocorrelation.html


rm(list = ls())
# Load libraries -----------------------------------------------------------
library(tidyverse)
library(car)
library(lme4)
library(lmerTest)
library(performance)


# Read & prepare data -------------------------------------------------------
# read disturbance data
disturbance_data <- read_csv("data-raw/Disturbn_commun_mean.csv") %>% 
  filter(scale == 100) %>%  dplyr::select(-subplot, -scale)

alien_data_100 <- read_csv("data/alien_dataset_all.csv") %>% 
  filter(type == "p_a" &  scale == 100) %>%
  left_join(disturbance_data, by = c("series")) %>% 
  mutate(
  grazing=log1p(grazing),
  cover_gravel_stones = cover_gravel_stones / 100,
  roads = roads / 100,
  builtup_1000m = log1p(builtup_1000m), # builtup_1000m / 100,
  builtup_250m = log1p(builtup_250m), #builtup_250m / 100,
  builtup_500m = log1p(builtup_500m), #builtup_500m / 100,
  cropland_1000m = log1p(cropland_1000m), # cropland_1000m / 100,
  cropland_250m = log1p(cropland_250m), # cropland_250m / 100,
  cropland_500m = log1p(cropland_500m), # cropland_500m / 100,
  cover_litter = cover_litter / 100,
  cover_herbs_sum = cover_herbs_sum / 100
)


alien_data_100


names(alien_data_100)

# non_native_percent -----------
## Mod 1 -------------------------------------------

m1a<- glmer(non_native_percent ~  
             pca1_clima + pH + microrelief + heat_index + 
 cover_litter + cover_herbs_sum + cover_gravel_stones + builtup_1000m + 
   cropland_1000m + grazing_intencity + mowing + abandonment +
   (1 | dataset), weights = total_species, family = binomial, 
            data = alien_data_100, 
 control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000)))



check_convergence(m1a)
car::Anova(m1a)
vif(m1a)

m1b<- glmer(non_native_percent ~  
              pca1_clima + pH + microrelief + heat_index + 
              cover_litter + cover_herbs_sum + cover_gravel_stones + builtup_500m + 
              cropland_500m + grazing_intencity + mowing + abandonment +
              (1 | dataset), weights = total_species, family = binomial, 
            data = alien_data_100, 
            control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000)))

check_convergence(m1b)
car::Anova(m1b)
vif(m1b)


m1c<- glmer(non_native_percent ~  
              pca1_clima + pH + microrelief + heat_index + 
              cover_litter + cover_herbs_sum + cover_gravel_stones + builtup_250m + 
              cropland_250m + grazing_intencity + mowing + abandonment +
              (1 | dataset), weights = total_species, family = binomial, 
            data = alien_data_100, 
            control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000)))

check_convergence(m1c)
car::Anova(m1c)
vif(m1c)

## Morans's I tests -----------------------------------------------------------
# Get residuals 

#  For lme4, re.form = NULL simulates residuals conditional on fitted random effects
## re.form specify which random effects to condition on when predicting.
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
dM_a <- 1 / as.matrix(dist(cbind(alien_data_100$lon, alien_data_100$lat)))
diag(dM_a) <- 0
str(dM_a)
# We have created a matrix where each off-diagonal entry [i, j] in the matrix is 
# equal to 1/(distance between point i and point j).

# (3) calculate Moran’s I (DHARMa works using ape package)

DHARMa::testSpatialAutocorrelation(res.sim_mod1a, distMat = dM_a)
DHARMa::testSpatialAutocorrelation(res.sim_mod1b, distMat = dM_a)
DHARMa::testSpatialAutocorrelation(res.sim_mod1c, distMat = dM_a)

#------------------------------------------------------------------------------#
## Mod 2 ----

mod2<- glmer(non_native_percent ~  pca1_clima + roads + Disturbance.Frequency + 
               Disturbance.Severity + (1 | dataset), weights = total_species, 
             family = binomial, 
             data = alien_data_100, 
             control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000)))



check_convergence(mod2)
vif(mod2)
car::Anova(mod2)
check_overdispersion(mod2)

### Morans's I tests -----------------------------------------------------------
# Get residuals

# (1) get randomized residuals.
res.sim_mod2 <- DHARMa::simulateResiduals(mod2, re.form = NULL)

# (2)  matrix of inverse distance weights.
dM_a

# (3) calculate Moran’s I (DHARMa works using ape package)
DHARMa::testSpatialAutocorrelation(res.sim_mod2, distMat = dM_a) 


#------------------------------------------------------------------------------#
## Mod 3 ----

mod3 <- glmer(non_native_percent ~  pca1_clima + native + (1 | dataset), weights = total_species, 
             family = binomial, 
             data = alien_data_100, 
             control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000)))


check_convergence(mod3)
car::Anova(mod3)


### Morans's I tests -----------------------------------------------------------
# Get residuals 

# (1) get randomized residuals.
res.sim_mod3 <- DHARMa::simulateResiduals(mod3, re.form = NULL)

# (2)  matrix of inverse distance weights.
dM_a 

# (3) calculate Moran’s I (DHARMa works using ape package)
testm3 <- DHARMa::testSpatialAutocorrelation(res.sim_mod3, distMat = dM_a)

attributes(testm3)
