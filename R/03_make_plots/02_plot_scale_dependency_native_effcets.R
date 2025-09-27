# Purpose: Plot scale-dependency of species richness for different plant groups
# (alien, archaeophyte, neophyte, and invasive species).

## Output: The script generates plots (Fig 1 C,F,I,L) and statistical summaries that illustrate
# how the relationship between native and non-native species richness varies
# with spatial scale, for each group of interest.

# Scale-dependency is estimated as the slopes of native species richness effects 
# (obtained from GLMMs at each grain size) regressed against grain size 
# sing least-squares linear models 

library(tidyverse) # Data manipulation and plotting
library(lme4) # Mixed-effects models
library(performance) # Model performance metrics
library(car) # Anova and regression diagnostics
library(sjPlot) # Quick visualization of regression models and

# Set consistent plotting theme for all ggplot2 outputs
set_theme(
  base = theme_bw(),
  axis.textsize.x = 0.9,
  axis.textsize.y = 0.9,
  axis.textcolor = "black",
  axis.title.color = "black",
  axis.title.size = 1.2,
  geom.linetype = 1
)

# Helper: Custom x-axis breaks and labels for log-scale plots
log_scale_breaks <- c(
  -9.210340,
  -6.907755,
  -4.605170,
  -2.302585,
  0.000000,
  2.302585,
  4.605170
)
log_scale_labels <- c("0.0001", "0.001", "0.01", "0.1", "1", "10", "100")

# ---------------------------------------------------------------------------#
# Read and prepare data ------------------------------------------------------
# ---------------------------------------------------------------------------#

# Read all model results and filter for the relevant model
# We also remove the smallest scale because at this scale we do not have
# enough data points with non-zero alien species richness
all_models <- read_csv("data/model_results_summary.csv") |>
  filter(
    model_id == "climate_native",
    remove_zeroes == TRUE,
    predictor == "native",
    scale != 0.0001
  )

# Create subsets for different response variables

alien_dat <- all_models |>
  filter(
    response_var == "non_native_percent"
  )

archaeophyte_dat <- all_models |>
  filter(
    response_var == "archaeophyte_percent"
  )
# For neophytes and invasive we also remove the 2 other scale because at this scale
# we do not have enough data points with non-zero neophyte species richness
neoph_dat <- all_models |>
  filter(
    response_var == "neophyte_percent",
    !scale %in% c(0.0001, 0.001, 0.01),
  )

invas_dat <- all_models |>
  filter(
    response_var == "invasive_percent",
    !scale %in% c(0.0001, 0.001, 0.01)
  )

# ---------------------------------------------------------------------------#
# Response 1: alien species, %
# ---------------------------------------------------------------------------#

# Calculate contribution of random effects
alien_dat |>
  dplyr::select(scale, slope, p_value_slope, r2_partial, r2m, r2c) |>
  mutate(random = r2c - r2m)

# Fit linear and quadratic relationships of slope vs log(scale)
mod1 <- lm(slope ~ log(scale), data = alien_dat)
mod2 <- lm(slope ~ poly(log(scale), 2), data = alien_dat)

# Model comparison and diagnostics
anova(mod1, mod2)
Anova(mod2)
summary(mod1)
par(mfrow = c(2, 2))
plot(mod2)
par(mfrow = c(1, 1))

# Check plot of predicted quadratic relationship (with data)
plot_model(mod2, type = "pred", terms = c("scale"), show.data = TRUE)

# Get predicted values for plotting
mod1s_pred <- get_model_data(
  mod2,
  type = "pred",
  terms = "scale[0.001:100, by=0.001]"
)

## Fig 1 C ----
ggplot(mod1s_pred, aes(log(x), predicted)) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    fill = "forestgreen",
    alpha = 0.1
  ) +
  geom_point(
    data = alien_dat,
    aes(log(scale), slope),
    pch = 20,
    col = "gray22",
    size = 3
  ) +
  geom_line(linetype = 1, linewidth = 1, col = "forestgreen") +
  labs(
    y = "Slope (effect of native SR)",
    x = expression(paste(
      'Grain size, ',
      m^{
        2
      }
    ))
  ) +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  ) +
  scale_x_continuous(
    breaks = log_scale_breaks[-1],
    labels = log_scale_labels[-1]
  )

# Correlation analyses
cor(
  alien_dat$slope,
  alien_dat$scale,
  use = "pairwise.complete.obs",
  method = "pearson"
)
Hmisc::rcorr(log(alien_dat$slope + 10), log(alien_dat$scale), type = "spearman")
Hmisc::rcorr(log(alien_dat$slope + 10), log(alien_dat$scale), type = "pearson")

# R2 plot for alien species
dodge_width <- 0.5
ggplot(alien_dat, aes(y = log(scale), x = r2m)) +
  geom_vline(xintercept = 0, color = "gray33", linetype = "dashed") +
  geom_point(
    position = position_dodge(width = dodge_width),
    size = 3,
    fill = "forestgreen",
    pch = 23
  ) +
  geom_errorbarh(
    aes(xmin = 0, xmax = r2m),
    col = "forestgreen",
    linetype = "dashed",
    position = position_dodge(width = dodge_width),
    height = 0.1
  ) +
  theme(
    legend.key = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  labs(
    y = expression(paste(
      "Grain size, ",
      m^{
        2
      }
    )),
    x = expression(paste(
      "Variance explained, ",
      R^{
        2
      }
    ))
  ) +
  scale_y_continuous(
    breaks = log_scale_breaks[-1],
    labels = log_scale_labels[-1]
  )

# ---------------------------------------------------------------------------#
# Response 2: Archaeophyte species, %
# ---------------------------------------------------------------------------#

# Check contribution of random effects
archaeophyte_dat |>
  dplyr::select(scale, slope, p_value_slope, r2_partial, r2m, r2c) |>
  mutate(random = r2c - r2m)

mod2.1 <- lm(slope ~ log(scale), data = archaeophyte_dat)
mod2.2 <- lm(slope ~ poly(log(scale), 2), data = archaeophyte_dat)

anova(mod2.1, mod2.2)
Anova(mod2.2)
summary(mod2.2)
par(mfrow = c(2, 2))
plot(mod2.2)
par(mfrow = c(1, 1))

plot_model(mod2, type = "pred", terms = c("scale"), show.data = TRUE)
mod1s_pred <- get_model_data(
  mod2,
  type = "pred",
  terms = "scale[0.001:100, by=0.001]"
)

## Fig 1 F ----

ggplot(mod1s_pred, aes(log(x), predicted)) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    fill = "#5F9EA0",
    alpha = 0.1
  ) +
  geom_point(
    data = archaeophyte_dat,
    aes(log(scale), slope),
    pch = 20,
    col = "gray22",
    size = 3
  ) +
  geom_line(linetype = 1, linewidth = 1, col = "#5F9EA0") +
  labs(
    y = "Slope (effect of native SR)",
    x = expression(paste(
      'Grain size, ',
      m^{
        2
      }
    ))
  ) +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  ) +
  scale_x_continuous(
    breaks = log_scale_breaks[-1],
    labels = log_scale_labels[-1]
  )

cor(
  archaeophyte_dat$slope,
  archaeophyte_dat$scale,
  use = "pairwise.complete.obs",
  method = "pearson"
)
Hmisc::rcorr(
  log(archaeophyte_dat$slope + 10),
  log(archaeophyte_dat$scale),
  type = "spearman"
)
Hmisc::rcorr(
  log(archaeophyte_dat$slope + 10),
  log(archaeophyte_dat$scale),
  type = "pearson"
)

# R2 plot for archaeophytes
ggplot(archaeophyte_dat, aes(y = log(scale), x = r2m)) +
  geom_vline(xintercept = 0, color = "gray33", linetype = "dashed") +
  geom_point(
    position = position_dodge(width = dodge_width),
    size = 3,
    fill = "#5F9EA0",
    pch = 23
  ) +
  geom_errorbarh(
    aes(xmin = 0, xmax = r2m),
    col = "#5F9EA0",
    linetype = "dashed",
    position = position_dodge(width = dodge_width),
    height = 0.1
  ) +
  theme(
    legend.key = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  labs(
    y = expression(paste(
      "Grain size, ",
      m^{
        2
      }
    )),
    x = expression(paste(
      "Variance explained, ",
      R^{
        2
      }
    ))
  ) +
  scale_y_continuous(
    breaks = log_scale_breaks[-1],
    labels = log_scale_labels[-1]
  )

# ---------------------------------------------------------------------------#
# Response 3: Neophyte species, % -------------------------------------------
# ---------------------------------------------------------------------------#

neoph_dat |>
  dplyr::select(scale, slope, p_value_slope, r2_partial, r2m, r2c) |>
  mutate(random = r2c - r2m)

mod3.1 <- lm(slope ~ log(scale), data = neoph_dat)
mod3.2 <- lm(slope ~ poly(log(scale), 2), data = neoph_dat)

anova(mod3.1, mod3.2)
Anova(mod3.1)
summary(mod3.1)

#plot_model(mod3.1, type = "pred", terms = c("scale"), show.data = TRUE)
mod3s_pred <- get_model_data(
  mod3.1,
  type = "pred",
  terms = "scale[0.1:100, by=0.001]"
)

## Fig 1 I ----

ggplot(mod3s_pred, aes(log(x), predicted)) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    fill = "slateblue",
    alpha = 0.1
  ) +
  geom_point(
    data = neoph_dat,
    aes(log(scale), slope),
    pch = 20,
    col = "gray22",
    size = 3
  ) +
  geom_line(linetype = 1, linewidth = 1, col = "slateblue") +
  labs(
    y = "Slope (effect of native SR)",
    x = expression(paste(
      'Grain size, ',
      m^{
        2
      }
    ))
  ) +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  ) +
  scale_x_continuous(
    limits = c(-6.907755, 5),
    log_scale_breaks[-1],
    labels = log_scale_labels[-1]
  )

cor(
  neoph_dat$slope,
  neoph_dat$scale,
  use = "pairwise.complete.obs",
  method = "pearson"
)

# R2 plot for neophyte species
ggplot(neoph_dat, aes(y = log(scale), x = r2m)) +
  geom_vline(xintercept = 0, color = "gray33", linetype = "dashed") +
  geom_point(
    position = position_dodge(width = dodge_width),
    size = 3,
    fill = "slateblue",
    pch = 23
  ) +
  geom_errorbarh(
    aes(xmin = 0, xmax = r2m),
    col = "slateblue",
    linetype = "dashed",
    position = position_dodge(width = dodge_width),
    height = 0.1
  ) +
  theme(
    legend.key = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  labs(
    y = expression(paste(
      "Grain size, ",
      m^{
        2
      }
    )),
    x = expression(paste(
      "Variance explained, ",
      R^{
        2
      }
    ))
  ) +
  xlim(0, 0.1) +
  scale_y_continuous(
    breaks = log_scale_breaks,
    labels = log_scale_labels
  )

# ---------------------------------------------------------------------------#
# Response 4: Invasive species, % -------------------------------------------
# ---------------------------------------------------------------------------#

# Random effects contribution
invas_dat |>
  dplyr::select(scale, slope, p_value_slope, r2_partial, r2m, r2c) |>
  mutate(random = r2c - r2m)

mod4.1 <- lm(slope ~ log(scale), data = invas_dat)
mod4.2 <- lm(slope ~ poly(log(scale), 2), data = invas_dat)

anova(mod4.1, mod4.2)
Anova(mod4.2)
summary(mod4.2)

mod2s_pred <- get_model_data(
  mod4.2,
  type = "pred",
  terms = "scale[0.1:100, by=0.001]"
)

## Fig 1 L ----

ggplot(mod2s_pred, aes(log(x), predicted)) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    fill = "brown",
    alpha = 0.1
  ) +
  geom_point(
    data = invas_dat,
    aes(log(scale), slope),
    pch = 20,
    col = "gray22",
    size = 3
  ) +
  geom_line(linetype = 1, linewidth = 1, col = "brown") +
  labs(
    y = "Slope (effect of native SR)",
    x = expression(paste(
      'Grain size, ',
      m^{
        2
      }
    ))
  ) +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  ) +
  scale_x_continuous(
    limits = c(-6.907755, 5),
    breaks = log_scale_breaks[-1],
    labels = log_scale_labels[-1]
  )

cor(
  invas_dat$slope,
  invas_dat$scale,
  use = "pairwise.complete.obs",
  method = "pearson"
)

# R2 plot for invasive species
ggplot(invas_dat, aes(y = log(scale), x = r2m)) +
  geom_vline(xintercept = 0, color = "gray33", linetype = "dashed") +
  geom_point(
    position = position_dodge(width = dodge_width),
    size = 3,
    fill = "brown",
    pch = 23
  ) +
  geom_errorbarh(
    aes(xmin = 0, xmax = r2m),
    col = "brown",
    linetype = "dashed",
    position = position_dodge(width = dodge_width),
    height = 0.1
  ) +
  theme(
    legend.key = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  labs(
    y = expression(paste(
      "Grain size, ",
      m^{
        2
      }
    )),
    x = expression(paste(
      "Variance explained, ",
      R^{
        2
      }
    ))
  ) +
  xlim(0, 0.1) +
  scale_y_continuous(
    breaks = log_scale_breaks,
    labels = log_scale_labels
  )
