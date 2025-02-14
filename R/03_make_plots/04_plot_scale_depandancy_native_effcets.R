# Purpose: Plot scale-dependency of alien and native richness

library(tidyverse)
library(lme4)
library(performance)
library(car)
library(sjPlot)


set_theme(base = theme_bw(),
  axis.textsize.x = 0.9, axis.textsize.y = 0.9, axis.textcolor = "black",
  axis.title.color = "black", axis.title.size = 1.2,
  geom.linetype = 1) # legend.pos = "None",


# (1) Alien species,  %  ----
alien_dat <- read_csv("data/model_results_summary.csv") %>%
  filter(model_id == "native") %>% # filter only models that have native as a single predictor
  filter(remove_zeroes == TRUE) %>% # remove plots when alien are 0
  filter(response_var == "non_native_percent") %>%
  filter(!scale == 0.0001)


names(alien_dat)

alien_dat %>%
  dplyr::select(scale, slope, p_value_slope, r2_partial, r2m, r2c) %>%
  mutate(random = r2c - r2m)

mod1 <- lm(slope ~ log(scale), data = alien_dat)
mod2 <- lm(slope ~ poly(log(scale), 2), data = alien_dat)

anova(mod1, mod2)
Anova(mod2)

summary(mod1)

par(mfrow = c(2, 2))
plot(mod2)
par(mfrow = c(1, 1))

plot_model(mod2, type = "pred", terms = c("scale"), show.data = T)


mod1s_pred <- get_model_data(mod2, type = "pred", terms = "scale[0.001:100, by=0.001]")


ggplot(mod1s_pred, aes(log(x), predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    fill = "forestgreen", alpha = 0.1) +
  # geom_boxplot(alpha=0, lwd=0.6, outlier.shape = NA)+
  geom_point(data = alien_dat,
    aes(log(scale), slope), pch = 20, stroke = 1,
    col = "gray22", size = 3, alpha = 1) +
  labs(y = "Slope (effect of native SR)", x = expression(paste('Grain size, ', m^{2}))) +
  # guides(shape = "none") +
  theme(axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 10)) +
  geom_line(linetype = 1, linewidth = 1, col = "forestgreen") +
  scale_x_continuous(breaks = c(-6.907755, -4.605170, -2.302585, 0.000000, 2.302585, 4.605170),
    labels = c("0.001", "0.01", "0.1", "1", "10", "100"))



cor(alien_dat$slope, alien_dat$scale, use = "pairwise.complete.obs",
  method = "pearson")

Hmisc::rcorr(log(alien_dat$slope + 10), log(alien_dat$scale), type = "spearman")

Hmisc::rcorr(log(alien_dat$slope + 10), log(alien_dat$scale), type = "pearson")


# R2 plot

dodge_width <- 0.5

ggplot(alien_dat,
  aes(y = log(scale), x = r2m)) +
  geom_vline(xintercept = 0, color = "gray33", linetype = "dashed") +
  geom_point(position = position_dodge(width = dodge_width), size = 3,
    fill = "forestgreen", pch = 23) +
  geom_errorbarh(aes(xmin = 0, xmax = r2m), col = "forestgreen", linetype = "dashed",
    position = position_dodge(width = dodge_width), height = 0.1) +
  theme(legend.key = element_blank()) +
  # theme_bw()+
  theme(axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)) +
  #  theme(axis.title.x=element_text(vjust=-0.1), axis.title.y=element_text(vjust=2))+
  labs(y = expression(paste("Grain size, ", m^{2})),
    x = expression(paste("Variance explained, ", R^{2}))) +
  xlim(0, 0.11) +
  scale_y_continuous(breaks = c(-6.907755, -4.605170, -2.302585, 0.000000, 2.302585, 4.605170),
    labels = c("0.001", "0.01", "0.1", "1", "10", "100"))



#------------------------------------------------------------------------------#

# (2) Invasive species,  %  ----
invas_dat <- read_csv("data/model_results_summary.csv") %>%
  filter(model_id == "native") %>% # filter only models that have native as a single predictor
  filter(remove_zeroes == TRUE) %>% # remove plots when alian are 0
  filter(response_var == "invasive_percent") %>%
  filter(!scale %in% c(0.0001, 0.001, 0.01))


names(invas_dat)

invas_dat %>%
  dplyr::select(scale, slope, p_value_slope, r2_partial, r2m, r2c) %>%
  mutate(random = r2c - r2m)


mod2.1 <- lm(slope ~ log(scale), data = invas_dat)
mod2.2 <- lm(slope ~ poly(log(scale), 2), data = invas_dat)

anova(mod2.1, mod2.2)
Anova(mod2.1)
Anova(mod2.2)
summary(mod2.2)


plot_model(mod2.2, type = "pred", terms = c("scale"), show.data = T)


mod2s_pred <- get_model_data(mod2.2, type = "pred", terms = "scale[0.1:100, by=0.001]")


ggplot(mod2s_pred, aes(log(x), predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    fill = "brown", alpha = 0.1) +
  # geom_boxplot(alpha=0, lwd=0.6, outlier.shape = NA)+
  geom_point(data = invas_dat,
    aes(log(scale), slope), pch = 20, stroke = 1,
    col = "gray22", size = 3, alpha = 1) +
  labs(y = "Slope (effect of native SR)",
    x = expression(paste('Grain size, ', m^{2}))) +
  # guides(shape = "none") +
  theme(axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 10)) +
  geom_line(linetype = 1, linewidth = 1, col = "brown") +
  scale_x_continuous(limits = c(-6.907755, 5),
    breaks = c(-6.907755, -4.605170, -2.302585, 0.000000, 2.302585, 4.605170),
    labels = c("0.001", "0.01", "0.1", "1", "10", "100"))



cor(invas_dat$slope, invas_dat$scale, use = "pairwise.complete.obs",
  method = "pearson")

Hmisc::rcorr(log(invas_dat$slope + 10), log(invas_dat$scale), type = "spearman")

Hmisc::rcorr(log(invas_dat$slope + 10), log(invas_dat$scale), type = "pearson")


# R2 plot

dodge_width <- 0.5

ggplot(invas_dat,
  aes(y = log(scale), x = r2m)) +
  geom_vline(xintercept = 0, color = "gray33", linetype = "dashed") +
  geom_point(position = position_dodge(width = dodge_width), size = 3,
    fill = "brown", pch = 23) +
  geom_errorbarh(aes(xmin = 0, xmax = r2m), col = "brown", linetype = "dashed",
    position = position_dodge(width = dodge_width), height = 0.1) +
  theme(legend.key = element_blank()) +
  # theme_bw()+
  theme(axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)) +
  #  theme(axis.title.x=element_text(vjust=-0.1), axis.title.y=element_text(vjust=2))+
  labs(y = expression(paste("Grain size, ", m^{2})),
    x = expression(paste("Variance explained, ", R^{2}))) +
  xlim(0, 0.1) +
  scale_y_continuous(breaks = c(-9.210340, -6.907755, -4.605170, -2.302585, 0.000000, 2.302585, 4.605170),
    labels = c("0.0001", "0.001", "0.01", "0.1", "1", "10", "100"))
