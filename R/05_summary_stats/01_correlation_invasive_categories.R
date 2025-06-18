# Calculate correlations between different categories of invasive species
# Correlation between archaeophytes and invasive species

# Load packages
library(tidyverse)

# Read the data --------------------------------------------------------------
species <- read_csv("data/database_analysis_summary.csv") |> 
  filter(type == "p_a")

# Corrlation between number of archaeophytes and number of invasive in each plot
ggplot(species, aes(x = neophyte, y = invasive)) +
  geom_point(position = position_jitter()) +
  geom_smooth(method = "lm")

# correlation = 0.4
cor(species$neophyte, species$invasive, method = "pearson")
