library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)
library(terra)


# This file is used to create a boxplot showing the distribution of modelled LAI trends
# in the BOREAL biome

#run the following files first:
#'model_comparison.R'
#'boreal_biome.R'
#'boreal_mean_observations.R'


#load boreal modelled mean LAI. It is calculated in the file 'boreal_biome.R'.
bor_means <- readRDS("data/variables/results_boreal_final.rds")



#create model name vector for iteration
models <- c("CABLE-POP", "ORCHIDEE", "LPJ-GUESS", "EDv3", "DLEM", "IBIS",
            "CLASSIC", "LPX-Bern", "JULES", "GDSTEM", "CLM6.0", "JSBACH", "E3SM", "CLM-FATES", "VISIT-UT")


#create results tibble
boreal_slopes <- tibble(model = character(), slope = numeric())



#iterate over the file to select data by model
for (dgvm in models) {
  d <- bor_means |> dplyr::filter(model == dgvm)
  
  linmod <- lm(weighted_mean ~ year, data = d)
  
  #trend slope over the total 40 years
  slope <- coefficients(linmod)[2] * 40
  
  #store slope in results tibble
  boreal_slopes <- boreal_slopes |> add_row(model = dgvm, slope = slope)
}


#save modelled boreal slopes
saveRDS(boreal_slopes, "data/variables/boreal_slopes.rds")
boreal_slopes <- readRDS("data/variables/boreal_slopes.rds")



#load boreal observations
bor_mean_obs <- readRDS("data/variables/boreal_mean_obs.rds")

#calculate slope
linmod_obs_bor <- lm(weighted_mean ~ year, data = bor_mean_obs)
slope_bor_obs <- coefficients(linmod_obs_bor)[2] * 40


#get confidence interval
ci_bor <- confint(linmod_obs_bor)["year", ]
ci_bor <- ci_bor * 40
ci_low_bor <- ci_bor[1]
ci_high_bor <- ci_bor[2]

#load color scheme
model_colors <- readRDS("data/variables/model_colors.rds")

#boxplot with jitter points
boreal_boxplot <- ggplot(boreal_slopes, aes(x = "", y = slope)) +
  geom_boxplot(outlier.shape = NA,fill = "grey90", width = 0.4) +
  geom_jitter(aes(color = model), width = 0.05, size = 2) +
  geom_hline(yintercept = slope_bor_obs, color = "red", linewidth = 0.4) +
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = ci_low_bor, ymax = ci_high_bor,
    fill = "red", alpha = 0.1
  ) +
  scale_color_manual(values = model_colors) +
  labs(
    title = "Distribution of Boreal LAI Trends by Model",
    subtitle = "Red Line: Slope of Boreal LAI Observations, 95% Confidence Interval",
    x = NULL,
    y = "Slope",
    color = "Model"
  ) +
  theme_bw()

boreal_boxplot
