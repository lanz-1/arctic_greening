library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)
library(terra)


# This file is used to create a boxplot showing the distribution of modelled LAI trends
# in the BOREAL biome



#load tundra modelled mean LAI. It was calculated in the file 'tundra_biome.R'.
tundra_means <- readRDS("data/variables/results_tundra_final.rds")



#create model name vector for iteration
models <- c("CABLE-POP", "ORCHIDEE", "LPJ-GUESS", "EDv3", "DLEM", "IBIS",
            "CLASSIC", "LPX-Bern", "JULES", "GDSTEM", "CLM6.0", "JSBACH", "E3SM", "CLM-FATES", "VISIT-UT")


#create results tibble
tundra_slopes <- tibble(model = character(), slope = numeric())



#iterate over the file to select data by model
for (dgvm in models) {
  d <- tundra_means |> dplyr::filter(model == dgvm)
  
  linmod <- lm(weighted_mean ~ year, data = d)
  
  #trend slope over the total 40 years
  slope <- coefficients(linmod)[2] * 40
  
  #store slope in results tibble
  tundra_slopes <- tundra_slopes |> add_row(model = dgvm, slope = slope)
}


#save modelled tundra slopes
saveRDS(tundra_slopes, "data/variables/tundra_slopes.rds")
tundra_slopes <- readRDS("data/variables/tundra_slopes.rds")



#load tundra observations
tundra_mean_obs <- readRDS("data/variables/tundra_mean_obs.rds")

#calculate slope
linmod_obs_tundra <- lm(weighted_mean ~ year, data = tundra_mean_obs)
tundra_obs_slope <- coefficients(linmod_obs_tundra)[2] * 40

#get confidence interval
ci_tundra <- confint(linmod_obs_tundra)["year", ]
ci_tundra <- ci_tundra * 40
ci_low_tundra <- ci_tundra[1]
ci_high_tundra <- ci_tundra[2]


#boxplot with jitter points
tundra_boxplot <- ggplot(tundra_slopes, aes(x = "", y = slope)) +
  geom_boxplot(outlier.shape = NA,fill = "grey90", width = 0.4) +
  geom_jitter(aes(color = model), width = 0.05, size = 2) +
  geom_hline(yintercept = tundra_obs_slope, color = "red", linewidth = 0.4) +
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = ci_low_tundra, ymax = ci_high_tundra,
    fill = "red", alpha = 0.1
  ) +
  scale_color_manual(values = model_colors) +
  labs(
    title = "Distribution of Tundra LAI Trends by Model",
    subtitle = "Red Line: Slope of Tundra LAI Observations, 95% Confidence Interval",
    x = NULL,
    y = "Slope",
    color = "Model"
  ) +
  theme_bw()

tundra_boxplot
