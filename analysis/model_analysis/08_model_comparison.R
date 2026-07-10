library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)

source("./R/compare.R")


# This script is used to analyze model data. I calculate the deviation from modelled LAI to observed LAI.
# This is done by iterating over every model and applying the function "compare()".
# 
#The output is the results dataframe df_metrics, which contains statistical deviation metrics,
# and a boxplot showing the distribution of modelled LAI trends






#create empty tibble to store metrics of different models
df_metrics <- tibble(model = character(), slope = numeric(), MAE = numeric(), RMSE = numeric())


#create model name vector for iteration
models <- c("CABLE-POP", "ORCHIDEE", "LPJ-GUESS", "EDv3", "DLEM", "IBIS",
            "CLASSIC", "LPX-Bern", "JULES", "GDSTEM", "CLM6.0", "JSBACH", "E3SM", "CLM-FATES", "VISIT-UT")


# Define target regular grid outside the loop
target_grid <- terra::rast(
  xmin = -180, xmax = 180,
  ymin = 60,   ymax = 90,
  resolution = 0.5,
  crs = "EPSG:4326"
)




for (m in models) {
  df_metrics <- rbind(df_metrics, compare(m))

}

#save the results table, then reload it
saveRDS(df_metrics, "data/variables/df_metrics.rds")
df_metrics <- readRDS("data/variables/df_metrics.rds")




#compare MAE
ggplot(data = df_metrics, aes(x = model, y = MAE)) + geom_col() + 
  labs(title = "MAE of different Models") +
    coord_flip()


#load observations
obs_arcmean <- readRDS("data/variables/obs_arcmean_weighted.rds")

#fit linear model of observations
lm_obs <- lm(weighted_mean ~ year, data = obs_arcmean)

#get slope
obs_slope <- lm_obs$coefficients["year"]
obs_slope <- obs_slope * 40

#get the confidence interval of the linear model
ci <- confint(lm_obs)["year", ]
ci <- ci * 40
ci_low <- ci[1]
ci_high <- ci[2]


# Define colors (same as lineplot)
model_colors <- c(
  "CABLE-POP"  = "#FF6B9D",
  "CLASSIC"    = "#E69500",
  "CLM-FATES"  = "#B8860B",
  "CLM6.0"     = "#9DB800",
  "DLEM"       = "#4CAF50",
  "E3SM"       = "#2E7D32",
  "EDv3"       = "#00695C",
  "GDSTEM"     = "#00BCD4",
  "IBIS"       = "#29B6F6",
  "JSBACH"     = "#1565C0",
  "JULES"      = "#5C6BC0",
  "LPJ-GUESS"  = "#9C27B0",
  "LPX-Bern"   = "#CE93D8",
  "ORCHIDEE"   = "#FF80AB",
  "VISIT-UT"   = "#FF1493"
)


#save color scheme to use again later in other scripts
saveRDS(model_colors, "data/variables/model_colors.rds")


#boxplot with jitter points
boxplot_jitter <- ggplot(df_metrics, aes(x = "", y = slope)) +
  geom_boxplot(outlier.shape = NA,fill = "grey90", width = 0.4) +
  geom_jitter( aes(color = model), width = 0.05, size = 2) +
  geom_hline(yintercept = obs_slope, color = "red", linewidth = 0.4) +
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = ci_low, ymax = ci_high,
    fill = "red", alpha = 0.1
  ) +
  scale_color_manual(values = model_colors) +
  labs(
    title = "Distribution of Arctic LAI Trends by Model",
    subtitle = "Red Line: Slope of LAI Observations, 95% Confidence Interval",
    x = NULL,
    y = "Slope",
    color = "Model"
  ) +
  theme_bw()


