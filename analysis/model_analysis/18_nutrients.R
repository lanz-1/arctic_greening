library(ggplot2)
library(dplyr)



#important: Run the script 'model_comparison.R' first


# Nutrients: compare boxplots of C only vs. CN

# Information on nutrient cycle modelling is from Sitch et al. 2024

models_NC <- list("CABLE-POP", "CLM6.0", "DLEM", "JSBACH", "JULES", 
                 "LPJ-GUESS", "LPX-BERN", "ORCHIDEE")

models_C <- list("CLASSIC", "IBIS", "VISIT-UT")


#load data
#(if you can't load, make sure to run the script model_comparison.R first)

arctic_slopes <- readRDS("data/variables/df_metrics.rds")

#load color scheme
model_colors <- readRDS("data/variables/model_colors.rds")



# now extract the slope values for the two groups 

slopes_NC <- tibble("model" = character(), "slope" = numeric())

for (m in arctic_slopes$model) {
  if (m %in% models_NC) {
    slopes_NC <- slopes_NC |> 
      add_row(model = m, slope = arctic_slopes$slope[arctic_slopes$model == m])
  }
}

slopes_NC

# add column specifying nutrient simulation
slopes_NC <- slopes_NC |> dplyr::mutate("nutrients" = "C + N")


# slope values for C only models
slopes_C <- tibble("model" = character(), "slope" = numeric())

for (m in arctic_slopes$model) {
  if (m %in% models_C) {
    slopes_C <- slopes_C |> 
      add_row(model = m, slope = arctic_slopes$slope[arctic_slopes$model == m])
  }
}


# add column specifying nutrient simulation
slopes_C <- slopes_C |> dplyr::mutate("nutrients" = "C")



# combine to one dataframe
slopes_nutrients <- rbind(slopes_NC, slopes_C)



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


#define color scheme

nutrient_model_colors <- c(
  "CABLE-POP"  = "#FF6B9D",
  "CLASSIC"    = "#E69500",
  "CLM6.0"     = "#9DB800",
  "DLEM"       = "#4CAF50",
  "IBIS"       = "#29B6F6",
  "JSBACH"     = "#1565C0",
  "JULES"      = "#5C6BC0",
  "LPJ-GUESS"  = "#9C27B0",
  "LPX-Bern"   = "#CE93D8",
  "ORCHIDEE"   = "#FF80AB",
  "VISIT-UT"   = "#FF1493"
)



#plot
ggplot(slopes_nutrients, aes(x = nutrients, y = slope)) + 
  geom_boxplot(outlier.shape = NA, fill = "grey90", width = 0.4) +
  geom_jitter(aes(color = model), width = 0.05, size = 2) +
  geom_hline(yintercept = obs_slope, color = "red", linewidth = 0.4) +
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = ci_low, ymax = ci_high,
    fill = "red", alpha = 0.15) +
  scale_color_manual(values = model_colors) +
  labs(title = "Modelled LAI trend slopes per biome",
       subtitle = "Observed slopes in red") +
  theme_bw()

