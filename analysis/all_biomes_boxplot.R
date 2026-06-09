library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)





#create a big boxplot: Arctic, Tundra, Boreal Forest

# you have to run the following files first:
#boreal_boxplot.R
#model_comparison.R
#tundra_boxplot.R


#load data
arctic_slopes <- readRDS("data/variables/df_metrics.rds")
tundra_slopes <- readRDS("data/variables/tundra_slopes.rds")
boreal_slopes <- readRDS("data/variables/boreal_slopes.rds")


#add a column specifying the biome
arctic_slopes <- arctic_slopes |> dplyr::mutate(biome = "Arctic", MAE = NULL, RMSE = NULL)
tundra_slopes <- tundra_slopes |> dplyr::mutate(biome = "Tundra")
boreal_slopes <- boreal_slopes |> dplyr::mutate(biome = "Boreal")

#combine to one dataframe
all_slopes <- rbind(arctic_slopes, tundra_slopes, boreal_slopes)



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



#plot
ggplot(all_slopes, aes(x = biome, y = slope)) + 
  geom_boxplot(outlier.shape = NA, fill = "grey90", width = 0.4) +
  geom_jitter(aes(color = model), width = 0.05, size = 2) +
  geom_segment(
    aes(x = 0.8, xend = 1.2,
        y = obs_slope, yend = obs_slope),
    color = "red", linewidth = 0.4) +
  annotate(
    "rect",
    xmin = 0.8, xmax = 1.2,
    ymin = ci_low, ymax = ci_high,
    fill = "red", alpha = 0.15) +
  
  geom_segment(
    aes(x = 1.8, xend = 2.2,
        y = slope_bor_obs, yend = slope_bor_obs),
    color = "red", linewidth = 0.4) +
  annotate(
    "rect",
    xmin = 1.8, xmax = 2.2,
    ymin = ci_low_bor, ymax = ci_high_bor,
    fill = "red", alpha = 0.15) +
  
  geom_segment(
    aes(x = 2.8, xend = 3.2,
        y = tundra_obs_slope, yend = tundra_obs_slope),
    color = "red", linewidth = 0.4) +
  annotate(
    "rect",
    xmin = 2.8, xmax = 3.2,
    ymin = ci_low_tundra, ymax = ci_high_tundra,
    fill = "red", alpha = 0.15) +
  scale_color_manual(values = model_colors) +
  labs(title = "Modelled LAI trend slopes per biome",
       subtitle = "Observed slopes in red") +
  theme_bw()






