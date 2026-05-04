library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)

source("./R/compare.R")


#create empty tibble to store metrics of different models
df_metrics <- tibble(model = character(), slope = numeric(), MAE = numeric(), RMSE = numeric())


#create model name vector for iteration
models <- c("CABLE-POP", "ORCHIDEE", "LPJ-GUESS", "EDv3", "DLEM", "IBIS",
            "CLASSIC", "LPX-Bern", "JULES", "GDSTEM", "CLM6.0", "JSBACH", "E3SM", "CLM-FATES", "LPJ-GUESS")


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

#save the results table, then load it
saveRDS("data/variables/df_metrics.rds")
df_metrics <- readRDS("data/variables/df_metrics.rds")


#boxplot showing distribution of slope means
boxplot_slope <- ggplot(data = df_metrics) + 
  geom_boxplot(aes(x = "", y = slope), width = 0.3) +
  geom_jitter(aes(x = "", y = slope), width = 0, height = 0, alpha = 0.3) +
  labs(title = "Distribution of modelled LAI trend slope (40 years)") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())


ggplot(data = df_metrics, aes(x = model, y = MAE)) + geom_col() + coord_flip()





