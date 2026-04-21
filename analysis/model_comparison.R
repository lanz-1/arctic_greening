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
            "CLASSIC", "LPX-Bern", "JULES", "GDSTEM")


for (m in models) {
  df_metrics <- rbind(df_metrics, compare(m))

}

df_metrics

ggplot(data = df_metrics, aes(x = model, y = MAE)) + geom_col() + coord_flip()

