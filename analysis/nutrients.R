library(ggplot2)
library(dplyr)



#important: Run the script 'model_comparison.R' first


# Nutrients: compare boxplots of C only vs. CN

# Information on nutrient cycle modelling is from Sitch et al. 2024

models_NC <- list("CABLE-POP", "CLM6.0", "DLEM", "JSBACH", "JULES", 
                 "LPJ-GUESS", "LPX-BERN", "ORCHIDEE")

models_C <- list("CLASSICn", "IBIS", "VISIT-UT")


#load data
#(if you can't load, make sure to run the script model_comparison.R first)

arctic_slopes <- readRDS("data/variables/df_metrics.rds")

#load color scheme
model_colors <- readRDS("data/variables/model_colors.rds")



