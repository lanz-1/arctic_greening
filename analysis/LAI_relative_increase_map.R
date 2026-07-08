library(ncdf4)
library(ggplot2)
library(dplyr)
library(tidyterra)



# This script is used to plot relative changes in LAI. The relative change
# is calculated as:

#  LAI increase over 40 years / mean LAI from 1982 to 1991



#load dataset with LAI increases over 40 years. This is calculated in the file
# 'spatial_LAI_60_north.R'



# increase per year
LAI_trend_land <- terra::rast("data/variables/LAI_trend_land.tif")

# total increase over 40 years
LAI_trend_land <- LAI_trend_land *40



#load LAI data
LAI_north_60 <- readRDS("data/variables/LAI_north_60.rds")


# Filter years 1982 to 1992
LAI_1982_1991 <- LAI_north_60 |> dplyr::filter(time <= 1991)


# Build one SpatRaster per year, then stack
years <- 1982:1991


raster_list_91 <- lapply(years, function(yr) {
  LAI_1982_1991 |>
    dplyr::filter(time == yr) |>
    dplyr::select(lon, lat, LAI) |>
    terra::rast(type = "xyz", crs = "EPSG:4326")
})

r_LAI_91 <- terra::rast(raster_list_91)
names(r_LAI_91) <- years


#mask the oceans out
land <- terra::vect("data/spatial/land_surface/ne_10m_land.shp")
r_LAI_91 <- terra::mask(r_LAI_91, land)




#calculate 10-year mean for every pixel
obs_mean_91 <- app(r_LAI_91, fun = mean, na.rm = TRUE)



#divide: 40 year LAI increase / 10 year mean LAI from 1982 to 1991

relative_change <- LAI_trend_land / obs_mean_91

# change in percents
relative_change <- relative_change * 100



relative_change_map <- ggplot() +
  geom_spatraster(data = relative_change) +
    scale_fill_gradient2(
      low = "red", mid = "white", high = "darkgreen",
      limits = c(-50, 50), #set limits to -0.02 and 0.04 in order to have stronger colors.
      midpoint = 0,
      na.value = NA,
      name = "LAI change\n(%)") +
    labs(title = "Relative change in LAI", subtitle = "40 year increase relative to 1982-1991 mean")+ 
  theme_grey() +
  theme(
    panel.grid.major = element_line(colour = "gray")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8))



