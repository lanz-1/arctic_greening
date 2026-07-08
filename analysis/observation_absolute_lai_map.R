library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)


# This script is used to plot the observed absolute values of LAI

# Important: You need to run the script 'spatial_LAI_60_north.R' first


#read the raster data
r_LAI <- terra::rast("data/variables/r_LAI.tif")

#mask the oceans out
land <- terra::vect("data/spatial/land_surface/ne_10m_land.shp")
r_LAI_land <- terra::mask(r_LAI_loaded, land)


#calculate 40-year mean for every pixel
obs_mean_lai <- app(r_LAI_land, fun = mean, na.rm = TRUE)



#plot
observed_abolute_lai_map <- ggplot() +
  geom_spatraster(data = obs_mean_lai) +
  scale_fill_gradientn(
    colours = c("white", "#fee08b", "#91cf60", "#1a9850"),
    limits = c(0, 4),
    oob = scales::squish,
    na.value = NA,
    name = "Mean LAI\n(m²/m²)"
  ) +
  labs(
    title = "Mean Observed July LAI (1982-2021)"
  ) +
  theme_grey() +
  theme(
    panel.grid.major = element_line(colour = "grey"),
    panel.border     = element_rect(colour = "black", fill = NA, linewidth = 0.8)
  )


observed_abolute_lai_map



#----

# #generate r_LAI (only if you have not run 'spatial_LAI_60_north.R')
# 
# #read data
# #from 31.12.1981 to 31.12.2021
# LAI_spatial <- metR::ReadNetCDF("data/spatial/1982_2021_cat_transxy_wgrid_invertlat.nc") |>
#   as_tibble()
# 
# 
# #create time axis to add to data
# time_axis <- 1982:2021
# 
# n_cells <- nrow(LAI_spatial) / 40
# 
# LAI_spatial <- LAI_spatial |>
#   mutate(time = rep(time_axis, each = n_cells))
# 
# 
# # Filter northern latitudes (60 degrees)
# LAI_north_60 <- LAI_spatial |>
#   dplyr::filter(lat >= 60)
# 
# 
# # Build one SpatRaster per year, then stack
# years <- sort(unique(LAI_north_60$time))
# 
# raster_list <- lapply(years, function(yr) {
#   LAI_north_60 |>
#     dplyr::filter(time == yr) |>
#     dplyr::select(lon, lat, LAI) |>
#     terra::rast(type = "xyz", crs = "EPSG:4326")
# })
# 
# r_LAI <- terra::rast(raster_list)
# names(r_LAI) <- years
# 
# 
# #mask the oceans out
# land <- terra::vect("data/spatial/land_surface/ne_10m_land.shp")
# r_LAI_land <- terra::mask(r_LAI, land)

