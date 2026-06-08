library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)
library(terra)




# Latitude bands. Plot data for latitudes 60-70 and for latitudes 70+

#read data
#from 31.12.1981 to 31.12.2021
LAI_spatial <- metR::ReadNetCDF("data/spatial/1982_2021_cat_transxy_wgrid_invertlat.nc") |>
  as_tibble()


#create time axis to add to data
years <- 1982:2021

n_cells <- nrow(LAI_spatial) / 40

LAI_spatial <- LAI_spatial |>
  mutate(time = rep(years, each = n_cells))


#Latitude bands:

# Filter data from 60 to 70 degrees
LAI_60_70 <- LAI_spatial |> dplyr::filter(lat >= 60) & (lat < 70)

# Filter latitudes above 70 degrees
LAI_70plus <- LAI_spatial |> dplyr::filter(lat >= 70)





#Create a multi-layered spatraster object. One layer per year.
# Build one SpatRaster per year, then stack

raster_list_60_70 <- lapply(years, function(yr) {
  LAI_60_70 |>
    dplyr::filter(time == yr) |>
    dplyr::select(lon, lat, LAI) |>
    terra::rast(type = "xyz", crs = "EPSG:4326")
})

r_LAI_60_70 <- terra::rast(raster_list_60_70)
names(r_LAI_60_70) <- years




# The same for 70+ latitudes
raster_list_70plus <- lapply(years, function(yr) {
  LAI_70plus |>
    dplyr::filter(time == yr) |>
    dplyr::select(lon, lat, LAI) |>
    terra::rast(type = "xyz", crs = "EPSG:4326")
})

r_LAI_70plus <- terra::rast(raster_list_70plus)
names(r_LAI_70plus) <- years






#calculate Arctic mean LAI over time. Start with lat 60-70.

#remove oceans first
#load land surface shapefile and create land mask
land <- terra::vect("data/spatial/land_surface/ne_10m_land.shp")

r_LAI_60_70_land <- terra::mask(r_LAI_60_70, land)

# Get cell area weights
cellsize <- terra::cellSize(r_LAI_60_70_land, unit = "m")

# Calculate Arctic mean for every year. Weighted by cell size.
arc_mean_60_70 <- terra::global(r_LAI_60_70_land, "mean", weights = cellsize, na.rm = TRUE) |>
  as.data.frame()
arc_mean_60_70 <- arc_mean_60_70 |> dplyr::mutate(year = 1982:2021) #add year column for plot



ggplot(arc_mean_60_70, aes(x = year, y = weighted_mean)) + 
  geom_line() + 
  geom_smooth(method = "lm") +
  labs(title = "Observed mean LAI, latitudes 60-70") +
  theme_bw()




# Do the same for lat 70+

r_LAI_70plus_land <- terra::mask(r_LAI_70plus, land)

# Get cell area weights
cellsize <- terra::cellSize(r_LAI_70plus_land, unit = "m")

# Calculate Arctic mean for every year. Weighted by cell size.
arc_mean_70plus <- terra::global(r_LAI_70plus_land, "mean", weights = cellsize, na.rm = TRUE) |>
  as.data.frame()
arc_mean_70plus <- arc_mean_70plus |> dplyr::mutate(year = 1982:2021) #add year column for plot



ggplot(arc_mean_70plus, aes(x = year, y = weighted_mean)) + 
  geom_line() + 
  geom_smooth(method = "lm") +
  labs(title = "Observed mean LAI, latitudes >= 70") +
  theme_bw()


