library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)


#read data
#from 31.12.1981 to 31.12.2021
LAI_spatial <- metR::ReadNetCDF("data/spatial/1982_2021_cat_transxy_wgrid_invertlat.nc") |>
  as_tibble()


#create time axis to add to data
time_axis <- 1982:2021

n_cells <- nrow(LAI_spatial) / 40

LAI_spatial <- LAI_spatial |>
  mutate(time = rep(time_axis, each = n_cells))

#----experimental

#spatial data in northern latitudes for 2011
LAI_2011 <- LAI_spatial |> dplyr::filter((as.Date(time) == as.Date("2011-12-31")) & (lat >= 55))

#change data frame to SpatRaster object
r_LAI_2011 <- LAI_2011 |>
  dplyr::select(lon, lat, LAI) |>
  terra::rast(type = "xyz", crs = "EPSG:4326")


#plot LAI observation values
plot_LAI_2011 <- ggplot() + geom_spatraster(data = r_LAI_2011) + scale_fill_viridis_c(na.value = NA) +
  labs(title = "LAI Observations: Latitudes >= 55, 2011-12-31")


#select and plot the cells where LAI is above 1.5
m <- r_LAI_2011 > 1.5
ggplot() + geom_spatraster(data = m) + scale_fill_viridis_d(na.value = NA) +
  labs(title = "LAI above 1.5 (true/false), Latitudes >= 55, 2011-12-31")


LAI_1985 <- LAI_spatial |> dplyr::filter((as.Date(time) == as.Date("1985-12-31")) & (lat >= 55))
#change data frame to SpatRaster object
r_LAI_1985 <- LAI_1985 |>
  dplyr::select(lon, lat, LAI) |>
  terra::rast(type = "xyz", crs = "EPSG:4326")

#plot LAI observation values for 1985
plot_LAI_1985 <- ggplot() + geom_spatraster(data = r_LAI_1985) + scale_fill_viridis_c(na.value = NA) +
  labs(title = "LAI Observations: Latitudes >= 55, 1985-12-31")


#select and plot the cells where LAI is above 1.5
m <- r_LAI_1985 > 1.5
ggplot() + geom_spatraster(data = m) + scale_fill_viridis_d(na.value = NA) +
  labs(title = "LAI above 1.5 (true/false), Latitudes >= 55, 1985-12-31")

#----

# Filter northern latitudes
LAI_north <- LAI_spatial |>
  dplyr::filter(lat >= 55)

#this is AI-generated. Creates a multi-layered spatraster object. One layer per year.

# Build one SpatRaster per year, then stack
years <- sort(unique(LAI_north$time))

raster_list <- lapply(years, function(yr) {
  LAI_north |>
    dplyr::filter(time == yr) |>
    dplyr::select(lon, lat, LAI) |>
    terra::rast(type = "xyz", crs = "EPSG:4326")
})

r_LAI <- terra::rast(raster_list)
names(r_LAI) <- years



#end of AI section

#----
#plot year 2011, directly extracted from multi-layered r_LAI. 
plot_r_LAI_2011 <- ggplot() + geom_spatraster(data = r_LAI[["2011"]]) + scale_fill_viridis_c(na.value = NA) +
  labs(title = "LAI: Latitudes >= 55, 2011-12-31")
plot_r_LAI_2011

#see if the AI did it correctly (the plots should be the same)
cowplot::plot_grid(plot_r_LAI_2011, plot_LAI_2011, nrow = 2)

#----



# AI again
# Create a numeric year vector matching layer order
year_nums <- as.numeric(names(r_LAI))

# Fit pixel-wise linear trend using terra::app() with lm
r_LAI_trend <- terra::app(r_LAI, fun = function(x) {
  if (all(is.na(x))) return(NA)
  fit <- lm(x ~ year_nums)
  return(coef(fit)[2])  # return slope
})


names(r_LAI_trend) <- "LAI_trend"

# Plot
ggplot() +
  geom_spatraster(data = r_LAI_trend) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "darkgreen",
    limits = c(-0.02, 0.03), #set limits to -0.02 and 0.02 in order to have stronger colors.
    midpoint = 0,
    na.value = NA,
    name = "LAI trend\n(per year)") +
  labs(title = "Linear trend in LAI (1982–2021)") +
  theme_minimal()


#is there a latitude effect?

#write a for loop that calculates the spatial mean for every latitude degree. 
#The problem is that if ocean surface is included in the mean, latitudes with
#more ocean get a smaller mean. But we want to examine land surface only for LAI.

#create a land surface mask

#load land surface shapefile and create land mask
land <- terra::vect("data/spatial/land_surface/ne_10m_land.shp")
LAI_trend_land <- terra::mask(r_LAI_trend, land)


#create empty data frame to be filled by the loop
df <-  data.frame(lat = numeric(), mean = numeric())

#start looping over every latitude, calculating mean
for(i in seq(55,80)) {
  
  lat_slope <- LAI_trend_land |> filter((y >= i) & (y < (i+1)))  #select latitude degree-wise (e.g. select pixels between 55 degrees and 56 degrees)
  lat_slope_mean <- terra::global(lat_slope, mean, na.rm = TRUE)  #calculate mean across pixels of this latitude
  
  #add new row to data frame
  df <- rbind(df, data.frame(
    lat = i,
    mean = lat_slope_mean[1, 1]))
  
}
df


#plot spatial mean by latitude. Look for latitude effect
ggplot(data = df, aes(x = lat, y = mean)) + geom_point()
