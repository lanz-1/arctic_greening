library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(terra)
library(tidyterra)


# This script is used to examine what seems to be
# a regional browning trend in Siberia, near Yakutsk. Data: LAI observation values.

# Output:
# a map of the region showing trends in LAI
# a line plot showing the regional spatial mean LAI over time
# maps depicting LAI values in specific years


# before running this script, run 'spatial_LAI_60_north.R'



#read data
#Arctic LAI observations from 31.12.1981 to 31.12.2021 
LAI_north_60 <- readRDS("data/variables/LAI_north_60.rds")


# Filter data spatially by coordinates. From 100 to 140 East, 60 to 67 North
LAI_siberia <- LAI_north_60 |>
  dplyr::filter(lat >= 60  & lat < 67 & lon >= 100 & lon < 140)




#plot maps for every year separately
for (yr in 1982:2021) {
  LAI_yr <- LAI_siberia |> dplyr::filter(time == yr)
  p <- ggplot(LAI_yr) +
    geom_raster(aes(x = lon, y = lat, fill = LAI)) +
    scale_fill_gradient(low = "black", high = "green2", limits = c(0, 5), na.value = "grey90") +
    labs(title = paste0("Absolute LAI, ", yr)) +
      theme_bw()
  ggsave(paste0("fig/siberia/sib_LAI_", yr, ".png"), plot = p, width = 8, height = 5, dpi = 300)
  print(p)
}




# Build one SpatRaster per year, then stack

raster_list <- lapply(time_axis, function(yr) {
  LAI_siberia |>
    dplyr::filter(time == yr) |>
    dplyr::select(lon, lat, LAI) |>
    terra::rast(type = "xyz", crs = "EPSG:4326")
})

r_siberia <- terra::rast(raster_list)
names(r_siberia) <- time_axis




# Fit pixel-wise linear trend using terra::app() with lm
r_siberia_trend <- terra::app(r_siberia, fun = function(x) {
  if (all(is.na(x))) return(NA)
  fit <- lm(x ~ time_axis)
  return(coef(fit)[2])  # return slope
})

names(r_siberia_trend) <- "LAI_trend"


# Plot trendline map
siberia_trendmap <- ggplot() +
  geom_spatraster(data = r_siberia_trend) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "darkgreen",
    limits = c(-0.02, 0.04), #set limits to -0.02 and 0.04 in order to have stronger colors.
    midpoint = 0,
    na.value = NA,
    name = "LAI trend\n(per year)") +
  labs(title = "Yakutsk Region: Linear trend in LAI (1982–2021)") +
  theme_bw()
  
siberia_trendmap



# calculate spatial mean LAI per year


# Get cell area weights
cellsize <- terra::cellSize(r_siberia, unit = "m")

# Calculate Arctic mean for every year. Weighted by cell size.
siberia_mean <- terra::global(r_siberia, "mean", weights = cellsize, na.rm = TRUE) |>
  as.data.frame()
siberia_mean <- siberia_mean |> dplyr::mutate(year = 1982:2021) #add year column for plot


plot_siberia_mean <- ggplot(data = siberia_mean, aes(x = year, y = weighted_mean)) + 
              geom_line() +
              labs(title = "Yakutsk Region: Mean LAI, 1982-2021",
                   x = "Year",
                   y = "LAI") +
              geom_smooth(method = "lm") +
              theme_minimal()
                                                        
plot_siberia_mean


# get the slope
linmod_siberia <- lm(weighted_mean ~ year, data = siberia_mean)
slope_siberia <- coefficients(linmod_siberia)[2]*40





#Now focus on the browning hotspot

# Filter data spatially by coordinates. From 115 to 125 East, 62 to 66 North
LAI_siberia_brown <- LAI_spatial |>
  dplyr::filter(lat >= 62  & lat < 66 & lon >= 115 & lon < 125)


# Build one SpatRaster per year, then stack

raster_list_brown <- lapply(time_axis, function(yr) {
  LAI_siberia_brown |>
    dplyr::filter(time == yr) |>
    dplyr::select(lon, lat, LAI) |>
    terra::rast(type = "xyz", crs = "EPSG:4326")
})

r_siberia_brown <- terra::rast(raster_list_brown)
names(r_siberia_brown) <- time_axis



# Fit pixel-wise linear trend using terra::app() with lm
r_siberia_trend_brown <- terra::app(r_siberia_brown, fun = function(x) {
  if (all(is.na(x))) return(NA)
  fit <- lm(x ~ time_axis)
  return(coef(fit)[2])  # return slope
})

names(r_siberia_trend_brown) <- "LAI_trend"


# Plot trendline map
siberia_trendmap_brown <- ggplot() +
  geom_spatraster(data = r_siberia_trend_brown) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "darkgreen",
    limits = c(-0.02, 0.04), #set limits to -0.02 and 0.04 in order to have stronger colors.
    midpoint = 0,
    na.value = NA,
    name = "LAI trend\n(per year)") +
  labs(title = "Yakutsk Browning: Linear trend in LAI (1982–2021)") +
  theme_bw()

siberia_trendmap_brown








# calculate spatial mean LAI per year
# Get cell area weights
cellsize_brown <- terra::cellSize(r_siberia_brown, unit = "m")

# Calculate Arctic mean for every year. Weighted by cell size.
siberia_mean_brown <- terra::global(r_siberia_brown, "mean", weights = cellsize_brown, na.rm = TRUE) |>
  as.data.frame()
siberia_mean_brown <- siberia_mean_brown |> dplyr::mutate(year = 1982:2021) #add year column for plot


plot_siberia_mean_brown <- ggplot(data = siberia_mean_brown, aes(x = year, y = weighted_mean)) + 
  geom_line() +
  labs(title = "Yakutsk Browning: Mean LAI, 1982-2021",
       x = "Year",
       y = "LAI") +
  geom_smooth(method = "lm") +
  theme_minimal()

plot_siberia_mean_brown


# get the slope
linmod_siberia_brown <- lm(weighted_mean ~ year, data = siberia_mean_brown)
slope_siberia_brown <- coefficients(linmod_siberia_brown)[2]*40





