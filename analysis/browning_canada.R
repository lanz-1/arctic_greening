library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(terra)
library(tidyterra)


# This script is used to look at what seems to be
# a regional browning trend in Canada, near the Great Slave Lake.

# Output:
# a map of the region showing trends in LAI
# a line plot showing the regional spatial mean LAI over time
# maps depicting LAI values in specific years





#read data
#from 31.12.1981 to 31.12.2021
LAI_spatial <- metR::ReadNetCDF("data/spatial/1982_2021_cat_transxy_wgrid_invertlat.nc") |>
  as_tibble()



#create time axis to add to data
time_axis <- 1982:2021

n_cells <- nrow(LAI_spatial) / 40

LAI_spatial <- LAI_spatial |>
  mutate(time = rep(time_axis, each = n_cells))


# Filter data spatially by coordinates. From 60°N to 65°N. From 120°W to 100°W.
LAI_canada <- LAI_spatial |>
  dplyr::filter(lat >= 60 & lat < 67 & lon >= -140 & lon < -100)




#plot maps for every year separately
for (yr in 1982:2021) {
  LAI_yr <- LAI_canada |> dplyr::filter(time == yr)
  p <- ggplot(LAI_yr) +
    geom_raster(aes(x = lon, y = lat, fill = LAI)) +
    scale_fill_gradient(low = "black", high = "green2", limits = c(0, 5), na.value = "grey90") +
    labs(title = paste0("Great Slave Lake: July LAI, ", yr)) +
    theme_bw()
  ggsave(paste0("fig/canada/canada_LAI_", yr, ".png"), plot = p, width = 8, height = 5, dpi = 300)
  
}




# Build one SpatRaster per year, then stack

raster_list_ca <- lapply(time_axis, function(yr) {
  LAI_canada |>
    dplyr::filter(time == yr) |>
    dplyr::select(lon, lat, LAI) |>
    terra::rast(type = "xyz", crs = "EPSG:4326")
})

r_canada <- terra::rast(raster_list_ca)
names(r_canada) <- time_axis




# Fit pixel-wise linear trend using terra::app() with lm
r_canada_trend <- terra::app(r_canada, fun = function(x) {
  if (all(is.na(x))) return(NA)
  fit <- lm(x ~ time_axis)
  return(coef(fit)[2])  # return slope
})

names(r_canada_trend) <- "LAI_trend"


# Plot trendline map
canada_trendmap <- ggplot() +
  geom_spatraster(data = r_canada_trend) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "darkgreen",
    limits = c(-0.02, 0.04), #set limits to -0.02 and 0.04 in order to have stronger colors.
    midpoint = 0,
    na.value = NA,
    name = "LAI trend\n(per year)") +
  labs(title = "Great Slave Lake: Linear trend in LAI (1982–2021)") +
  theme_bw()

canada_trendmap



# calculate spatial mean LAI per year


# Get cell area weights
cellsize <- terra::cellSize(r_canada, unit = "m")

# Calculate Arctic mean for every year. Weighted by cell size.
canada_mean <- terra::global(r_canada, "mean", weights = cellsize, na.rm = TRUE) |>
  as.data.frame()
canada_mean <- canada_mean |> dplyr::mutate(year = 1982:2021) #add year column for plot


plot_canada_mean <- ggplot(data = canada_mean, aes(x = year, y = weighted_mean)) + 
  geom_line() +
  labs(title = "Great Slave Lake: Mean LAI, 1982-2021",
       x = "Year",
       y = "LAI") +
  geom_smooth(method = "lm") +
  theme_minimal()

plot_canada_mean


# get the slope
linmod_canada <- lm(weighted_mean ~ year, data = canada_mean)
slope_canada <- coefficients(linmod_canada)[2]*40
