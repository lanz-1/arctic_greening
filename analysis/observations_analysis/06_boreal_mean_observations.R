library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)
library(terra)



# This script is used to calculate annual OBSERVED LAI means in the boreal biome
# Output is a line plot showing Boreal mean LAI from 1982-2021




#load LAI data
LAI_spatial <- readRDS("data/variables/LAI_north_60.rds")


# This is AI code to regrid the biome mask to the observations grid

# ── Build LAI reference grid from first time slice (global extent) ─────────
LAI_grid <- LAI_spatial |>
  dplyr::filter(time == min(time)) |>
  dplyr::select(longitude, latitude, LAI) |>
  terra::rast(type = "xyz", crs = "EPSG:4326")

# ── Build boreal mask raster ──────────────────────────────────────────────────
nc   <- ncdf4::nc_open("data/lct_regrid_for_trendy/boreal/LPX-Bern_S3_lai.nc_boreal_mask")
lon  <- ncdf4::ncvar_get(nc, "longitude")
lat  <- ncdf4::ncvar_get(nc, "latitude")
mask <- ncdf4::ncvar_get(nc, "Majority_Land_Cover_Type_1")
ncdf4::nc_close(nc)


mask_flipped <- mask[, rev(seq_len(ncol(mask)))]  # reverse lat (S→N becomes N→S)

mask_rast <- terra::rast(
  nrows = length(lat), ncols = length(lon),
  xmin  = -180, xmax  = 180,
  ymin  = -90,  ymax  = 90,
  crs   = "EPSG:4326"
)
terra::values(mask_rast) <- as.vector(mask_flipped)  # no t()
mask_rast[mask_rast != 1] <- NA



# Resample mask to LAI grid (nearest neighbour)
mask_resampled <- terra::resample(mask_rast, LAI_grid, method = "near")

# Define boreal target extent (≥ 60 N)
target_ext <- terra::ext(-180, 180, 60, 90)

# Crop mask once, outside the loop
mask_boreal <- terra::crop(mask_resampled, target_ext)

#Build masked raster stack for every year
years <- 1982:2021

raster_list <- lapply(years, function(yr) {
  r <- LAI_spatial |>
    dplyr::filter(time == yr) |>
    dplyr::select(longitude, latitude, LAI) |>
    terra::rast(type = "xyz", crs = "EPSG:4326")
  
  r_boreal <- terra::crop(r, target_ext)
  terra::mask(r_boreal, mask_boreal)
})

r_LAI <- terra::rast(raster_list)
names(r_LAI) <- years





# Now plot values for 2011 to see if it worked
plot_year <- 2011
bor_plot  <- r_LAI[[which(years == plot_year)]]

land <- terra::vect("data/spatial/land_surface/ne_10m_land.shp")

ggplot() +
  geom_spatraster(data = bor_plot) +
  geom_spatvector(data = land, fill = NA, color = "grey30", linewidth = 0.2) +
  scale_fill_viridis_c(
    name     = "LAI (m²/m²)",
    option   = "viridis",
    na.value = "transparent"
  ) +
  coord_sf(ylim = c(60, 90)) +
  labs(
    title = paste("Observed Boreal LAI,", plot_year),  # no hardcoded "July"
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



# annual mean weighted by cell size
cellsize <- terra::cellSize(bor_plot, unit = "m")

bor_mean <- terra::global(r_LAI, "mean", weights = cellsize, na.rm = TRUE) |>
  as.data.frame() |>
  dplyr::mutate(year = years)   # derive from years, not hardcoded range


#save boreal mean
saveRDS(bor_mean, "data/variables/boreal_mean_obs.rds")
bor_mean <- readRDS("data/variables/boreal_mean_obs.rds")



#plot boreal mean over time
plot_boreal_mean <- ggplot(bor_mean, aes(x = year, y = weighted_mean)) +
  geom_line() +
  geom_smooth(method = "lm") +
  scale_y_continuous(breaks = c(2.7, 2.8, 2.9)) +
  labs(
    title = "Boreal mean LAI, 1982-2021",
    x     = "Year",
    y     = "LAI"
  ) +
  theme_minimal()


plot_boreal_mean

# calculate the slope of the linear regression line
boreal_lm <- lm(weighted_mean ~ year, data = bor_mean)
boreal_obs_slope <- boreal_lm$coefficients[2] * 40  #LAI increase over 40 years

