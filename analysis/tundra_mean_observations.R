library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)
library(terra)



# This script is used to calculate annual OBSERVED LAI means in the TUNDRA biome.




#load data
LAI_spatial <- metR::ReadNetCDF("data/spatial/1982_2021_cat_transxy_wgrid_invertlat.nc") |>
  as_tibble() |>
  dplyr::rename(latitude = lat, longitude = lon)


#add time axis
time_axis <- 1982:2021
n_cells   <- nrow(LAI_spatial) / 40
LAI_spatial <- LAI_spatial |>
  mutate(time = rep(time_axis, each = n_cells))


# ── Build LAI reference grid from first time slice (global extent) ─────────
LAI_grid <- LAI_spatial |>
  dplyr::filter(time == min(time)) |>
  dplyr::select(longitude, latitude, LAI) |>
  terra::rast(type = "xyz", crs = "EPSG:4326")

# ── Build boreal mask raster ──────────────────────────────────────────────────
nc   <- ncdf4::nc_open("data/lct_regrid_for_trendy/tundra/LPX-Bern_S3_lai.nc_tundra_mask")
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

plot(mask_rast, main = "Raw boreal mask")



# Resample mask to LAI grid (nearest neighbour)
mask_resampled <- terra::resample(mask_rast, LAI_grid, method = "near")

# Define boreal target extent (≥ 60 N)
target_ext <- terra::ext(-180, 180, 60, 90)

# Crop mask once, outside the loop
mask_tundra <- terra::crop(mask_resampled, target_ext)

#Build masked raster stack for every year
years <- 1982:2021

raster_list <- lapply(years, function(yr) {
  r <- LAI_spatial |>
    dplyr::filter(time == yr) |>
    dplyr::select(longitude, latitude, LAI) |>
    terra::rast(type = "xyz", crs = "EPSG:4326")
  
  r_tundra <- terra::crop(r, target_ext)
  terra::mask(r_tundra, mask_tundra)
})

r_LAI <- terra::rast(raster_list)
names(r_LAI) <- years





# Now plot values for 2011 to see if it worked
tundra_2011  <- r_LAI[[which(years == 2000)]]

land <- terra::vect("data/spatial/land_surface/ne_10m_land.shp")

ggplot() +
  geom_spatraster(data = tundra_2011) +
  geom_spatvector(data = land, fill = NA, color = "grey30", linewidth = 0.2) +
  scale_fill_viridis_c(
    name     = "LAI (m²/m²)",
    option   = "viridis",
    na.value = "transparent"
  ) +
  coord_sf(ylim = c(60, 90)) +
  labs(
    title = paste("Observed Tundra LAI,", plot_year),
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ── Area-weighted annual mean ─────────────────────────────────────────────────
# cellSize only needs to be computed once (geometry is identical for all layers)
cellsize <- terra::cellSize(tundra_2011, unit = "m")

tundra_mean <- terra::global(r_LAI, "mean", weights = cellsize, na.rm = TRUE) |>
  as.data.frame() |>
  dplyr::mutate(year = years)


#save tundra mean
saveRDS(tundra_mean, "data/variables/tundra_mean_obs.rds")
tundra_mean <- readRDS("data/variables/tundra_mean_obs.rds")



#plot tundra mean over time
ggplot(tundra_mean, aes(x = year, y = weighted_mean)) +
  geom_line() +
  geom_smooth(method = "lm") +
  labs(
    title = "Tundra mean LAI, 1982-2021",
    x     = "Year",
    y     = "LAI (m²/m²)"
  ) +
  theme_bw()



# calculate the slope of the linear regression line
tundra_lm <- lm(weighted_mean ~ year, data = tundra_mean)
tundra_obs_slope <- tundra_lm$coefficients[2] * 40  #LAI increase over 40 years


