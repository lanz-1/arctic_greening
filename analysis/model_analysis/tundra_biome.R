library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)



# This script is used to plot the different modelled LAI means in the tundra biome.


# Create model name vector for iteration
models <- c("CLM6.0", "ORCHIDEE", "LPJ-GUESS", "EDv3", "DLEM", "IBIS",
            "CLASSIC", "LPX-Bern", "GDSTEM", "CABLE-POP", "JSBACH", "E3SM", "CLM-FATES", "JULES")



# VISIT-UT has a different time format and takes long to calculate. It was done separately.
# Use this code and append it to results later.
# models <- c("VISIT-UT")


# or try everything at once, with VISIT-UT included (could take long)

# models <- c("VISIT-UT", "CLM6.0", "ORCHIDEE", "LPJ-GUESS", "EDv3", "DLEM", "IBIS",
# "CLASSIC", "LPX-Bern", "GDSTEM", "CABLE-POP", "JSBACH", "E3SM", "CLM-FATES", "JULES")





#read land surface shapefile, later used for masking
land <- terra::vect("data/spatial/land_surface/ne_10m_land.shp")


# Define target regular grid outside the loop
target_grid <- terra::rast(
  xmin = -180, xmax = 180,
  ymin = 60,   ymax = 90,
  resolution = 0.5,
  crs = "EPSG:4326"
)



# Create results list
results <- list()

for (dgvm in models) {
  
  # Read data. LAI from one of the models.
  LAI <- metR::ReadNetCDF(paste0("data/trendyv14_lai_july_mean/", dgvm, "_S3_lai.nc"),
                          vars = "lai") |> as_tibble()
  
  #some models have a 'lat' column, others a 'latitude' column. This causes errors. 
  
  #rename potential 'lat' column to 'latitude'
  if ("lat" %in% colnames(LAI)){
    LAI <- LAI |> dplyr::rename(latitude = lat)
  }
  
  #same for 'lon' and 'longitude'
  if ("lon" %in% colnames(LAI)){
    LAI <- LAI |> dplyr::rename(longitude = lon)
  }
  

  
  
  #now create the Tundra mask. Multiply LAI values with the mask to select
  #only values inside the biome.
  m_tundra <- metR::ReadNetCDF(paste0("data/lct_regrid_for_trendy/tundra/", dgvm, "_S3_lai.nc_tundra_mask"), 
                               vars = "Majority_Land_Cover_Type_1") |> as_tibble()
  
  #rename colnames for the mask
  if ("lat" %in% colnames(m_tundra)){
    m_tundra <- m_tundra |> dplyr::rename(latitude = lat)
  }
  
  #rename colnames for the mask
  if ("lon" %in% colnames(m_tundra)){
    m_tundra <- m_tundra |> dplyr::rename(longitude = lon)
  }
  
 
  
  # Join mask and data on spatial coordinates, then multiply
  LAI <- LAI |>
    left_join(m_tundra, by = c("latitude", "longitude")) |>       
    mutate(lai_tundra = lai * Majority_Land_Cover_Type_1)
  
  
  
  
  
  # Filter latitudes above 60 degrees
  LAI <- LAI |> dplyr::filter(latitude >= 60)
  
  
  
  #handle the different time format of VISIT-UT. The code is from an AI
  if (!inherits(LAI$time, "POSIXct")) {
    nc <- ncdf4::nc_open(paste0("data/trendyv14_lai_july_mean/", dgvm, "_S3_lai.nc"))
    time_vals <- nc$dim$time$vals
    ncdf4::nc_close(nc)
    
    origin_year <- 1700
    actual_years <- as.integer(floor(origin_year + time_vals))  # 325 year values
    
    # Map each row's raw time value to the correct converted year
    LAI <- LAI |> dplyr::mutate(
      time = as.POSIXct(
        paste0(actual_years[match(time, time_vals)], "-07-15"),
        tz = "UTC"
      )
    )
  }
  
  
  
  # Filter data from 1982 to 2021
  LAI <- LAI |> dplyr::filter(
    time >= as.POSIXct("1982-01-01", tz = "UTC"),
    time <= as.POSIXct("2021-12-31", tz = "UTC")
  )
  
  
  
  
  # Build multilayer SpatRaster with one layer per year
  years_f <- sort(unique(LAI$time))
  raster_list_f <- lapply(years_f, function(yr) {
    r_df <- LAI |>
      dplyr::filter(time == yr) |>
      dplyr::select(longitude, latitude, lai_tundra) |>
      dplyr::mutate(
        longitude = round(longitude, 3),
        latitude  = round(latitude, 3)
      )
    
    # Try direct rasterization first
    r_out <- tryCatch({
      terra::rast(r_df, type = "xyz", crs = "EPSG:4326")
    }, error = function(e) {
      # Capture r_df explicitly into error handler scope
      r_df_local <- r_df
      pts <- terra::vect(r_df_local, geom = c("longitude", "latitude"), crs = "EPSG:4326")
      terra::rasterize(pts, target_grid, field = "lai_tundra", fun = mean)
    })
    
    # Resample onto common grid for comparability across models
    terra::resample(r_out, target_grid, method = "bilinear")
  })
  
  
  raster_LAI_f <- terra::rast(raster_list_f)
  names(raster_LAI_f) <- years_f
  
  # Remove ocean surface cells
  raster_LAI_f <- raster_LAI_f |> terra::mask(land)
  
  
  # Get cell area weights
  cellsize <- terra::cellSize(raster_LAI_f, unit = "m")
  
  # Calculate Arctic mean for every year. Weighted by cell size.
  arc_mean_f <- terra::global(raster_LAI_f, "mean", weights = cellsize, na.rm = TRUE) |>
    as.data.frame()
  arc_mean_f <- arc_mean_f |>
    dplyr::mutate(
      year  = as.integer(format(as.POSIXct(years_f), "%Y")),
      model = dgvm
    )
  
  # Save to results list
  results[[dgvm]] <- arc_mean_f
  
  
}


results_tundra <- dplyr::bind_rows(results)
  
  
#load tundra mean from VISIT-UT
tundra_visitut <- readRDS("data/variables/tundra_visitut.rds")

#add them to results table
results_tundra_final <- rbind(results_tundra, tundra_visitut)

#save and reload
saveRDS(results_tundra_final, "data/variables/results_tundra_final.rds")
readRDS("data/variables/results_tundra_final.rds")





#load tundra observations from file 'tundra_mean_observations.R'
tundra_mean_obs <- readRDS("data/variables/tundra_mean_obs.rds")



#define color scheme
model_colors <- readRDS("data/variables/model_colors.rds")


#line plot to compare different models
ggplot(results_tundra_final, aes(x = year, y = weighted_mean, color = model)) +
  geom_line(linewidth = 0.8) +
  geom_line(data = tundra_mean_obs, aes(x = year, y = weighted_mean), color = "black", linewidth = 1.0) +
  scale_color_manual(values = model_colors) +
  labs(
    x = "Year",
    y = "Tundra Mean LAI",
    color = "Model",
    title = "Tundra Mean LAI by Model, 1982–2021"
    #,subtitle = "Observation values in black"
  ) +
  theme_bw()











#----
#Example: plot tundra LAI in 2011. See if the masking worked.

  # Extract one year as a single SpatRaster layer
  lai_2011 <- raster_LAI_f[[which(format(as.POSIXct(years_f), "%Y") == "2011")]]
  
  ggplot() +
    geom_spatraster(data = lai_2011) +
    geom_spatvector(data = land, fill = NA, color = "grey30", linewidth = 0.2) +
    scale_fill_viridis_c(
      name = "LAI (m²/m²)",
      option = "viridis"
    ) +
    coord_sf(ylim = c(60, 90)) +
    labs(
      title = paste0(dgvm, " — Tundra LAI, July 2011"),
      x = NULL, y = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  
  
  
  