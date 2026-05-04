library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)

# Create model name vector for iteration
models <- c("CABLE-POP", "ORCHIDEE", "LPJ-GUESS", "EDv3", "DLEM", "IBIS",
         "CLASSIC", "LPX-Bern", "JULES", "GDSTEM", "CLM6.0", "JSBACH", "E3SM", "CLM-FATES", "LPJ-GUESS")




#VISIT-UT has a different time format and takes long to calculate. It was done separately.
  
#read land surface shapefile, later used for masking
land <- terra::vect("data/spatial/land_surface/ne_10m_land.shp")


# Define target regular grid outside the loop
target_grid <- terra::rast(
  xmin = -180, xmax = 180,
  ymin = 60,   ymax = 90,
  resolution = 0.5,
  crs = "EPSG:4326"
)



#load lai observations
arc_mean_obs <- readRDS("data/variables/obs_arcmean_weighted.rds")
arc_mean_obs <-  arc_mean_obs |> dplyr::mutate(model = "OBSERVED")



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
      dplyr::select(longitude, latitude, lai) |>
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
      terra::rasterize(pts, target_grid, field = "lai", fun = mean)
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

results_df <- dplyr::bind_rows(results)

#load Arctic mean LAI of VISIT-UT model
arcmean_visitut <- readRDS("data/variables/VISIT-UT_arcmean.rds") |> as_tibble() |>
  tidyr::unpack(everything())

#add to results dataframe
results_df <- rbind(results_df, arcmean_visitut)


#save
saveRDS(results_df, "data/variables/arcmean_lai_timeseries.rds")

#read
results_df <- readRDS("data/variables/arcmean_lai_timeseries.rds")



#line plot to compare different models
ggplot(results_df, aes(x = year, y = weighted_mean, color = model)) +
  geom_line(linewidth = 0.8) +
  geom_line(data = arc_mean_obs, aes(x = year, y = weighted_mean), color = "black", linewidth = 1.0) +
  labs(
    x = "Year",
    y = "Arctic mean LAI",
    color = "Model",
    title = "Arctic Mean LAI by Model, 1982–2021",
    subtitle = "Observation values in black"
  ) +
  theme_bw()





#compare annual observed mean to annual modelled mean


# Merge modelled and observed by year
scatter_df <- results_df |>
  dplyr::left_join(
    arc_mean_obs |> dplyr::select(year, mean) |> dplyr::rename(obs = mean),
    by = "year"
  )

# 1:1 line range
lims <- range(c(scatter_df$mean, scatter_df$obs), na.rm = TRUE)

# Create a list of plots, one per model
plot_list <- lapply(unique(scatter_df$model), function(m) {
  df_m <- scatter_df |> dplyr::filter(model == m)
  
  ggplot(df_m, aes(x = obs, y = mean)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.6) +
    geom_point(alpha = 0.7, size = 1.5, color = "blue") +
    coord_equal(xlim = c(0, 2), ylim = c(0, 2)) +
    labs(
      title = m,
      x = "Observed",
      y = "Modelled"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 8, face = "bold"),
      axis.text  = element_text(size = 7),
      axis.title = element_text(size = 7)
    )
})

cowplot::plot_grid(plotlist = plot_list, ncol = 4)
plot_list[5]
