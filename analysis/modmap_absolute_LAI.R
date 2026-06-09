library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)



#This script is used to create maps for modelled data.




#create model name vector for iteration
models <- c("CABLE-POP", "ORCHIDEE", "LPJ-GUESS", "EDv3", "DLEM", "IBIS",
            "CLASSIC", "LPX-Bern", "JULES", "GDSTEM", "CLM6.0", "JSBACH", "E3SM", "CLM-FATES")

models <- c("VISIT-UT")

map_list <- list()

#create time axis to add to data
time_axis <- 1982:2021




for (dgvm in models) {
  
  mLAI_spatial <- metR::ReadNetCDF(
    paste0("data/trendyv14_lai_july_mean/", dgvm, "_S3_lai.nc"),
    vars = "lai"
  ) |> as_tibble()
  
#  mLAI_spatial <- mLAI_spatial |> dplyr::filter(
#    time >= as.POSIXct("1982-01-01", tz = "UTC"),
#    time <= as.POSIXct("2021-12-31", tz = "UTC")
# )
  
#  mLAI_spatial <- mLAI_spatial |>
#    dplyr::mutate(year = as.numeric(substr(as.character(time), 1, 4))) |>
#    dplyr::filter(year %in% 1982:2021)
  
  
  
  #now filter data from 1982 to 2021
  
  
  #LAI has a different, numeric time-format ("years since 1700-7-15 00:00:00")
  if (is.numeric(mLAI_spatial$time)) {
    nc <- ncdf4::nc_open(paste0("data/trendyv14_lai_july_mean/", dgvm, "_S3_lai.nc"))
    time_units <- nc$dim$time$units
    ncdf4::nc_close(nc)
    
    base_year <- as.numeric(sub(".*since (\\d{4}).*", "\\1", time_units))
    
    mLAI_spatial <- mLAI_spatial |>
      dplyr::mutate(year = base_year + floor(time)) |>
      dplyr::filter(year %in% 1982:2021)
  } 
  
  
  #normal time formats
  else {
    mLAI_spatial <- mLAI_spatial |> dplyr::filter(
      time >= as.POSIXct("1982-01-01", tz = "UTC"),
      time <= as.POSIXct("2021-12-31", tz = "UTC")
    )
    
  }
  
  
  if ("lat" %in% colnames(mLAI_spatial))
    mLAI_spatial <- mLAI_spatial |> dplyr::rename(latitude = lat)
  if ("lon" %in% colnames(mLAI_spatial))
    mLAI_spatial <- mLAI_spatial |> dplyr::rename(longitude = lon)
  
  mLAI_north_60 <- mLAI_spatial |> dplyr::filter(latitude >= 60)
  
  mLAI_north_60 <- mLAI_north_60 |>
    mutate(longitude = ifelse(longitude > 180, longitude - 360, longitude))
  
  n_cells <- nrow(mLAI_north_60) / 40
  mLAI_north_60 <- mLAI_north_60 |>
    mutate(time = rep(time_axis, each = n_cells))
  
  years <- sort(unique(mLAI_north_60$time))
  
  raster_list <- lapply(years, function(yr) {
    df <- mLAI_north_60 |>
      dplyr::filter(time == yr) |>
      dplyr::select(longitude, latitude, lai)
    
    tryCatch({
      terra::rast(df, type = "xyz", crs = "EPSG:4326")
    }, error = function(e) {
      # Fix: compute lat/lon diffs locally from df
      lat_diffs  <- diff(sort(unique(df$latitude)))
      lon_diffs  <- diff(sort(unique(df$longitude)))
      grid_res   <- max(
        median(lat_diffs, na.rm = TRUE),
        median(lon_diffs, na.rm = TRUE)
      )
      pts <- terra::vect(df, geom = c("longitude", "latitude"), crs = "EPSG:4326")
      template <- terra::rast(
        xmin = min(df$longitude), xmax = max(df$longitude),
        ymin = min(df$latitude),  ymax = max(df$latitude),
        resolution = grid_res,
        crs = "EPSG:4326"
      )
      terra::rasterize(pts, template, field = "lai", fun = mean)
    })
  })
  
  mr_LAI       <- terra::rast(raster_list)
  names(mr_LAI) <- years
  
  
  
  
  #calculate 40-year mean for every pixel
  mean_LAI_40yrs <- app(mr_LAI, fun = mean, na.rm = TRUE)
  
  
  #plot
  mod_map <- ggplot() +
    geom_spatraster(data = mean_LAI_40yrs) +
    scale_fill_gradientn(
      colours = c("white", "#fee08b", "#91cf60", "#1a9850"),
      limits = c(0, 4),
      oob = scales::squish,
      na.value = NA,
      name = "Mean LAI\n(m²/m²)"
    ) +
    labs(
      title = paste0(dgvm, ": Mean July LAI (1982–2021)"),
      x = NULL, y = NULL
    ) +
    theme_grey() +
    theme(
      panel.grid.major = element_line(colour = "grey"),
      panel.border     = element_rect(colour = "black", fill = NA, linewidth = 0.8)
    )
  
  
  
  ggsave(paste0("fig/maps_abs_LAI_modelled/", dgvm, "_abs_lai.png"), plot = mod_map, width = 8, height = 5, dpi = 300)
  map_list[[dgvm]] <- mod_map
}

map_list
