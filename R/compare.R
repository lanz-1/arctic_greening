library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)


#this function takes DGVM LAI data as input and compares it to LAI observations



compare <- function(dgvm) {
  #read data. LAI from one of the models.
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
  
  
  #now filter latitudes above 60 degrees
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
  
  
  #load observed Arctic mean LAI
  arc_mean_obs <- readRDS("data/variables/obs_arcmean_weighted.rds")
  
  #plot mean modelled LAI from 1982-2022 (blue) and observations
#p_mean_LAI_f <- ggplot() +
#   geom_line(data = arc_mean_f,
#             aes(x = year, y = mean), color = "blue") +
#   geom_line(data = arc_mean_obs, aes(x = time, y = LAI)) + #observations
#  labs(title = paste0(dgvm, ": Arctic LAI 1982-2021")) 
#p_mean_LAI_f
  
  
  #fit linear regression model for Arctic mean LAI and calculate slope
  linmod <- lm(weighted_mean ~ year, data = arc_mean_f)
  slope_m <- coef(linmod)[2]
  
  #calculate deviation from observed mean LAI
  
  #mean absolute error
  MAE <- mean(abs(arc_mean_obs$weighted_mean -arc_mean_f$weighted_mean))

  #root mean square error
  RMSE <- sqrt(mean((arc_mean_f$weighted_mean - arc_mean_obs$weighted_mean)^2))
  
  return(tibble(model = dgvm,
                slope = slope_m * 40,
                MAE = MAE,
                RMSE = RMSE))
}





