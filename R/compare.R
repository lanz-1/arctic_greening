library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)


#this function takes DGVM LAI data as input and compares it to LAI observations



compare <- function(dgvm) {
  #read data. LAI from one of the models.
  LAI <- metR::ReadNetCDF(paste0("data/trendyv14_lai_july_mean/", dgvm, "_S3_lai.nc")) |>
    as_tibble()
  
  #filter data from 1982 to 2022
  LAI <- LAI |> dplyr::filter(time >= as.Date("1981-12-31"), time <= as.Date("2021-12-31"))
  
  
  
  
  #some models have a 'lat' column, others a 'latitude' column. This causes errors. 
  
  #rename potential 'lat' column to 'latitude'
  if ("lat" %in% colnames(LAI)){
    LAI <- LAI |> dplyr::rename(latitude = lat)
  }
  
  #same for 'lon' and 'longitude'
  if ("lon" %in% colnames(LAI)){
    LAI <- LAI |> dplyr::rename(longitude = lon)
  }
  
  
  #filter latitudes above 60 degrees
  LAI <- LAI |> dplyr::filter("latitude" >= 60)
  
  
  
  #build multilayer spatraster with one layer per year
  years_f <- sort(unique(LAI$time))
  
  raster_list_f <- lapply(years_f, function(yr) {
    LAI |>
      dplyr::filter(time == yr) |>
      dplyr::select(longitude, latitude, lai) |>
      terra::rast(type = "xyz", crs = "EPSG:4326")
  })
  
  raster_LAI_f <- terra::rast(raster_list_f)
  names(raster_LAI_f) <- years_f
  
  
  #remove ocean surface cells
  land <- terra::vect("data/spatial/land_surface/ne_10m_land.shp")
  raster_LAI_f <- raster_LAI_f |> terra::mask(land)
  
  
  #calculate Arctic mean LAI over time
  arc_mean_f <- terra::global(raster_LAI_f, mean, na.rm = TRUE)
  arc_mean_f <- arc_mean_f |> dplyr::mutate(year = 1982:2021) #add year column for plot
  
  #plot mean modelled LAI from 1982-2022 (blue) and observations
  p_mean_LAI_f <- ggplot() +
    geom_line(data = arc_mean_f,
              aes(x = year, y = mean), color = "blue") +
    geom_line(data = arctic_mean, aes(x = time, y = LAI)) + #observations
    labs(title = paste0(dgvm, ": Arctic LAI 1982-2021")) 
  p_mean_LAI_f
  
  
  #fit linear regression model for Arctic mean LAI and calculate slope
  linmod <- lm(mean ~ year, data = arc_mean_f)
  slope_m <- coef(fit)[2]
  
  #calculate deviation from observed mean LAI
  
  #mean absolute error
  MAE <- mean(abs(arc_mean$mean -arc_mean_f$mean))

  #root mean square error
  RMSE <- sqrt(mean((arc_mean_f$mean - arc_mean$mean)^2))
  
  return(tibble(model = dgvm,
                slope = slope_m,
                MAE = MAE,
                RMSE = RMSE))
  
}





