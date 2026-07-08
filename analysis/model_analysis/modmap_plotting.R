library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)



#This script is used to create maps for modelled data.




#create model name vector for iteration
models <- c("CABLE-POP", "ORCHIDEE", "LPJ-GUESS", "EDv3", "DLEM", "IBIS",
                        "CLASSIC", "LPX-Bern", "JULES", "GDSTEM", "CLM6.0", "JSBACH", "E3SM", "CLM-FATES")



map_list <- list()

#create time axis to add to data
time_axis <- 1982:2021




for (dgvm in models) {

  #read data. LAI from one of the models.
  mLAI_spatial <- metR::ReadNetCDF(paste0("data/trendyv14_lai_july_mean/", dgvm, "_S3_lai.nc"),
                          vars = "lai") |> as_tibble()
  

  
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
  
  
  #some models have a 'lat' column, others a 'latitude' column. This causes errors. 
  
  #rename potential 'lat' column to 'latitude'
  if ("lat" %in% colnames(mLAI_spatial)){
    mLAI_spatial <- mLAI_spatial |> dplyr::rename(latitude = lat)
  }
  
  #same for 'lon' and 'longitude'
  if ("lon" %in% colnames(mLAI_spatial)){
    mLAI_spatial <- mLAI_spatial |> dplyr::rename(longitude = lon)
  }
  

# Now filter northern latitudes (60 degrees)
mLAI_north_60 <- mLAI_spatial |>
  dplyr::filter(latitude >= 60)


#change longitudes that range from 0 to 360 to longitude from -180 to 180
mLAI_north_60 <- mLAI_north_60 |>
  mutate(longitude = ifelse(longitude > 180, longitude - 360, longitude))


#add time axis
n_cells <- nrow(mLAI_north_60) / 40

mLAI_north_60 <- mLAI_north_60 |>
  mutate(time = rep(time_axis, each = n_cells))



#this is AI-generated. Creates a multi-layered spatraster object. One layer per year.

# Build one SpatRaster per year, then stack
years <- sort(unique(mLAI_north_60$time))

#AI code to handle the different grid format
raster_list <- lapply(years, function(yr) {
  df <- mLAI_north_60 |>
    dplyr::filter(time == yr) |>
    dplyr::select(longitude, latitude, lai)
  
  tryCatch({
    terra::rast(df, type = "xyz", crs = "EPSG:4326")
  }, error = function(e) {
    pts <- terra::vect(df, geom = c("longitude", "latitude"), crs = "EPSG:4326")
    grid_res <- max(
      median(lat_diffs, na.rm = TRUE),
      median(diff(sort(unique(df$longitude))), na.rm = TRUE)
    )
    template <- terra::rast(
      xmin = min(df$longitude), xmax = max(df$longitude),
      ymin = min(df$latitude),  ymax = max(df$latitude),
      resolution = grid_res,
      crs = "EPSG:4326"
    )
    terra::rasterize(pts, template, field = "lai", fun = mean)
  })
})




#raster_list <- lapply(years, function(yr) {
#  mLAI_north_60 |>
#    dplyr::filter(time == yr) |>
#    dplyr::select(longitude, latitude, lai) |>
#  terra::rast(type = "xyz", crs = "EPSG:4326")
#})

mr_LAI <- terra::rast(raster_list)
names(mr_LAI) <- years


# Create a numeric year vector matching layer order
year_nums <- as.numeric(names(mr_LAI))

# Fit pixel-wise linear trend using terra::app() with lm
mod_trend <- terra::app(mr_LAI, fun = function(x) {
  if (all(is.na(x))) return(NA)
  fit <- lm(x ~ year_nums)
  return(coef(fit)[2])  # return slope
})


names(mod_trend) <- "LAI_trend"
  
  
  # Plot trendline map
  mod_map <- ggplot() +
    geom_spatraster(data = mod_trend) +
    scale_fill_gradient2(
      low = "red", mid = "white", high = "darkgreen",
      limits = c(-0.02, 0.04), #set limits to -0.02 and 0.04 in order to have stronger colors.
      midpoint = 0,
      na.value = NA,
      name = "LAI trend\n(per year)") +
    labs(title = paste0(dgvm, ": Modelled LAI trend (1982-2021)")) +
    theme_grey() +
    theme(
      panel.grid.major = element_line(colour = "grey")) +
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8))
  
  #add plot to map list
  map_list[[dgvm]] <- mod_map
  
  
}

map_list


