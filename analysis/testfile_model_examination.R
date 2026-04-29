library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)


#read data. LAI from CABLE-POP_S3 model.
cable_pop <- metR::ReadNetCDF("data/trendyv14_lai_july_mean/CABLE-POP_S3_lai.nc") |>
  as_tibble()

#filter data from 1982 to 2022
cable_pop <- cable_pop |> dplyr::filter(time >= as.Date("1981-12-31"), time <= as.Date("2021-12-31"))

#filter latitudes above 60 degrees
cable_pop <- cable_pop |> dplyr::filter(latitude >= 60)


#build multilayer spatraster with one layer per year
years_f <- sort(unique(cable_pop$time))

raster_list_f <- lapply(years_f, function(yr) {
  cable_pop |>
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

#plot mean LAI from 1982-2022 with trendline
p_mean_LAI_f <- ggplot(data = arc_mean_f,
                       aes(x = year, y = mean)) +
  geom_line() +
  geom_smooth(method = "lm") +        #add linear regression line
  labs(title = parse0("CABLE-POP Arctic LAI 1982-2021") 
p_mean_LAI_f
