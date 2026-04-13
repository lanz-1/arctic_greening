library(ncdf4)
library(metR)
library(ggplot2)


#Read LAI data from netcdf file
borealforest <- metR::ReadNetCDF("data/spatial_means/LAI_AVHRR/LAI_AVHRR_global_borealforest.nc",
                                 out = "data.frame")

tundra <- metR::ReadNetCDF("data/spatial_means/LAI_AVHRR/LAI_AVHRR_global_tundra.nc", 
                           out = "data.frame")

arctic <- metR::ReadNetCDF("data/spatial_means/LAI_AVHRR/LAI_AVHRR_arctic.nc", 
                           out = "data.frame")



#plot data
plot_boreal <- ggplot(data= borealforest,
                      aes(x = time, y = LAI)) +
  geom_line() +
  geom_smooth(method = "lm") +        #fit linear regression model
  labs(title = "Boreal Forest LAI") 
plot_boreal



plot_tundra <- ggplot(data= tundra,
                      aes(x = time, y = LAI)) +
  geom_line() +
  geom_smooth(method = "lm") +
  labs(title = "Tundra LAI")
plot_tundra


plot_arctic <- ggplot(data= arctic,
                      aes(x = time, y = LAI)) +
  geom_line() +
  geom_smooth(method = "lm") +
  labs(title = "Arctic LAI")
plot_arctic




#plot all lines in one plot
plot_combined <- ggplot(mapping = aes(x = time, y = LAI)) + 
  geom_line(data = borealforest) +
  geom_smooth(data = borealforest, method = "lm", color = "red", linewidth = 0.5) +
  geom_line(data = tundra,
            color = "darkgreen") +
  geom_smooth(data = tundra, method = "lm", color = "red", linewidth = 0.5) +
  geom_line(data = arctic,
            color = "blue") +
  geom_smooth(data = arctic, method = "lm", color = "red", linewidth = 0.5) +
  labs(title = "LAI for boreal forest (black), tundra (green) and arctic (blue)")

plot_combined


#different approach (combined plots 1, 2 and 3)
cowplot::plot_grid(plot_boreal,plot_tundra, plot_arctic
                   , nrow = 3)



#add column with the year only
borealforest <- borealforest |> dplyr::mutate(time_corrected = lubridate::year(time))
tundra <- tundra |> dplyr::mutate(time_corrected = lubridate::year(time))
arctic <- arctic |> dplyr::mutate(time_corrected = lubridate::year(time))

#fit linear models for the three regions and extract regression coefficient (slope)
boreal_mod <- lm(LAI ~ time_corrected, data = borealforest)
slope_boreal <- coef(boreal_mod)[2]

tundra_mod <- lm(LAI ~ time_corrected, data = tundra)
slope_tundra <- coef(tundra_mod)[2]

arctic_mod <- lm(LAI ~ time_corrected, data = arctic)
slope_arctic <- coef(arctic_mod)[2]


#create a data frame with the above coefficients
LAI_trends <- data.frame(region = c("boreal forest", "tundra", "arctic"), LAI_regr_coef = c(slope_boreal, slope_tundra, slope_arctic))

#plot the regression coefficients
slope_plot <- ggplot(data = LAI_trends, aes(x = region, y = LAI_regr_coef)) + geom_col(fill = "darkgreen")
slope_plot
