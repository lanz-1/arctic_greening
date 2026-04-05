library(ncdf4)
library(metR)
library(ggplot2)

#in this file I plot GPP trends and calculate the linear regression coefficient 
#Read data from netcdf file
borealforest <- metR::ReadNetCDF("C:/Users/valen/Documents/Arctic/arctic_data/spatial_means/GPP_MODIS_OD17A3HGF.061/MOD17A3HGF.061_gpp_borealforest.nc",
                                 out = "data.frame")

tundra <- metR::ReadNetCDF("C:/Users/valen/Documents/Arctic/arctic_data/spatial_means/GPP_MODIS_OD17A3HGF.061/MOD17A3HGF.061_gpp_tundra.nc", 
                           out = "data.frame")

arctic <- metR::ReadNetCDF("C:/Users/valen/Documents/Arctic/arctic_data/spatial_means/GPP_MODIS_OD17A3HGF.061/MOD17A3HGF.061_gpp_arctic.nc", 
                           out = "data.frame")



#plot data
plot_boreal <- ggplot(data= borealforest,
                      aes(x = time, y = Gpp)) +
  geom_line() +
  geom_smooth(method = "lm") +        #draw linear trend line
  labs(title = "Boreal Forest GPP") 
plot_boreal



plot_tundra <- ggplot(data= tundra,
                      aes(x = time, y = Gpp)) +
  geom_line() +
  geom_smooth(method = "lm") +
  labs(title = "Tundra GPP")
plot_tundra


plot_arctic <- ggplot(data= arctic,
                      aes(x = time, y = Gpp)) +
  geom_line() +
  geom_smooth(method = "lm") +
  labs(title = "Arctic GPP")
plot_arctic




#plot all lines in one plot
plot_combined <- ggplot(mapping = aes(x = time, y = Gpp)) + 
  geom_line(data = borealforest) +
  geom_smooth(data = borealforest, method = "lm", color = "red", linewidth = 0.5) +
  geom_line(data = tundra,
            color = "darkgreen") +
  geom_smooth(data = tundra, method = "lm", color = "red", linewidth = 0.5) +
  geom_line(data = arctic,
            color = "yellow") +
  geom_smooth(data = arctic, method = "lm", color = "red", linewidth = 0.5) +
  labs(title = "GPP for boreal forest (black), tundra (green) and arctic (yellow)")

plot_combined


#different approach (combined plots 1, 2 and 3)
cowplot::plot_grid(plot_boreal,plot_tundra, plot_arctic
                   , nrow = 3)


#add column with the year only
borealforest <- borealforest |> dplyr::mutate(time_corrected = lubridate::year(time))
tundra <- tundra |> dplyr::mutate(time_corrected = lubridate::year(time))
arctic <- arctic |> dplyr::mutate(time_corrected = lubridate::year(time))

#fit linear models for the three regions and extract regression coefficient (slope)
boreal_mod <- lm(Gpp ~ time_corrected, data = borealforest)
slope_boreal <- coef(boreal_mod)[2]

tundra_mod <- lm(Gpp ~ time_corrected, data = tundra)
slope_tundra <- coef(tundra_mod)[2]

arctic_mod <- lm(Gpp ~ time_corrected, data = arctic)
slope_arctic <- coef(arctic_mod)[2]


#create a data frame with the above coefficients
GPP_trends <- data.frame(region = c("boreal forest", "tundra", "arctic"), GPP_regr_coef = c(slope_boreal, slope_tundra, slope_arctic))

#plot the regression coefficients
slope_plot <- ggplot(data = GPP_trends, aes(x = region, y = GPP_regr_coef)) + geom_col(fill = "darkgreen")
slope_plot



