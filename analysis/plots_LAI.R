library(ncdf4)
library(metR)
library(ggplot2)


#read data. Spatial mean LAI in the Arctic
arctic_mean <- metR::ReadNetCDF("data/spatial_means/LAI_AVHRR/1982_2021_cat_transxy_wgrid_invertlat_arctic.nc", 
                                out = "data.frame")

#add time axis
arctic_mean <- arctic_mean |> dplyr::mutate(time = 1982:2021)

#plot data
plot_arctic_LAI <- ggplot(data = arctic_mean,
                      aes(x = time, y = LAI)) +
  geom_line() +
  geom_smooth(method = "lm") +        #add linear regression line
  labs(title = "Arctic mean LAI 1982-2021") 
plot_arctic_LAI


#fit linear model and extract regression coefficient (slope)

arctic_mod <- lm(LAI ~ time, data = arctic_mean)
slope_arctic <- coef(arctic_mod)[2]

#view significance linear regression model
summary(arctic_mod)
