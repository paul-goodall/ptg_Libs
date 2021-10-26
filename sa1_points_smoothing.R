library(tidyverse)
require(FITSio)
library(astro)
library(leaflet)
library(gganimate)
library(magick)
library(tidyverse)

my_dir <- "/Volumes/DataVault/Projects/GeoSmoothing"
setwd(my_dir)

#geo_data2 <- read.csv("data/jtwcenras.csv")
#saveRDS(geo_data2, file="data/jtwcenras.rds")



obj_sf <- sf::st_read(dsn=here::here('data','1270055001_sa1_2016_aust_shape'),layer="SA1_2016_AUST")
geo_data2 <- readRDS("data/jtwcenras.rds")
neighbours <- read.csv("data/sa1neighbours.csv")
head(geo_data2)

# Choose some coords centred on Sydney:
radius_earth <- 6.371 * 10^6
# Sydney CBD
lon0 <- 151.207190
lat0 <- -33.867502
# 
lat0 <- -33.38751
lon0 <- 151.207190
radius_cutoff_km <- 13
radius_cutoff_degrees <- (radius_cutoff_km*1000/radius_earth)*(180/pi)


# Starting the analysis
sa1stats <- geo_data2 %>% group_by(sa1_7dig16) %>% summarise('lat'=mean(latitude), 'lon'=mean(longitude))
sa1stats$distance_to_point_degrees <- sqrt((sa1stats$lat - lat0)^2 + (sa1stats$lon - lon0)^2)

sa1sample <- sa1stats %>% filter(distance_to_point_degrees <= radius_cutoff_degrees)

geo_data_sample <- geo_data2 %>% filter(sa1_7dig16 %in% sa1sample$sa1_7dig16)
obj_sf_sample   <- obj_sf    %>% filter(SA1_7DIG16 %in% sa1sample$sa1_7dig16)


obj_sf_sample %>%
  ggplot() +
  geom_sf(aes(fill = AREASQKM16), lwd=line_width) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4)




## ====================================================
## GENERAL
## Choose the variables to smooth:

my_vars <- c("jtw_conn1","jtw_conn10","jtw_conn19","jtw_mbInd","jtw_mb_Res","jtw_ttl","median_mor","median_tot","median_ren","median_to0","average_nu","median_to1","average_ho")

# Go through each of the my_vars and ensure they're all in numeric format
for(nn in my_vars){
  geo_data_sample[[nn]] <- as.numeric(geo_data_sample[[nn]])
}

# Go through each feature in the table, and replace each NULL with an NA.
for(nn in names(geo_data_sample)){
  ii <- which(is.nan(geo_data_sample[[nn]]))
  geo_data_sample[[nn]][ii] <- NA
}

# =================
# Power Smoothing
dist_cutoff <- 2000
power_int <- 2
power_ext <- 3
# =================
# Gaussian Smoothing
sigma <- 500
nsigma_cutoff <- 4
# =================

# Two ways of doing this:
# A) Average the sa1s values and coords and then assign values to the points according to their positions
# B) Smooth the actual point values according to their positions and then redetermine the SA1 means.

# At the end you'll need to left join the values onto the shapefile dataset to plot the results.

## Since method A is a nonsense, I'll do Method B
## ====================================================
## METHOD B

group_vars <- c("longitude", "latitude")
all_vars <- c(group_vars, my_vars)
geo_data_sample$coord_tag <- paste0(geo_data_sample$longitude, ",", geo_data_sample$latitude)
geo_data_sample_deduped <- geo_data_sample %>% group_by(coord_tag, longitude, latitude) %>% summarise_at(my_vars, mean,  na.rm = TRUE)
geo_data_sample_lookup  <- geo_data_sample %>% group_by(coord_tag, sa1_7dig16) %>% summarise(n=n())

cc <- which(names(geo_data_sample_deduped) %in% my_vars)

geo_data_sample_power <- geo_data_sample_deduped
geo_data_sample_gauss <- geo_data_sample_deduped

## Loop through each data point:
for(nr in 1:dim(geo_data_sample_deduped)[1]){
  print(nr)
  my_point <- geo_data_sample_deduped[nr,]
  my_coord_id <- as.character(my_point$coord_tag[1])
  my_lon <- as.numeric(my_point$longitude[1])
  my_lat <- as.numeric(my_point$latitude[1])
  
  # Determine the distance of every point from this point:
  geo_data_sample_deduped$dist_metres <- haversine_distance(r=radius_earth, lat1=geo_data_sample_deduped$latitude, lat2=my_lat, 
                                                  lon1=geo_data_sample_deduped$longitude, lon2=my_lon)
  
  ii <- which(geo_data_sample_deduped$dist_metres <= dist_cutoff)
  my_sample <- geo_data_sample_deduped[ii,]
  ww <- (1 - (my_sample$dist_metres/dist_cutoff)^power_int)^power_ext
  
  ## Normalise weightings:
  ww <- ww/sum(ww)
  # FUTURE:  Need to account for the cases where the values are NA
  
  # Multiply each column by the weight:
  vals_sample_power <- my_sample[,cc]*t(ww)
  vals_sample_power <- vals_sample_power %>% summarise_all(sum,  na.rm = TRUE)
  geo_data_sample_power[nr,cc] <- vals_sample_power
  
  dist_cutoff_gauss <- sigma * nsigma_cutoff
  ii <- which(geo_data_sample_deduped$dist_metres <= dist_cutoff_gauss)
  my_sample <- geo_data_sample_deduped[ii,]
  ww <- exp(-((my_sample$dist_metres)/(sigma))^2)
  ww <- ww/sum(ww)
  # Multiply each column by the weight, without the first three columns:
  vals_sample_gauss <- my_sample[,cc]*t(ww)
  vals_sample_gauss <- vals_sample_gauss %>% summarise_all(sum,  na.rm = TRUE)
  geo_data_sample_gauss[nr,cc] <- vals_sample_gauss
  
}

## ====================================================

geo_data_sample_deduped <- geo_data_sample_deduped %>% left_join(geo_data_sample_lookup)
geo_data_sample_power <- geo_data_sample_power %>% left_join(geo_data_sample_lookup)
geo_data_sample_gauss <- geo_data_sample_gauss %>% left_join(geo_data_sample_lookup)

## ====================================================

geo_data_sample_deduped$sa1_7dig16 <- as.character(geo_data_sample_deduped$sa1_7dig16)
geo_data_sample_power$sa1_7dig16   <- as.character(geo_data_sample_power$sa1_7dig16)
geo_data_sample_gauss$sa1_7dig16   <- as.character(geo_data_sample_gauss$sa1_7dig16)

method1_means <- geo_data_sample_deduped %>% group_by(sa1_7dig16) %>% summarise_at(my_vars, mean,  na.rm = TRUE)
method1_means_power <- geo_data_sample_power %>% group_by(sa1_7dig16) %>% summarise_at(my_vars, mean,  na.rm = TRUE)
method1_means_gauss <- geo_data_sample_gauss %>% group_by(sa1_7dig16) %>% summarise_at(my_vars, mean,  na.rm = TRUE)

method1_means$sa1_7dig16 <- as.character(method1_means$sa1_7dig16)
method1_means_power$sa1_7dig16   <- as.character(method1_means_power$sa1_7dig16)
method1_means_gauss$sa1_7dig16   <- as.character(method1_means_gauss$sa1_7dig16)

vis_data_real  <- left_join(x=obj_sf_sample, y=method1_means, by = c('SA1_7DIG16' = 'sa1_7dig16'))
vis_data_power <- left_join(x=obj_sf_sample, y=method1_means_power, by = c('SA1_7DIG16' = 'sa1_7dig16'))
vis_data_gauss <- left_join(x=obj_sf_sample, y=method1_means_gauss, by = c('SA1_7DIG16' = 'sa1_7dig16'))

my_data <- list()
my_data$geo_data_sample <- geo_data_sample
my_data$geo_data_sample_deduped <- geo_data_sample_deduped
my_data$geo_data_sample_power <- geo_data_sample_power
my_data$geo_data_sample_gauss <- geo_data_sample_gauss
save(my_data, file="data/popran_13km.Rdata")


## VISUALISATION
line_width=0.2
vis_data_real %>%
  ggplot() +
  geom_sf(aes(fill = median_tot), lwd=line_width) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) + 
  geom_point(data=geo_data_sample_deduped, shape=19, aes(longitude, latitude, colour=median_tot), inherit.aes = F, alpha = 1, size = 0.6) + 
  scale_color_viridis_c(trans = "sqrt", alpha = 1) + 
  coord_sf() +
  labs(x = "Longitude", y = "Latitude", title = "Original Data")

vis_data_power %>%
  ggplot() +
  geom_sf(aes(fill = median_tot), lwd=line_width) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) + 
  geom_point(data=geo_data_sample_power, shape=19, aes(longitude, latitude, colour=median_tot), inherit.aes = F, alpha = 1, size = 0.6) + 
  scale_color_viridis_c(trans = "sqrt", alpha = 1) + 
  coord_sf() +
  labs(x = "Longitude", y = "Latitude", title = "Power-law smoothing")

vis_data_gauss %>%
  ggplot() +
  geom_sf(aes(fill = median_tot), lwd=line_width) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) + 
  geom_point(data=geo_data_sample_gauss, shape=19, aes(longitude, latitude, colour=median_tot), inherit.aes = F, alpha = 1, size = 0.6) + 
  scale_color_viridis_c(trans = "sqrt", alpha = 1) + 
  coord_sf() +
  labs(x = "Longitude", y = "Latitude", title = "Gaussian Smoothing")

leaflet(vis_data) %>%
  addTiles() %>%
  addPolylines(stroke = TRUE, weight = 1,color = "blue")


## ====================================================
## VISUALISATION

line_width=0.2

dist_cutoff <- 2000
power_int <- 2
power_ext <- 3
# =================
# Gaussian Smoothing
sigma <- 500
nsigma_cutoff <- 4


##  Real plots
for(my_n in my_vars){
  my_plot_info <- paste0(my_n, "_methodB_real")
  my_plot_name <- paste0(my_dir, "/plots/", my_plot_info, ".png")
  my_plot_title <- paste0("Raw data: ", my_plot_info)
  plot_map(vis_data_real, my_points=geo_data_sample_deduped, my_n, my_plot_name, my_plot_title)
}

##  Power smoothed plots
for(my_n in my_vars){
  my_plot_info <- paste0(my_n, "_methodB_d_", dist_cutoff, "_p1_", power_int, "_p2_", power_ext)
  my_plot_name <- paste0(my_dir, "/plots/", my_plot_info, ".png")
  my_plot_title <- paste0("Power-smoothed data: ", my_plot_info)
  plot_map(vis_data_power, my_points=geo_data_sample_power, my_n, my_plot_name, my_plot_title)
}

##  Gaussian smoothed plots
for(my_n in my_vars){
  my_plot_info <- paste0(my_n, "_methodB_sigma_", sigma, "_ns_", nsigma_cutoff)
  my_plot_name <- paste0(my_dir, "/plots/", my_plot_info, ".png")
  my_plot_title <- paste0("Gaussian-smoothed data: ", my_plot_info)
  plot_map(vis_data_gauss, my_points=geo_data_sample_gauss, my_n, my_plot_name, my_plot_title)
}


# ===========================
plot_map <- function(my_df, my_points, my_varname, my_plotfile, my_title){
  my_df$value <- my_df[[my_varname]]
  my_points$value <- my_points[[my_varname]]
  gg <- my_df %>%
    ggplot() +
    geom_sf(aes(fill = value), lwd=line_width) +
    scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
    theme(legend.position = c(1.1, 0.5), plot.margin=unit(c(0.5,2.5,0.5,0.5),"cm")) +
    labs(x = "Longitude", y = "Latitude", title = my_title) + 
    geom_point(data=my_points, shape=19, aes(longitude, latitude, colour=value), inherit.aes = F, alpha = 1, size = 0.6) + 
    scale_color_viridis_c(trans = "sqrt", alpha = 1) + 
    coord_sf()
  
  ggsave(plot = gg, filename = my_plotfile, device = "png")
  gg
}
# ===========================
imgs <- c()
for(my_n in my_vars){
  ## list file names and read in
  my_ims <- Sys.glob(paste0(my_dir, "/plots/", my_n,"_methodB*png"))
  my_ims <- my_ims[c(2,1,3)]
  imgs <- c(imgs, my_ims)
}

img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 1)

## save to disk
image_write(image = img_animated, path = paste0(my_dir, "/plots/Rural_Points_Smoothing.gif"))




## ====================================================








