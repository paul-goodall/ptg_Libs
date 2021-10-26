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
lon0 <- 151.207190
lat0 <- -33.867502
radius_cutoff_km <- 5
radius_cutoff_degrees <- (radius_cutoff_km*1000/radius_earth)*(180/pi)

sa1stats <- geo_data2 %>% group_by(sa1_7dig16) %>% summarise('lat'=mean(latitude), 'lon'=mean(longitude))
sa1stats$distance_to_point_degrees <- sqrt((sa1stats$lat - lat0)^2 + (sa1stats$lon - lon0)^2)

sa1sample <- sa1stats %>% filter(distance_to_point_degrees <= radius_cutoff_degrees)

geo_data_sample <- geo_data2 %>% filter(sa1_7dig16 %in% sa1sample$sa1_7dig16)
obj_sf_sample   <- obj_sf    %>% filter(SA1_7DIG16 %in% sa1sample$sa1_7dig16)


# Two ways of doing this:
# 1) Average the sa1s and then run the smoothing across the sa1s
# 2) Run the smoothing across the data to create smoothed data. Then re-aggregate to the sa1 level.

# At the end you'll need to left join the values onto the shapefile dataset to plot the results.

## ====================================================
## GENERAL
## Choose the variables to smooth:

my_vars <- c("jtw_conn1","jtw_conn10","jtw_conn19","jtw_mbInd","jtw_mb_Res","jtw_ttl","median_mor","median_tot","median_ren","median_to0","average_nu","median_to1","average_ho")

for(nn in my_vars){
  geo_data_sample[[nn]] <- as.numeric(geo_data_sample[[nn]])
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

## ====================================================
## METHOD 1

group_vars <- c("longitude", "latitude", my_vars)
method1_means  <- geo_data_sample %>% group_by(sa1_7dig16) %>% summarise_at(group_vars, mean, na.rm = TRUE)
method1_sums   <- geo_data_sample %>% group_by(sa1_7dig16) %>% summarise_at(group_vars, sum,  na.rm = TRUE)
method1_counts <- geo_data_sample %>% group_by(sa1_7dig16) %>% summarise('count'=n())

for(nn in names(method1_means)){
  ii <- which(is.nan(method1_means[[nn]]))
  method1_means[[nn]][ii] <- NA
}

for(nn in names(method1_sums)){
  ii <- which(is.nan(method1_sums[[nn]]))
  method1_sums[[nn]][ii] <- NA
}

## Loop through each postcode:
method1_means_power <- method1_means
method1_means_gauss <- method1_means
for(nr in 1:dim(method1_means)[1]){
  my_sa1 <- method1_means[nr,]
  method1_means$dist_metres <- haversine_distance(r=radius_earth, lat1=method1_means_power$latitude, lat2=my_sa1$latitude[1], 
                                                  lon1=method1_means_power$longitude, lon2=my_sa1$longitude[1])
  
  ii <- which(method1_means$dist_metres <= dist_cutoff)
  sample_means_power <- method1_means[ii,]
  ww <- (1 - (sample_means_power$dist_metres/dist_cutoff)^power_int)^power_ext
  ww <- ww/sum(ww)
  # Multiply each column by the weight, without the first three columns:
  vals_means_power <- sample_means_power[,-(1:3)]*t(ww)
  vals_means_power <- vals_means_power %>% summarise_all(sum,  na.rm = TRUE)
  vals_means_power <- vals_means_power[,-dim(vals_means_power)[2]]
  method1_means_power[nr,-(1:3)] <- vals_means_power
  
  dist_cutoff_gauss <- sigma * nsigma_cutoff
  ii <- which(method1_means$dist_metres <= dist_cutoff)
  sample_means_gauss <- method1_means[ii,]
  ww <- ww <- exp(-((sample_means_power$dist_metres)/(sigma))^2)
  ww <- ww/sum(ww)
  # Multiply each column by the weight, without the first three columns:
  vals_means_gauss <- sample_means_gauss[,-(1:3)]*t(ww)
  vals_means_gauss <- vals_means_gauss %>% summarise_all(sum,  na.rm = TRUE)
  vals_means_gauss <- vals_means_gauss[,-dim(vals_means_gauss)[2]]
  method1_means_gauss[nr,-(1:3)] <- vals_means_gauss
  
}

## ====================================================



## ====================================================

method1_means$sa1_7dig16 <- as.character(method1_means$sa1_7dig16)
method1_means_power$sa1_7dig16 <- as.character(method1_means_power$sa1_7dig16)
method1_means_gauss$sa1_7dig16 <- as.character(method1_means_gauss$sa1_7dig16)

vis_data_real  <- left_join(x=obj_sf_sample, y=method1_means, by = c('SA1_7DIG16' = 'sa1_7dig16'))
vis_data_power <- left_join(x=obj_sf_sample, y=method1_means_power, by = c('SA1_7DIG16' = 'sa1_7dig16'))
vis_data_gauss <- left_join(x=obj_sf_sample, y=method1_means_gauss, by = c('SA1_7DIG16' = 'sa1_7dig16'))

## VISUALISATION
line_width=0.2
vis_data_real %>%
  ggplot() +
  geom_sf(aes(fill = jtw_conn1), lwd=line_width) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4)

vis_data_power %>%
  ggplot() +
  geom_sf(aes(fill = jtw_conn1), lwd=line_width) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4)

vis_data_gauss %>%
  ggplot() +
  geom_sf(aes(fill = jtw_conn1), lwd=line_width) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4)

leaflet(vis_data) %>%
  addTiles() %>%
  addPolylines(stroke = TRUE, weight = 1,color = "blue")


## ====================================================
## VISUALISATION

# png(filename = "overlay.png",
#     width = my_w, height = my_h, units = "px", pointsize = 12,
#     bg = "transparent")
# image(pvals, qvals, log(my_matrix), xlab = xlab, ylab = ylab)
# view_fits("pixels_100m.fits")
# dev.off()

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
  my_plot_info <- paste0(my_n, "_method1_real")
  my_plot_name <- paste0(my_dir, "/plots/", my_plot_info, ".png")
  my_plot_title <- paste0("Raw data: ", my_plot_info)
  plot_map(vis_data_real, my_n, my_plot_name, my_plot_title)
}

##  Power smoothed plots
for(my_n in my_vars){
  my_plot_info <- paste0(my_n, "_method1_d_", dist_cutoff, "_p1_", power_int, "_p2_", power_ext)
  my_plot_name <- paste0(my_dir, "/plots/", my_plot_info, ".png")
  my_plot_title <- paste0("Power-smoothed data: ", my_plot_info)
  plot_map(vis_data_power, my_n, my_plot_name, my_plot_title)
}

##  Gaussian smoothed plots
for(my_n in my_vars){
  my_plot_info <- paste0(my_n, "_method1_sigma_", sigma, "_ns_", nsigma_cutoff)
  my_plot_name <- paste0(my_dir, "/plots/", my_plot_info, ".png")
  my_plot_title <- paste0("Gaussian-smoothed data: ", my_plot_info)
  plot_map(vis_data_gauss, my_n, my_plot_name, my_plot_title)
}


# ===========================
plot_map <- function(my_df, my_varname, my_plotfile, my_title){
  my_df$value <- my_df[[my_varname]]
  gg <- my_df %>%
    ggplot() +
    geom_sf(aes(fill = value), lwd=line_width) +
    scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
    theme(legend.position = c(1.1, 0.5), plot.margin=unit(c(0.5,2.5,0.5,0.5),"cm")) +
    labs(x = "Longitude", y = "Latitude", title = my_title)
  
  ggsave(plot = gg, filename = my_plotfile, device = "png")
  gg
}
# ===========================

for(my_n in my_vars){
  ## list file names and read in
  imgs <- list.files(paste0(my_dir, "/plots/"), pattern=paste0(my_n,"*"), full.names = TRUE)
  imgs <- imgs[c(2,1,3)]
  img_list <- lapply(imgs, image_read)
  
  ## join the images together
  img_joined <- image_join(img_list)
  
  ## animate at 2 frames per second
  img_animated <- image_animate(img_joined, fps = 1)
  
  ## save to disk
  image_write(image = img_animated, path = paste0(my_dir, "/plots/", my_n, ".gif"))
}




## ====================================================








