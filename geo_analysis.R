library(tidyverse)
require(FITSio)
library(astro)

my_dir <- "/Volumes/DataVault/Projects/GeoSmoothing"
setwd(my_dir)

geo_data <- read.csv("data/sample nsw 2.csv")
head(geo_data)

plot(geo_data$longitude, geo_data$latitude)



my_w <- 960
my_h <- 960
png(filename = "coords.png",
    width = my_w, height = my_h, units = "px", pointsize = 12,
    bg = "white")
plot(geo_data$longitude, geo_data$latitude)
dev.off()
png(filename = "raster.png",
    width = my_w, height = my_h, units = "px", pointsize = 12,
    bg = "white")
view_fits("power_smoothing.fits")
dev.off()
png(filename = "adaptive.png",
    width = my_w, height = my_h, units = "px", pointsize = 12,
    bg = "white")
xlab <- "Longitude (degrees)"
ylab <- "Latitude (degrees)"
image(pvals, qvals, log(my_matrix), xlab = xlab, ylab = ylab)
dev.off()

png(filename = "overlay.png",
    width = my_w, height = my_h, units = "px", pointsize = 12,
    bg = "transparent")
image(pvals, qvals, log(my_matrix), xlab = xlab, ylab = ylab)
view_fits("pixels_100m.fits")
dev.off()
  
  
  
power_smoothing(geo_data, pixel_size_metres=1000, dist_cutoff=2000, power_int=2, power_ext=3, base_filename="power_smoothing")
power_smoothing(geo_data, pixel_size_metres=100, dist_cutoff=2000, power_int=2, power_ext=3, base_filename="power_smoothing")

gaussian_smoothing(geo_data, pixel_size_metres=1000, sigma=500, nsigma_cutoff=4, base_filename="gaussian_smoothing")
gaussian_smoothing(geo_data, pixel_size_metres=100, sigma=500, nsigma_cutoff=4, base_filename="gaussian_smoothing")


fits2png("pixels_250m.fits")
fits2png("AMR_refinement12_npoints2.fits")
fits2png("power_smoothing_pix1000_dist2000.fits")
fits2png("power_smoothing_pix100_dist2000.fits")
fits2png("gaussian_smoothing_pix1000_dist2000.fits")
fits2png("gaussian_smoothing_pix100_dist2000.fits")


  
  my_var <- geo_data %>% select(longitude, latitude, 'var'=Elevation_at_Address)
  geo_var <- 1:(npix_x*npix_y)*NA
  

  
  geo_matrix <- matrix(geo_var, nrow=npix_x, ncol=npix_y)
  
  writeFITSim(geo_matrix, file = filename, c1 = paste0("Variable: Elevation at resolution: (", pixel_dx_metres, ")"),
              crpix = c(crpix1,crpix2), crvaln = c(crval1, crval2), cdeltn = c(cdelt1, cdelt2),
              ctypen = c("Longitude", "Latitude"),
              cunitn = c("degrees", "degrees"))
  


create_raster(geo_data, pixel_dx_metres=10000, filename="pixels_10000m.fits")
create_raster(geo_data, pixel_dx_metres=1000, filename="pixels_1000m.fits")
create_raster(geo_data, pixel_dx_metres=250, filename="pixels_250m.fits")
create_raster(geo_data, pixel_dx_metres=100, filename="pixels_100m.fits")
  
#X <-  readFITS("pixels_10000m.fits")

view_fits("pixels_10000m.fits")
view_fits("pixels_1000m.fits")
view_fits("pixels_250m.fits")
view_fits("pixels_100m.fits")


smooth_data(geo_data, pixel_dx_metres=1000, filename="smoothed_1000m.fits")

# ===========================


## Make test image with axis information, write to disk
Z <- matrix(1:15, ncol = 3)
dim(Z)
filename <- paste(tempdir(), "test.fits", sep="")
writeFITSim(Z, file = filename, c1 = "Test FITS file",
            crpix = c(1,1), crvaln = c(10, 100), cdeltn = c(8, 2),
            ctypen = c("Distance", "Time"),
            cunitn = c("Furlongs", "Fortnights"))

## Read image back and display
X <-  readFITS(filename)
ax1 <- axVec(1, X$axDat)          # Make axis vector for image
ax2 <- axVec(2, X$axDat)
xlab <- X$axDat$ctype[1]
ylab <- paste(X$axDat$ctype[2], " [", X$axDat$cunit[2], "]", sep = "")
image(ax1, ax2, X$imDat, xlab = xlab, ylab = ylab)
str(X)
X$axDat                           # Display data frame with axis data
X$hdr[1:10]                       # Header sample
X$hdr[which(X$hdr=="BITPIX")+1]   # BITPIX value from header

### Read back in, modify data, header, and axis data information,
## then write modified version as new file
Z <-  readFITS(filename)
Z$imDat <- Z$imDat + 300
Z$header <- addKwv('SCALE', 1.03, 'test header mod', header=Z$header)
# Z$axDat <- edit(Z$axDat)  # interactive edit
Z$axDat$cdelt[2] <- 20
filename2 <- paste(tempdir(), "test.fits", sep="")
writeFITSim(Z$imDat, file=filename2, axDat=Z$axDat, header=Z$header)

## Clean up files to avoid clutter
unlink(filename)
unlink(filename2)
