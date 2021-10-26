# ===============================================================
basic_distance <- function(r=NULL, lat1, lat2, lon1, lon2, input="degrees"){
  # assign Earth radius if not specified
  if(is.null(r)) r <- 6.371 * 10^6
  if(input == "degrees"){
    lat1 <- lat1 * (pi/180)
    lat2 <- lat2 * (pi/180)
    lon1 <- lon1 * (pi/180)
    lon2 <- lon2 * (pi/180)
  }
  
  d <- r * sqrt( (lat2 - lat1)^2 + (lon2 - lon1)^2 )
  return (d)
}
# ===============================================================
haversine_distance <- function(r=NULL, lat1, lat2, lon1, lon2, input="degrees"){
  # assign Earth radius if not specified
  if(is.null(r)) r <- 6.371 * 10^6
  if(input == "degrees"){
    lat1 <- lat1 * (pi/180)
    lat2 <- lat2 * (pi/180)
    lon1 <- lon1 * (pi/180)
    lon2 <- lon2 * (pi/180)
  }
  
  d <- 2 * r * asin(sqrt( sin((lat2 - lat1)/2)^2 + cos(lat1) * cos(lat2) * sin((lon2 - lon1)/2)^2 ))
  return (d)
}
# ===============================================================
gaussian2D_weight <- function(point, mu_point, sigma){
  dr <- sqrt((point[1] - mu_point[1])^2 + (point[2] - mu_point[2])^2)
  exp(-((dr)/(sigma))^2)
}
# ===============================================================
view_fits <- function(my_fits, title=NULL){
  if(is.null(title)) title <- my_fits
  X <-  readFITS(my_fits)
  X
  ax1 <- axVec(1, X$axDat)          # Make axis vector for image
  ax2 <- axVec(2, X$axDat)
  xlab <- paste0(X$axDat$ctype[1], " [", X$axDat$cunit[1], "]")
  ylab <- paste0(X$axDat$ctype[2], " [", X$axDat$cunit[2], "]")
  ylab <- paste0(X$axDat$ctype[2], " [", X$axDat$cunit[2], "]")
  image(ax1, ax2, X$imDat, xlab = xlab, ylab = ylab, main = title)
}
# ===============================================================
fits2png <- function(my_fits, title=NULL, my_w=500, my_h=500, my_bg="white", my_png=NULL){
  if(is.null(title)) title <- my_fits
  X <-  readFITS(my_fits)
  X
  ax1 <- axVec(1, X$axDat)          # Make axis vector for image
  ax2 <- axVec(2, X$axDat)
  xlab <- paste0(X$axDat$ctype[1], " [", X$axDat$cunit[1], "]")
  ylab <- paste0(X$axDat$ctype[2], " [", X$axDat$cunit[2], "]")
  ylab <- paste0(X$axDat$ctype[2], " [", X$axDat$cunit[2], "]")
  if(is.null(my_png)) my_png <- gsub(".fits", ".png", my_fits)
  png(filename = my_png,
      width = my_w, height = my_h, units = "px", pointsize = 12,
      bg = my_bg)
  image(ax1, ax2, X$imDat, xlab = xlab, ylab = ylab, main = title)
  dev.off()
}  
# ===============================================================
get_classes <- function(my_df){
  aa <- t(data.frame(lapply(my_df, class)))
  aa <- data.frame('feature'=rownames(aa), 'class'=as.character(aa))
  return(aa)
}
# ===============================================================
create_raster <- function(geo_data, pixel_dx_metres=100, filename="dummy.fits"){
  range_lat <- c(min(geo_data$latitude),  max(geo_data$latitude))
  range_lon <- c(min(geo_data$longitude), max(geo_data$longitude))
  
  radius_earth <- 6.371 * 10^6
  
  range_lat_metres <- radius_earth * (range_lat[2]-range_lat[1])*pi/180
  range_lon_metres <- radius_earth * (range_lon[2]-range_lon[1])*pi/180
  
  # choose a pixel resolution:
  
  pixel_dx_degs   <- (pixel_dx_metres*180/pi)/radius_earth
  npix_x <- ceiling(range_lon_metres/pixel_dx_metres)
  npix_y <- ceiling(range_lat_metres/pixel_dx_metres)
  
  #Elevation_at_Address
  
  # Calculate grid for this pixel resolution:
  centre_lat <- mean(range_lat)
  centre_lon <- mean(range_lon)
  lat1 <- centre_lat - 0.5 * npix_y * pixel_dx_degs
  lat2 <- centre_lat + 0.5 * npix_y * pixel_dx_degs
  lon1 <- centre_lon - 0.5 * npix_x * pixel_dx_degs
  lon2 <- centre_lon + 0.5 * npix_x * pixel_dx_degs
  
  xbins <- 1:npix_x
  ybins <- 1:npix_y
  crpix1 <- npix_x/2
  crpix2 <- npix_y/2
  crval1 <- centre_lon
  crval2 <- centre_lat
  cdelt1 <- pixel_dx_degs
  cdelt2 <- pixel_dx_degs
  xvals <- xbins - 0.5
  yvals <- ybins - 0.5
  pvals <- crval1 + (xvals - crpix1) * cdelt1
  qvals <- crval2 + (yvals - crpix2) * cdelt2
  
  geo_data$lat_bin <- ceiling((geo_data$latitude  - crval2)/cdelt2 + crpix2)
  geo_data$lon_bin <- ceiling((geo_data$longitude - crval1)/cdelt1 + crpix1)
  
  geo_data$Elevation_at_Address <- as.numeric(geo_data$Elevation_at_Address)
  
  geo_data_aggregate <- geo_data %>% group_by(lat_bin, lon_bin) %>% summarise('mean_elevation'=mean(Elevation_at_Address))
  geo_data_aggregate$bin_id <- geo_data_aggregate$lon_bin + (geo_data_aggregate$lat_bin - 1)*npix_x
  geo_var <- 1:(npix_x*npix_y)*NA
  geo_var[geo_data_aggregate$bin_id] <- geo_data_aggregate$mean_elevation
  geo_matrix <- matrix(geo_var, nrow=npix_x, ncol=npix_y)
  
  writeFITSim(geo_matrix, file = filename, c1 = paste0("Variable: Elevation at resolution: (", pixel_dx_metres, ")"),
              crpix = c(crpix1,crpix2), crvaln = c(crval1, crval2), cdeltn = c(cdelt1, cdelt2),
              ctypen = c("Longitude", "Latitude"),
              cunitn = c("degrees", "degrees"))
  
}
# ===============================================================
power_smoothing <- function(geo_data, pixel_size_metres=100, dist_cutoff=2000, power_int=2, power_ext=3, base_filename="power_smoothing"){
  
  # TO-DO in future:
  # PRECEDENCE:
  # If pixel_size_metres is specified and npix_x and npix_y are NULL, they will be determined automatically.
  # If npix_x is specified, the other two will be determined automatically, overwriting any existing inputs
  # If npix_y is specified, it will only work if npix_x is NULL (otherwise npix_x takes precedence)
  
  if(is.null(pixel_size_metres)) stop("You must specify pixel_size_metres")
  
  range_lat <- c(min(geo_data$latitude),  max(geo_data$latitude))
  range_lon <- c(min(geo_data$longitude), max(geo_data$longitude))
  
  radius_earth <- 6.371 * 10^6
  
  range_lat_metres <- radius_earth * (range_lat[2]-range_lat[1])*pi/180
  range_lon_metres <- radius_earth * (range_lon[2]-range_lon[1])*pi/180
  
  # choose a pixel resolution:
  
  pixel_size_degs   <- (pixel_size_metres*180/pi)/radius_earth
  npix_x <- ceiling(range_lon_metres/pixel_size_metres)
  npix_y <- ceiling(range_lat_metres/pixel_size_metres)
  
  #Elevation_at_Address
  
  # Calculate grid for this pixel resolution:
  centre_lat <- mean(range_lat)
  centre_lon <- mean(range_lon)
  lat1 <- centre_lat - 0.5 * npix_y * pixel_size_degs
  lat2 <- centre_lat + 0.5 * npix_y * pixel_size_degs
  lon1 <- centre_lon - 0.5 * npix_x * pixel_size_degs
  lon2 <- centre_lon + 0.5 * npix_x * pixel_size_degs
  
  xbins <- 1:npix_x
  ybins <- 1:npix_y
  crpix1 <- npix_x/2
  crpix2 <- npix_y/2
  crval1 <- centre_lon
  crval2 <- centre_lat
  cdelt1 <- pixel_size_degs
  cdelt2 <- pixel_size_degs
  xvals <- xbins - 0.5
  yvals <- ybins - 0.5
  pvals <- crval1 + (xvals - crpix1) * cdelt1
  qvals <- crval2 + (yvals - crpix2) * cdelt2
  
  geo_data$Elevation_at_Address <- as.numeric(geo_data$Elevation_at_Address)
  
  geo_var <- 1:(npix_x*npix_y)*NA
  
  pn <- 1
  for(qv in qvals){
    for(pv in pvals){
      rv <- radius_earth * sqrt((geo_data$longitude - pv)^2 + (geo_data$latitude - qv)^2) * pi/180
      ri <- which(rv < dist_cutoff)
      if(length(ri) > 0){
        ww <- (1 - (rv[ri]/dist_cutoff)^power_int)^power_ext
        
        # Normalise the weights to 1:
        sum_ww <- sum(ww)
        if(sum_ww == 0){
          ww <- ww*0
        } else {
          ww <- ww/sum_ww
        }
        geo_var[pn] <- sum(ww * geo_data$Elevation_at_Address[ri])
      } else {
        geo_var[pn] <- NA
      }
      pn <- pn + 1
    }
  }
  
  geo_matrix <- matrix(geo_var, nrow=npix_x, ncol=npix_y)
  
  filename <- paste0(base_filename, "_pix", pixel_size_metres, "_dist", dist_cutoff, ".fits")
  writeFITSim(geo_matrix, file = filename, c1 = paste0("Variable: Elevation at resolution: (", pixel_size_metres, ")"),
              crpix = c(crpix1,crpix2), crvaln = c(crval1, crval2), cdeltn = c(cdelt1, cdelt2),
              ctypen = c("Longitude", "Latitude"),
              cunitn = c("degrees", "degrees"))
}
# ===============================================================
# ===============================================================
gaussian_smoothing <- function(geo_data, pixel_size_metres=100, sigma=1000, nsigma_cutoff=4, base_filename="gaussian_smoothing"){
  
  # TO-DO in future:
  # PRECEDENCE:
  # If pixel_size_metres is specified and npix_x and npix_y are NULL, they will be determined automatically.
  # If npix_x is specified, the other two will be determined automatically, overwriting any existing inputs
  # If npix_y is specified, it will only work if npix_x is NULL (otherwise npix_x takes precedence)
  
  if(is.null(pixel_size_metres)) stop("You must specify pixel_size_metres")
  
  range_lat <- c(min(geo_data$latitude),  max(geo_data$latitude))
  range_lon <- c(min(geo_data$longitude), max(geo_data$longitude))
  
  radius_earth <- 6.371 * 10^6
  
  range_lat_metres <- radius_earth * (range_lat[2]-range_lat[1])*pi/180
  range_lon_metres <- radius_earth * (range_lon[2]-range_lon[1])*pi/180
  
  # choose a pixel resolution:
  
  pixel_size_degs   <- (pixel_size_metres*180/pi)/radius_earth
  npix_x <- ceiling(range_lon_metres/pixel_size_metres)
  npix_y <- ceiling(range_lat_metres/pixel_size_metres)
  
  #Elevation_at_Address
  
  # Calculate grid for this pixel resolution:
  centre_lat <- mean(range_lat)
  centre_lon <- mean(range_lon)
  lat1 <- centre_lat - 0.5 * npix_y * pixel_size_degs
  lat2 <- centre_lat + 0.5 * npix_y * pixel_size_degs
  lon1 <- centre_lon - 0.5 * npix_x * pixel_size_degs
  lon2 <- centre_lon + 0.5 * npix_x * pixel_size_degs
  
  xbins <- 1:npix_x
  ybins <- 1:npix_y
  crpix1 <- npix_x/2
  crpix2 <- npix_y/2
  crval1 <- centre_lon
  crval2 <- centre_lat
  cdelt1 <- pixel_size_degs
  cdelt2 <- pixel_size_degs
  xvals <- xbins - 0.5
  yvals <- ybins - 0.5
  pvals <- crval1 + (xvals - crpix1) * cdelt1
  qvals <- crval2 + (yvals - crpix2) * cdelt2
  
  geo_data$Elevation_at_Address <- as.numeric(geo_data$Elevation_at_Address)
  
  geo_var <- 1:(npix_x*npix_y)*NA
  
  pn <- 1
  for(qv in qvals){
    for(pv in pvals){
      rv <- radius_earth * sqrt((geo_data$longitude - pv)^2 + (geo_data$latitude - qv)^2) * pi/180
      dist_cutoff <- sigma * nsigma_cutoff
      ri <- which(rv < dist_cutoff)
      if(length(ri) > 0){
        ww <- exp(-((rv[ri])/(sigma))^2)
        
        # Normalise the weights to 1:
        sum_ww <- sum(ww)
        if(sum_ww == 0){
          ww <- ww*0
        } else {
          ww <- ww/sum_ww
        }
        geo_var[pn] <- sum(ww * geo_data$Elevation_at_Address[ri])
      } else {
        geo_var[pn] <- NA
      }
      pn <- pn + 1
    }
  }
  
  geo_matrix <- matrix(geo_var, nrow=npix_x, ncol=npix_y)
  
  filename <- paste0(base_filename, "_pix", pixel_size_metres, "_sigma", sigma, "_nsigma", nsigma_cutoff, ".fits")
  writeFITSim(geo_matrix, file = filename, c1 = paste0("Variable: Elevation at resolution: (", pixel_size_metres, ")"),
              crpix = c(crpix1,crpix2), crvaln = c(crval1, crval2), cdeltn = c(cdelt1, cdelt2),
              ctypen = c("Longitude", "Latitude"),
              cunitn = c("degrees", "degrees"))
}
# ===============================================================
# ===============================================================
refine_cell <- function(master_object, cell_id, points_threshold=100, refinement_threshold=12, my_vars){
  # First check if all the vars listed actually exist in the dataset:
  
  cat("Refining cell: ", cell_id, "\n")
  
  master_data <- master_object$geo_data
  adjusted_cell_id <- gsub("0", "", cell_id)
  
  
  nblocks <- length(geo_object$refinement_data$x1)
  nvars   <- length(my_vars)
  
  if(sum((my_vars %in% names(master_data))*1) == nvars){
    ii <- which(master_data$cell_id == cell_id)
    my_data <- master_data[ii,]
    npoints <- dim(my_data)[1]
    
    if(npoints <= points_threshold){
      my_data$cell_done <- 1
      kk <- which(master_object$refinement_data$id == cell_id)
      master_object$refinement_data$cell_done[kk] <- 1
    } else {
      my_data$cell_quadrant <- 1
      cell_x1 <- my_data$cell_x1[1]
      cell_x2 <- my_data$cell_x2[1]
      cell_y1 <- my_data$cell_y1[1]
      cell_y2 <- my_data$cell_y2[1]
      
      cell_x0 <- (cell_x1 + cell_x2)/2
      cell_y0 <- (cell_y1 + cell_y2)/2
      
      cell_quadrant1 <- (1:npoints)*0 + 1
      cell_quadrant2 <- (1:npoints)*0 + 1
      j1 <- which(my_data$longitude > cell_x0)
      j2 <- which(my_data$latitude  > cell_y0)
      cell_quadrant1[j1] <- 2
      cell_quadrant2[j2] <- 2
      my_data$cell_quadrant <- (cell_quadrant2-1)*2 + cell_quadrant1
      
      # Newly refined Cell ID
      my_data$cell_id  <- str_pad(paste0(my_data$cell_quadrant, adjusted_cell_id), width=20, side="left", pad="0")
      
      # SUMMARISE ALL COLUMNS AT ONCE AND ENSURE THERE ARE ALWAYS 4 QUADRANTS
      # DO THIS BY IMPUTING EMPY QUADRANTS TO THE PARENT VALUE (OR ZERO FOR COUNTS)
      my_vars_count <- data.frame('cell_quadrant'=1:4) %>% left_join(
        my_data %>% group_by(cell_quadrant) %>% summarise(n=n())
      )
      qq <- which(is.na(my_vars_count$n))
      if(length(qq) > 0) my_vars_count$n[qq] <- 0
      
      my_vars_means <- data.frame('cell_quadrant'=1:4) %>% left_join(
        my_data %>% group_by(cell_quadrant) %>% summarise_at(my_vars, mean, na.rm = TRUE)
      )
      my_parent_means <- my_data %>% summarise_at(my_vars, mean, na.rm = TRUE)
      
      for(my_v in my_vars){
        qq <- which(is.na(my_vars_means[[my_v]]))
        if(length(qq) > 0) my_vars_means[[my_v]][qq] <- my_parent_means[[my_v]][1]
      }
      
      my_vars_sum   <- data.frame('cell_quadrant'=1:4) %>% left_join(
        my_data %>% group_by(cell_quadrant) %>% summarise_at(my_vars, sum, na.rm = TRUE)
      )
      for(my_v in my_vars){
        qq <- which(is.na(my_vars_sum[[my_v]]))
        if(length(qq) > 0) my_vars_sum[[my_v]][qq] <- 0
      }
      
      cell_refinement_level <- my_data$cell_refinement_level[1] + 1
      my_data$cell_refinement_level <- cell_refinement_level
      if(cell_refinement_level >= refinement_threshold) my_data$cell_done <- 1
      
      # Do the quadrants:
      my_q <- 1
      jj <- which(my_data$cell_quadrant == my_q)
      if(length(jj) > 0){
        my_data$cell_x2[jj] <- cell_x0
        my_data$cell_y2[jj] <- cell_y0
        if(my_vars_count$n[my_q] <= points_threshold) my_data$cell_done[jj] <- 1
      }
      
      my_q <- 2
      jj <- which(my_data$cell_quadrant == my_q)
      if(length(jj) > 0){
        my_data$cell_x1[jj] <- cell_x0
        my_data$cell_y2[jj] <- cell_y0
        if(my_vars_count$n[my_q] <= points_threshold) my_data$cell_done[jj] <- 1
      }
      
      my_q <- 3
      jj <- which(my_data$cell_quadrant == my_q)
      if(length(jj) > 0){
        my_data$cell_x2[jj] <- cell_x0
        my_data$cell_y1[jj] <- cell_y0
        if(my_vars_count$n[my_q] <= points_threshold) my_data$cell_done[jj] <- 1
      }
      
      my_q <- 4
      jj <- which(my_data$cell_quadrant == my_q)
      if(length(jj) > 0){
        my_data$cell_x1[jj] <- cell_x0
        my_data$cell_y1[jj] <- cell_y0
        if(my_vars_count$n[my_q] <= points_threshold) my_data$cell_done[jj] <- 1
      }
      
      master_object$refinement_data$x1[nblocks+1] <- cell_x1
      master_object$refinement_data$x2[nblocks+1] <- cell_x0
      master_object$refinement_data$y1[nblocks+1] <- cell_y1
      master_object$refinement_data$y2[nblocks+1] <- cell_y0
      
      master_object$refinement_data$x1[nblocks+2] <- cell_x0
      master_object$refinement_data$x2[nblocks+2] <- cell_x2
      master_object$refinement_data$y1[nblocks+2] <- cell_y1
      master_object$refinement_data$y2[nblocks+2] <- cell_y0
      
      master_object$refinement_data$x1[nblocks+3] <- cell_x1
      master_object$refinement_data$x2[nblocks+3] <- cell_x0
      master_object$refinement_data$y1[nblocks+3] <- cell_y0
      master_object$refinement_data$y2[nblocks+3] <- cell_y2
      
      master_object$refinement_data$x1[nblocks+4] <- cell_x0
      master_object$refinement_data$x2[nblocks+4] <- cell_x2
      master_object$refinement_data$y1[nblocks+4] <- cell_y0
      master_object$refinement_data$y2[nblocks+4] <- cell_y2
      
      for(nq in 1:4){
        master_object$refinement_data$id[nblocks+nq] <- str_pad(paste0(nq, adjusted_cell_id), width=20, side="left", pad="0")
        master_object$refinement_data$cell_refinement_level[nblocks+nq] <- cell_refinement_level
        master_object$refinement_data$count[nblocks+nq] <- my_vars_count$n[nq]
        master_object$refinement_data$cell_done[nblocks+nq] <- 0
        if(my_vars_count$n[nq] <= points_threshold)       master_object$refinement_data$cell_done[nblocks+nq] <- 1
        if(cell_refinement_level >= refinement_threshold) master_object$refinement_data$cell_done[nblocks+nq] <- 1
        for(nn in 1:nvars){
          master_object$refinement_data[[paste0("mean_", my_vars[nn])]][nblocks+nq] <- my_vars_means[[my_vars[nn]]][nq]
          master_object$refinement_data[[paste0("sum_",  my_vars[nn])]][nblocks+nq] <- my_vars_sum[[my_vars[nn]]][nq]
        }
      }
      
      kk <- which(master_object$refinement_data$id == cell_id)
      master_object$refinement_data$cell_done[kk] <- 1
      
    }
    master_object$geo_data[ii,] <- my_data
    
  }
  return (master_object)
}
# ===============================================================
master_refine_data <- function(geo_data, points_threshold=100, max_ref_level=12, file_basename="dummy", 
                               min_lat=NULL, max_lat=NULL, min_lon=NULL, max_lon=NULL){
  
  if(is.null(min_lat)) min_lat <- min(geo_data$latitude)
  if(is.null(max_lat)) max_lat <- max(geo_data$latitude)
  if(is.null(min_lon)) min_lon <- min(geo_data$longitude)
  if(is.null(max_lon)) max_lon <- max(geo_data$longitude)
  
  range_lat <- c(min_lat, max_lat)
  range_lon <- c(min_lon, max_lon)
  centre_lat <- mean(range_lat)
  centre_lon <- mean(range_lon)
  
  #Elevation_at_Address
  geo_data$Elevation_at_Address <- as.numeric(geo_data$Elevation_at_Address)
  
  initial_id <- str_pad("", width=20, side="left", pad="0")
  geo_data$cell_id   <- initial_id
  geo_data$cell_quadrant <- 0
  geo_data$cell_x1 <- min_lon
  geo_data$cell_x2 <- max_lon
  geo_data$cell_y1 <- min_lat
  geo_data$cell_y2 <- max_lat
  geo_data$cell_done <- 0
  geo_data$cell_refinement_level <- 0
  
  # ==================================================
  # THIS would be the part where we loop through all of the variables to be smoothed.
  # =============
  geo_object <- list()
  geo_object$geo_data <- geo_data
  geo_object <- refine_cell(geo_object, initial_id, points_threshold=points_threshold, refinement_threshold=max_ref_level, my_vars=c("Elevation_at_Address"))
  
  # NOW loop though all the unfinished cells:
  uc <- which(geo_object$refinement_data$cell_done == 0)
  pending_cells <- geo_object$refinement_data$id[uc]
  npc <- length(pending_cells)
  while(npc > 0){
    next_cell_id <- pending_cells[1]
    geo_object <- refine_cell(geo_object, next_cell_id, points_threshold=points_threshold, refinement_threshold=max_ref_level, my_vars=c("Elevation_at_Address"))
    uc <- which(geo_object$refinement_data$cell_done == 0)
    pending_cells <- geo_object$refinement_data$id[uc]
    npc <- length(pending_cells)
  }
  save(geo_object, file = paste0(file_basename, ".Rdata"))
  # ABOVE NEEDS TO BE USING NUMBER OF NON-NULL POINTS, NOT NUMBER OF POINTS, TO DETERMINE REFINEMENT. !!!!!
  
  # ===> QNS:
  # WHY do we have NAs in the var columns in the refined dataset?  They should be imputed to the parent value.
  
  # =========================
  # Creating the FITS file:
  my_df <- data.frame(geo_object$refinement_data)
  max_refinement <- max(my_df$cell_refinement_level)
  
  npix_x <- 2^max_refinement
  npix_y <- npix_x
  npix_2d <- npix_x*npix_y
  
  radius_earth <- 6.371 * 10^6
  
  xbins  <- 1:npix_x
  ybins  <- 1:npix_y
  crpix1 <- npix_x/2
  crpix2 <- npix_y/2
  crval1 <- centre_lon
  crval2 <- centre_lat
  cdelt1 <- (max_lon - min_lon)/npix_x
  cdelt2 <- (max_lat - min_lat)/npix_y
  xvals  <- xbins - 0.5
  yvals  <- ybins - 0.5
  pvals  <- crval1 + (xvals - crpix1) * cdelt1
  qvals  <- crval2 + (yvals - crpix2) * cdelt2
  
  my_df$px1 <- 1 + floor(crpix1 + (my_df$x1 - crval1)/cdelt1)
  my_df$px2 <- round(crpix1 + (my_df$x2 - crval1)/cdelt1)
  my_df$py1 <- 1 + floor(crpix2 + (my_df$y1 - crval2)/cdelt2)
  my_df$py2 <- round(crpix2 + (my_df$y2 - crval2)/cdelt2)
  
  # Boundary fix:
  bi <- which(my_df$px1 < 1)
  if(length(bi) > 0) my_df$px1[bi] <- 1
  bi <- which(my_df$px2 < 1)
  if(length(bi) > 0) my_df$px2[bi] <- 1
  bi <- which(my_df$py1 < 1)
  if(length(bi) > 0) my_df$py1[bi] <- 1
  bi <- which(my_df$py2 < 1)
  if(length(bi) > 0) my_df$py2[bi] <- 1
  
  bi <- which(my_df$px1 > npix_x)
  if(length(bi) > 0) my_df$px1[bi] <- npix_x
  bi <- which(my_df$px2 > npix_x)
  if(length(bi) > 0) my_df$px2[bi] <- npix_x
  bi <- which(my_df$py1 > npix_y)
  if(length(bi) > 0) my_df$py1[bi] <- npix_y
  bi <- which(my_df$py2 > npix_y)
  if(length(bi) > 0) my_df$py2[bi] <- npix_y
  
  my_matrix <- matrix((1:npix_2d)*NA, nrow=npix_x, ncol=npix_y)
  
  ncells <- dim(my_df)[1]
  for(ci in 1:ncells){
    cat(ci,"\n")
    my_matrix[my_df$px1[ci]:my_df$px2[ci], my_df$py1[ci]:my_df$py2[ci]] <- my_df$mean_Elevation_at_Address[ci]
  }
  
  # xlab <- "Longitude (degrees)"
  # ylab <- "Latitude (degrees)"
  # image(pvals, qvals, my_matrix, xlab = xlab, ylab = ylab)
  
  fits_name <- "elevation.fits"
  writeFITSim(my_matrix, file = fits_name, c1 = paste0("Variable"),
              crpix = c(crpix1,crpix2), crvaln = c(crval1, crval2), cdeltn = c(cdelt1, cdelt2),
              ctypen = c("Longitude", "Latitude"),
              cunitn = c("degrees", "degrees"))
  

  
  
  # End of creating the FITS file
  # =========================
  
  # =============
  # ==================================================
  
} # End of the function.  
# ===============================================================  
