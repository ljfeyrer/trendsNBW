#readin a shapefile, buffer and grid it based on count/ cell, save as geoTiff, works for lines too------
#used in oilngasMap.r

pointRaster <- function(file, template, dsn){
  file_p <- st_buffer(file, 1000)
  r <- rasterize(file_p, template,  fun="sum", background = 0)
  # r[r ==0] <- NA
  writeRaster(r, filename = paste(dsn, "//", deparse(substitute(file)), ".tif", sep = ""), overwrite = TRUE,filetype ='GTiff')
}

polyRaster <- function(file, template, dsn){
  file_p <- st_buffer( file, 100)
  r <- rasterize(file_p, template,  fun="count", background = 0)
  # r[r ==0] <- NA
  writeRaster(r, filename = paste(dsn, "//", deparse(substitute(file)), ".tif", sep = ""), overwrite = TRUE,filetype ='GTiff')
}

#create a raster stack from all rasters in a folder----
#used in fishMap.r

stackRaster <- function(dsn){
  gridList <- dir(path=dsn,pattern=".tif", all.files = TRUE, full.names  = T)
  gridnames <- list.files(path=dsn, pattern=".tif", )
  rastnames =    sub(".tif","",gridnames)
  gridList <- lapply(gridList,raster)
  gridList = stack(gridList)
}

#sum all rasters in a folder and write new grid-------
#used in fishMap.r
SumRaster <- function(dsn, writepath, file){
  gridList <- dir(path=dsn,pattern=".tif", all.files = TRUE, full.names  = T)
  gridList <- rast(gridList)
  gridsum <- sum(gridList, na.rm = T)
  gridsum[gridsum ==0] <- NA
  writeRaster(gridsum, filename = paste(writepath,"//", file, sep = ""), overwrite = TRUE, filetype  ='GTiff')
  }

#combine all rasters in a folder and write new grid based on footprint-------
#used in oilngas.r
footprntRaster <- function(dsn, writepath){
  gridList <- dir(path=dsn,pattern=".tif", all.files = TRUE, full.names  = T)
  gridList <- rast(gridList)
  gridsum <- sum(gridList, na.rm = T)
  gridsum[gridsum ==0] <- NA
  gridsum[gridsum >=1] <- 100 

  writeRaster(gridsum, filename = writepath, overwrite = TRUE, filetype  ='GTiff')
}

# Halpern et al grid data--------
halpernRaster <- function(grid1, path, NBW, writepath){
  # grid1 =rast(list.files(dsn, full.names = T))
  mol = CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  NBW2 = NBW%>%st_transform(mol)
  # file  = tools::file_path_sans_ext( basename(writepath))
  #make data smaller
  grid1 <- crop(grid1,NBW2)
  # grid1<- mask(grid1,NBW2)
  grid1 <- terra::project(grid1, UTM20, res = 1000, method = 'near')
  
  writeRaster(grid1, filename = paste(writepath,"//", names(grid1), ".tif", sep = ""), overwrite = TRUE, filetype  ='GTiff')
}


# Quantile effort into deciles
      quantEffort <- function(effort, writepath){
      # Calculate quantile breaks (excluding NA values)
      # get all data values
        effort_values <- values(effort)
        # set 0 to NA
        # effort_values[effort_values == 0] <-NA
      probs=seq(0, 1, 0.1)
      quantiles <- quantile(effort_values, probs = probs, na.rm = TRUE)
      
      # Cut the values into quantile classes
      effort_binned_values <- .bincode(effort_values, breaks = quantiles, include.lowest = TRUE)
      
      # summary(Pel_quant_rast)
      
      # Create a new raster with these binned values
      Eff_quant_rast <- effort
      values(Eff_quant_rast) <- (effort_binned_values)
      
      # plot(Pel_quant_rast)
      writeRaster(Eff_quant_rast*10, filename = paste(writepath,"//", names(effort), "_Quant.tif", sep = ""), overwrite = TRUE, filetype  ='GTiff')
      
}

      quantEffort_inv <- function(effort, writepath){
        # Calculate quantile breaks (excluding NA values)
        # effort = bot_long_fish
        # get all data values
        effort_values <- values(effort)
        x = unique(effort_values)
        probs=seq(0, 1, 0.1)
        quantiles = quantile(x, probs = probs, na.rm = TRUE)
        # Cut the values into quantile classes
        effort_binned_values <- .bincode(effort_values, breaks = quantiles)
        
        # summary(Pel_quant_rast)
        
        # Create a new raster with these binned values
        Eff_quant_rast <- effort
        values(Eff_quant_rast) <- (effort_binned_values-11)*-1
        
        # plot(Pel_quant_rast)
        writeRaster(Eff_quant_rast*10, filename = paste(writepath,"//", names(effort), "_Quant.tif", sep = ""), overwrite = TRUE, filetype  ='GTiff')
        
      }
      