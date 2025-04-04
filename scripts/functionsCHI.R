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

