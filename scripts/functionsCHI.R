#readin a shapefile, buffer and grid it based on count/ cell, save as geoTiff, works for lines too------
#used in oilngasMap.r

pointRaster <- function(file, template, dsn){
  file_p <- st_buffer(file, 1000)
  r <- fasterize(file_p, template,  fun="sum", background = 0)
  r[r ==0] <- NA
  writeRaster(r, filename = paste(dsn, deparse(substitute(file)), sep = ""), overwrite = TRUE,format='GTiff')
}

polyRaster <- function(file, template, dsn){
  file_p <- st_buffer( file, 100)
  r <- fasterize(file_p, template,  fun="count", background = 0)
  r[r ==0] <- NA
  writeRaster(r, filename = paste(dsn, deparse(substitute(file)), sep = ""), overwrite = TRUE,format='GTiff')
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
  gridList <- lapply(gridList,raster)
  gridList = brick(gridList)
  gridsum <- sum(gridList, na.rm = T)
  gridsum=  resample(gridsum,template, method = 'ngb')
  gridsum[gridsum ==0] <- NA
  writeRaster(gridsum, filename = paste0(writepath, file, sep = ""), overwrite = TRUE,format='GTiff')
  }

#combine all rasters in a folder and write new grid based on footprint-------
#used in oilngas.r
footprntRaster <- function(dsn, writepath){
  gridList <- dir(path=dsn,pattern=".tif", all.files = TRUE, full.names  = T)
  gridList <- lapply(gridList,raster)
  gridList = brick(gridList)
  gridsum <- sum(gridList, na.rm = T)
  gridsum=  resample(gridsum,template, method = 'ngb')
  gridsum[gridsum ==0] <- NA
  gridsum[gridsum >=1] <- 1 
  
  writeRaster(gridsum, filename = writepath, overwrite = TRUE,format='GTiff')
}

# Halpern et al grid data--------
halpernRaster <- function(grid1, path, NBW, writepath){
  mol = CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  NBW2 = NBW%>%st_transform(mol)
  file  = tools::file_path_sans_ext( basename(path))
  #make data smaller
  grid1 <- crop(grid1,NBW2)
  grid1<- mask(grid1,NBW2)
  grid1 <- projectRaster(grid1, crs=UTM20, res = 1000, method = 'ngb')
  
  #need to crop so same extent
  grid1 =  resample(grid1,template, method = 'ngb')
  writeRaster(grid1, filename = paste(writepath,names(grid1), sep = ""), bylayer= T, overwrite = TRUE,format='GTiff')
}

