### 3. Standardize stressor intensity data so ranges between 0-1
# -  Applies to layers with effort information: Oil & Gas Explorations, Fishing
# effort, Shipping, SST anomalies - Read in Pre/Post layers, Log10[x+1]
# transform and then normalize effort grids by max value across both periods


### Oil and Gas Explorations------
#Read in raster layers
pre_exOnG = raster(here::here("Results/GRIDS/Effort/PRE/pre_exOnG.tif"))
post_exOnG = raster(here::here("Results/GRIDS/Effort/POST/post_exOnG.tif"))

#stack
exOnG = stack(pre_exOnG, post_exOnG)

#log 10 values
loggridList = log10(exOnG+1)
# calc max for both periods
maxLog = max(maxValue(loggridList))

#rescale between 0-1 using max values across both periods
z <- (loggridList - minValue(loggridList)) / (maxLog- minValue(loggridList))

#write scaled files to new folder
dsn = here::here("Results/GRIDS/scaled/")
writeRaster(stack(z), filename = file.path(dsn, names(z)), bylayer=TRUE,format='GTiff', overwrite = T)

### Fishing Effort-----

#Bottom Longline effort-----
#read in raster layers
preBot_long_fish = raster(here::here("Results/GRIDS/Effort/PRE/Fish/prebot_long_fish.tif"))
postBot_long_fish = raster(here::here("Results/GRIDS/Effort/POST/Fish/postbot_long_fish.tif"))

#stack
Bot_long_fish = stack(preBot_long_fish, postBot_long_fish)

#log 10 values
loggridList = log10(Bot_long_fish+1)
# calc max for both periods
maxLog = max(maxValue(loggridList))

#rescale between 0-1 using max values across both periods
z <- (loggridList - minValue(loggridList)) / (maxLog- minValue(loggridList))

#write scaled files to new folder
dsn = here::here("Results/GRIDS/scaled/")
writeRaster(stack(z), filename = file.path(dsn, names(z)), bylayer=TRUE,format='GTiff', overwrite = T)

#Pelagic Longline effort-----
#read in raster layers
prePel_long_fish = raster(here::here("Results/GRIDS/Effort/PRE/Fish/prePel_long_fish.tif"))
postPel_long_fish = raster(here::here("Results/GRIDS/Effort/POST/Fish/postPel_long_fish.tif"))

#stack
Pel_long_fish = stack(prePel_long_fish, postPel_long_fish)

#log 10 values
loggridList = log10(Pel_long_fish+1)
# calc max for both periods
maxLog = max(maxValue(loggridList))

#rescale between 0-1 using max values across both periods
z <- (loggridList - minValue(loggridList)) / (maxLog- minValue(loggridList))

#write scaled files to new folder
dsn = here::here("Results/GRIDS/scaled/")
writeRaster(stack(z), filename = file.path(dsn, names(z)), bylayer=TRUE,format='GTiff', overwrite = T)

##Other fisheries effort from Halpern et al-----
pre_fishHalpern = raster(here::here("Results/GRIDS/Effort/PRE/Fish/pre_fishHalpern.tif"))
post_fishHalpern = raster(here::here("Results/GRIDS/Effort/POST/Fish/post_fishHalpern.tif"))

#stack
fishHalpern = stack(pre_fishHalpern, post_fishHalpern)

#log 10 values
loggridList = log10(fishHalpern+1)
# calc max for both periods
maxLog = max(maxValue(loggridList))

#rescale between 0-1 using max values across both periods
z <- (loggridList - minValue(loggridList)) / (maxLog- minValue(loggridList))

#write scaled files to new folder
dsn = here::here("Results/GRIDS/scaled/")
writeRaster(stack(z), filename = file.path(dsn, names(z)), bylayer=TRUE,format='GTiff', overwrite = T)

#combine fishing effort----
path = here("Results/GRIDS/scaled/")

filesfish <- dir(path=path,pattern="fish", all.files = TRUE, full.names  = T)
filesfish = lapply(filesfish, raster)
filesfish = stack(filesfish)
names(filesfish)
#sum by pre/post
GridPreNF =sum(filesfish[[-c(1:3)]], na.rm = T) 
names(GridPreNF) ="pre_fish"
GridPostNF = sum(filesfish[[c(1:3)]], na.rm = T)
names(GridPostNF) ="post_fish"

#mask Gully Z1 from post 2004 fish effort
GridPostNF <- mask(GridPostNF,GullyZ1, inverse = T)

# calc max for both periods
filesfish = stack(GridPostNF,GridPreNF)
maxLog = max(maxValue(filesfish))

#rescale between 0-1 using max values across both periods
z <- (filesfish - minValue(filesfish)) / (maxLog- minValue(filesfish))
z[z ==0] <- NA

writeRaster(z, filename = file.path(path, names(z)), bylayer=TRUE,format='GTiff', overwrite = T)

# remove gear type layers and leave only combined layer for calculating CHI
filesfish <- dir(path=path,pattern="fish", all.files = TRUE, full.names  = T)
filesfish = filesfish[-c(1,5)]
save = rep(here("Results/GRIDS/scaled/gearTypes/"), 6)
file.rename(from = filesfish, to = paste0(save,basename(filesfish), sep = "") )

#Shipping------
# - same underlying data layer for both periods, but use same script for consistency
# - read in data layers
pre_shipping = raster(here::here("Results/GRIDS/Effort/PRE/pre_shipping.tif"))
post_shipping = raster(here::here("Results/GRIDS/Effort/POST/post_shipping.tif"))

#stack
shipping = stack(pre_shipping, post_shipping)

#log 10 values
loggridList = log10(shipping+1)
# calc max for both periods
maxLog = max(maxValue(loggridList))

#rescale between 0-1 using max values across both periods
z <- (loggridList - minValue(loggridList)) / (maxLog- minValue(loggridList))

#write scaled files to new folder
dsn = here::here("Results/GRIDS/scaled/")
writeRaster(stack(z), filename = file.path(dsn, names(z)), bylayer=TRUE,format='GTiff', overwrite = T)


### SST anomalies-----
# - read in data layers
pre_sst = raster(here::here("Results/GRIDS/Effort/PRE/pre_SST.tif"))
post_sst = raster(here::here("Results/GRIDS/Effort/POST/post_SST.tif"))

#stack
sst = stack(pre_sst, post_sst)

#log 10 values
loggridList = log10(sst+1)
# calc max for both periods
maxLog = max(maxValue(loggridList))

#rescale between 0-1 using max values across both periods
z <- (loggridList - minValue(loggridList)) / (maxLog- minValue(loggridList))

#write scaled files to new folder
dsn = here::here("Results/GRIDS/scaled/")
writeRaster(stack(z), filename = file.path(dsn, names(z)), bylayer=TRUE,format='GTiff', overwrite = T)

