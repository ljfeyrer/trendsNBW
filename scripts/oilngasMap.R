# #### C. Oil & Gas Exploration and Operations
#
# - Publicly available spatial and temporal data was collated from the
# Canada-Nova Scotia Offshore Petroleum Board (C-NSOPB) and the
# Canada-Newfoundland and Labrador Offshore Petroleum Board (C-NLOPB) websites
# (CNLOPB, 2021; CNSOPB, 2021). Shapefiles for existing pipelines, wells,
# platforms, seismic surveys (2D/3D), exploratory licenses, significant
# discovery licenses, production licenses and sector announcements provided as
# part of the call for bids processes were downloaded, cleaned, and checked for
# duplication. - Activities occurring before and after 2004 were identified
# based on metadata in the shapefile or provided elsewhere on the website. - The
# extent of oil and gas development activities in the portion of the study area
# regulated by the C-NLOPB (i.e., off Newfoundland) may be incomplete but this
# is unclear due to current data embargoes. However, based on our review of
# public reports on activities in the area, likely most seismic surveys were
# included in our analysis.

##### i. Exploratory Activities -----

# - Oil & Gas Exploration includes seismic surveys, exploratory licenses and
# licensed activities such as shipping and test wells - Data includes seismic
# surveys and exploratory licenses - Individual seismic survey data only
# available for period up to 2004. Shapefiles for survey data was deduplicated,
# merged and shapefiles clipped to SDM area. Seismic surveys are represented as
# either 3D (polygons) or 2D (lines) - Exploratory Licenses have a 9 year term
# and post 2004 are only data available on exploratory activities

#Import 3D polygons & transform to UTM----
seismic3D = read_sf(here::here("Data/OnG/Seismic/filter_poly.shp"))%>%
  mutate(ID = row_number())
seismic3DUTM = seismic3D%>%st_transform(UTM20)

#Plot
ggplot()+ geom_sf(data = bathy, col = "grey",  size = 0.2)+
  geom_sf(data = nbw_hab, col = "black",fill = NA)+
  geom_sf(data = seismic3D, col = "orange", alpha = .5)+theme_bw()+ 
  labs(title = "3D Survey areas (polygons)")+
  coord_sf( xlim = c( -70, -50), ylim = c( 40,47 ))

#Import 2D lines & transform to UTM----
seismic2D = read_sf(here::here("Data/OnG/Seismic/flterYrseismiclines.shp"))
seismic2DUTM = seismic2D%>%st_transform(UTM20)

#Plot
ggplot()+ geom_sf(data = bathy, col = "grey",  size = 0.2)+
  geom_sf(data = nbw_hab, col = "black",fill = NA)+
  geom_sf(data = seismic2D, col = "orange", alpha = .5)+theme_bw()+ 
  labs(title ="2D Surveys (lines)")+coord_sf( xlim = c( -70, -50), ylim = c( 40,47 ))

#read in Exploratory Licenses----
ExLic = read_sf(here::here("Data/OnG/Seismic/EL_ALL.shp"))
ExLicUTM = ExLic%>%st_transform(UTM20)

#Plot
ggplot()+ geom_sf(data = bathy, col = "grey",  size = 0.2)+
  geom_sf(data = nbw_hab, col = "black",fill = NA, size= 0.2 )+
  geom_sf(data = ExLic, aes(fill = YEAR_S), col = NA, alpha = .5)+theme_bw()+ 
  labs(title ="Exploratory Licenses")+coord_sf( xlim = c( -70, -50), ylim = c( 40,47 ))

#filter Ex lic before/ after 2004
preExLicUTM = ExLicUTM%>%filter(YEAR_S <=2004) 
post_exOnG = ExLicUTM%>%filter(YEAR_S >2004) 

#grid shapefiles  ----
# uses Fasterize which is faster than raster

#2d Lines
dsn = here::here("Results/GRIDS/original/OnG_Ex/")
pointRaster(seismic2DUTM["ID"], template, dsn)

#3d Poly
polyRaster(seismic3DUTM, template, dsn)

#Exploratory Licenses   
polyRaster(preExLicUTM, template, dsn)

dsn2 = here::here("Results/GRIDS/Effort/POST/")
polyRaster(post_exOnG, template, dsn2)

#Import all rasters of exploratory activities----

#combine rasters for period pre 2004   
writepath = here::here("Results/GRIDS/Effort/PRE/")
file = "pre_exOnG"
SumRaster(dsn, writepath,file)
pre_exOnG =  raster(paste(writepath, "pre_exOnG.tif", sep = ""))

# period 2005-2019 - one raster
post_exOnG =  raster(paste(dsn2, "post_exOnG.tif", sep = ""))


#plot pre 2004 exploratory effort-------
pre_exOnG_df = st_as_stars(pre_exOnG)

m8 = ggplot() +  
  scale_fill_continuous(
    type = "viridis",
    direction = 1, trans = "log",
                        guide="colorbar",na.value=NA, limits = c(1, 25 ),
                        breaks = c(0, 5, 10, 20))+
  geom_sf(data = nbw_habUTM, col = "black",fill = NA, size = .2)+
  geom_stars(data = pre_exOnG_df, na.rm = T, downsample = 0)+
  geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
  # add land region
  geom_sf(  data = landUTM, color=NA, fill="grey50") +
  # add scale bar
  annotation_scale(location = "br", width_hint=0.25,
                   text_cex = 0.6,
                   bar_cols = c("grey40", "white")) +
  # set map limits
  # set map limits
  coord_sf(
    xlim = c(26000, 1518416),
    ylim = c(4451888, 5104112),
    crs =  st_crs(UTM20),
    expand = FALSE
  ) +
  # format axes
  ylab("") +
  xlab("") +
  theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + labs(title = "Oil & Gas Explorations 1988-2004") +
  theme_bw() +
  guides(fill = guide_colourbar(title = "Surveys"))+
  theme(legend.position = "bottom") 

m8

#plot post 2004 exploratory effort------
exOnG_post_df = st_as_stars(post_exOnG)

m9 = ggplot() +  
  scale_fill_continuous(
    type = "viridis",
    direction = 1, trans = "log",
    guide="colorbar",na.value=NA, limits = c(1, 25 ),
    breaks = c(0, 5, 10, 20))+
  geom_sf(data = nbw_habUTM, col = "black",fill = NA, size = .2)+
  geom_stars(data = exOnG_post_df, na.rm = T, downsample = 0)+
  geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
  # add land region
  geom_sf(  data = landUTM, color=NA, fill="grey50") +
  # add scale bar
  annotation_scale(location = "br", width_hint=0.25,
                   text_cex = 0.6,
                   bar_cols = c("grey40", "white")) +
  # set map limits
  # set map limits
  coord_sf(
    xlim = c(26000, 1518416),
    ylim = c(4451888, 5104112),
    crs =  st_crs(UTM20),
    expand = FALSE
  ) +
  # format axes
  ylab("") +
  xlab("") +
  theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + labs(title = "Oil & Gas Explorations 2005-2019") +
  theme_bw() +
  guides(fill = guide_colourbar(title = "Surveys"))+
  theme(legend.position = "bottom") 
m9


# ##### ii. Operational activities ------- 
# - Oil & Gas Operations includes wells, pipelines, and platforms which are
# treated as presence absence data as per Halpern et al. (2008) - Some of this
# infrastructure has been decommissioned since it was built, however the ongoing
# risk associated with pollution is assumed to continue over the entire study
# period.

###### a. Drilled wells------
# - Wells sorted by spud (drill) date, Operations, Considered footprint/PA data 

#read in well data, make UTM
wells = read_sf(here::here("Data/OnG/Operations/all_wells.shp"))
wellsUTM = wells%>%st_transform(UTM20)

#  sort by period
preWells = wellsUTM%>%filter(YearEnd <=2004)
postWells = wellsUTM

#plot all by year
ggplot()+ geom_sf(data = bathy, col = "grey",  size = 0.2)+
  geom_sf(data = nbw_hab, col = "black",size = .2, fill = NA)+
  geom_sf(data = wells, aes(col = YearEnd))+theme_bw()+
  labs(title ="All Wells")+coord_sf( xlim = c( -70, -50), ylim = c( 40,47 ))

#rasterize Pre- 2004 use function pointRaster
dsn = here::here("Results/GRIDS/original/PRE_Ops/")
pointRaster(preWells, template, dsn)

#set any cell with a well to 1 so P/A
preWells = raster(paste(dsn,"preWells.tif", sep = ""))
preWells[preWells >=1] <- 1 
writeRaster(preWells, filename = paste(dsn,"preWells.tif", sep = ""), 
            overwrite = TRUE,format='GTiff')

#Rasterize POST 2004 - use function pointRaster
dsn2 = here::here("Results/GRIDS/original/POST_Ops/")
pointRaster(postWells, template, dsn2)

#set any cell with a well to 1 so P/A
postWells = raster(paste(dsn2,"postWells.tif", sep = ""))
postWells[postWells >=1] <- 1 
writeRaster(postWells, filename = paste(dsn2,"postWells.tif", sep = ""), 
            overwrite = TRUE,format='GTiff')

#### b. Platforms--------
# - Platforms Operations, Considered footprint/PA data 

#read in data, make UTM
platforms = read_sf(here::here("Data/OnG/Operations/platformsYR.shp"))
platformsUTM = platforms%>%st_transform(UTM20)

#  sort by period
prePlatforms = platformsUTM%>%filter(YEAR <=2004)
postPlatforms = platformsUTM

#plot all by year
ggplot()+ geom_sf(data = bathy, col = "grey",  size = 0.2)+
  geom_sf(data = nbw_hab, col = "black",size = .2, fill = NA)+
  geom_sf(data = platforms, aes(col = YEAR))+theme_bw()+
  labs(title ="All Platforms")+coord_sf( xlim = c( -70, -50), ylim = c( 40,47 ))

#Rasterize Pre- 2004 use function pointRaster
pointRaster(prePlatforms, template, dsn)

#Rasterize POST 2004 - use function pointRaster
pointRaster(postPlatforms, template, dsn2)

#### c. Production licenses-------
# - Production licenses are similar impact to platforms, but polygons, Operations, Considered footprint/PA data 

#read in data, make UTM
ProdLic = read_sf(here::here("Data/OnG/Operations/Production_LicenceYR.shp"))
ProdLicUTM = ProdLic%>%st_transform(UTM20)

#plot all by year
ggplot()+ geom_sf(data = bathy, col = "grey",  size = 0.2)+
  geom_sf(data = nbw_hab, col = "black",size = .2, fill = NA)+
  geom_sf(data = ProdLic, aes(col = YEAR))+theme_bw()+
  labs(title ="All Production Licenses")+coord_sf( xlim = c( -70, -50), ylim = c( 40,47 ))

#  sort by period (operational date)
preProdLic = ProdLicUTM%>%filter(YEAR <=2004)
postProdLic = ProdLicUTM

##Rasterize Pre- 2004 use function polyRaster
polyRaster(preProdLic, template, dsn)

#Rasterize POST 2004 - use function polyRaster
polyRaster(postProdLic, template, dsn2)

#### d. Significant discovery licenses  --------
# - Significant discovery licenses require drilling and surveys, operations, Considered footprint/PA data
#read in data, make UTM

SigDisLic =read_sf(here::here("Data/OnG/Operations/SigDis_LicenceYR.shp"))
SigDisLicUTM = SigDisLic%>%st_transform(UTM20)

#plot all by year
ggplot()+ geom_sf(data = bathy, col = "grey",  size = 0.2)+
  geom_sf(data = nbw_hab, col = "black",size = .2, fill = NA)+
  geom_sf(data = SigDisLic, aes(col = YEAR))+theme_bw()+
  labs(title ="Significant Discovery Licenses")+coord_sf( xlim = c( -70, -50), ylim = c( 40,47 ))

#  sort by period
preSigLic = SigDisLicUTM%>%filter(YEAR <=2004)
postSigLic = SigDisLicUTM

##Rasterize Pre- 2004 use function polyRaster
polyRaster(preSigLic, template, dsn)

#Rasterize POST 2004 - use function polyRaster
polyRaster(postSigLic, template, dsn2)

### e. Pipelines -------
# - All built by 1999 so occur as impact in both pre & post periods, Operations, Considered footprint/PA data 

#read in data, make UTM
pipes =read_sf(here::here("Data/OnG/Operations/all_pipes.shp"))
pipesUTM = pipes%>%st_transform(UTM20)

#change name so consistent
prePipes= pipesUTM
postPipes= pipesUTM

#plot all by year
ggplot()+ geom_sf(data = bathy, col = "grey",  size = 0.2)+
  geom_sf(data = nbw_hab, col = "black",size = .2, fill = NA)+
  geom_sf(data = pipes, col = "blue")+theme_bw()+
  labs(title ="Pipelines")+coord_sf( xlim = c( -70, -50), ylim = c( 40,47 ))

##Rasterize Pre- 2004 use function pointRaster
pointRaster(prePipes, template, dsn)

#Rasterize POST 2004 - use function pointRaster
pointRaster(postPipes, template, dsn2) 

#Import and combine all footprints of operational activities -------

#Early Period
#combine all operational rasters for period pre 2004  
writepath = here("Results/GRIDS/scaled/pre_opOnG.tif")
dsn = dsn
footprntRaster(dsn, writepath)

#Contemporary Period
#combine all operational rasters for period 2005-2019 into one raster  
writepath = here("Results/GRIDS/scaled/post_opOnG.tif")
dsn = dsn2
footprntRaster(dsn, writepath)

#plot pre 2004 operational effort----------
#make stars object
Op_pre_df = read_stars(here("Results/GRIDS/scaled/pre_opOnG.tif"))

m10 = ggplot() +  
  scale_fill_viridis_c(
       na.value=NA)+
  geom_sf(data = nbw_habUTM, col = "black",fill = NA, size = .2)+
  geom_stars(data = Op_pre_df, na.rm = T, downsample = 0)+
  geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
  # add land region
  geom_sf(  data = landUTM, color=NA, fill="grey50") +
  # add scale bar
  annotation_scale(location = "br", width_hint=0.25,
                   text_cex = 0.6,
                   bar_cols = c("grey40", "white")) +
  # set map limits
  # set map limits
  coord_sf(
    xlim = c(26000, 1518416),
    ylim = c(4451888, 5104112),
    crs =  st_crs(UTM20),
    expand = FALSE
  ) +
  # format axes
  ylab("") +
  xlab("") +
  theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + labs(title = "Oil & Gas Operations 1988-2004") +
  theme_bw() +
  theme(legend.position = "none") 
m10

#plot post 2004 operational effort----------
#make stars object
Op_post_df = read_stars(here("Results/GRIDS/scaled/post_opOnG.tif"))

m11 = ggplot() +  
  scale_fill_viridis_c(
    na.value=NA)+
  geom_sf(data = nbw_habUTM, col = "black",fill = NA, size = .2)+
  geom_stars(data = Op_post_df, na.rm = T, downsample = 0)+
  geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
  # add land region
  geom_sf(  data = landUTM, color=NA, fill="grey50") +
  # add scale bar
  annotation_scale(location = "br", width_hint=0.25,
                   text_cex = 0.6,
                   bar_cols = c("grey40", "white")) +
  # set map limits
  # set map limits
  coord_sf(
    xlim = c(26000, 1518416),
    ylim = c(4451888, 5104112),
    crs =  st_crs(UTM20),
    expand = FALSE
  ) +
  # format axes
  ylab("") +
  xlab("") +
  theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + labs(title = "Oil & Gas Operations 2005-2019") +
  theme_bw() +
  guides(fill = guide_colourbar(title = "Operations"))+
  theme(legend.position = "none") 
m11

