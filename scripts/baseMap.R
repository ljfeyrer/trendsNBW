# ### 1. Import basemap data:
# - This includes Coastlines, bathymetry, NBW habitat area, and conservation zones 
# - Code imports data and transforms to UTM Zone 20 where necessary
# - Extent of study area is based on area of influence around NBW habitat being a 50km buffer of the 1000m bathy line
# - Bathymetric data from Atlas of Canada available at: (https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/framework_cadre/North_America_Atlas10M/bathymetry/)
# - Conservation zones compiled from layers available as shapefiles. 
# - OECMS from https://open.canada.ca/data/en/dataset/44769543-7a23-4991-a53f-c2cf7c7a946f  
# - Ocean's Act MPAs from https://open.canada.ca/data/en/dataset/a1e18963-25dd-4219-a33f-1a38c4971250  
#     - Georges Bank Oil and Gas Exclusion zone from CNSOPB https://callforbids.cnsopb.ns.ca/2016/01/data-environment/gis-information  
#     - Northern bottlenose whale Critical Habitat designations from Species at Risk https://open.canada.ca/data/en/dataset/db177a8c-5d7d-49eb-8290-31e6a45d786c  
# 

#projection------

#UTM
UTM20 <- CRS("+init=epsg:32620") # CODE FOR UTM Zone 20


# boundbox for study area-----
bound <- st_bbox( c(xmin = -70,ymin = 38, xmax = -50, ymax =47 ), crs = st_crs(4326))
# bound <- detect_NAFO + c(-8, -3,60,12) # Increase the area around the box a bit (in degrees)
bound <- bound %>%st_as_sfc()%>% #turns the bounding box into a sfc object, that just describes a specific geometry
  st_sf()%>%st_transform(4326)
boundUTM <- bound %>%
  st_transform(UTM20)


# bathy data -------
# from Atlas of Canada Data (https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/framework_cadre/North_America_Atlas10M/bathymetry/)

bathy <- read_sf(here::here("Data/bathymetry_pl_v2/bathymetry_l_v2.shp"))

#change projection from Albers to Lat Long
bathy1 = bathy%>% 
  st_transform(4326)%>%st_intersection(bound)

#filter to show depths of interest
bathy = bathy1%>%
  filter(DEPTH >150)

#transform to UTM for later
bathyUTM = bathy%>% 
  st_transform(UTM20)

#NBW habitat area boundary --------
# (NBW habitat is 50 km buffer from the 1000m depth contour 
# to the extent of the Scotian Shelf population Designated Unit boundary identified by COSEWIC 2011)

nbw_hab = read_sf(here::here("Data/nbw_hab/nbw_hab.shp"))

#transform to UTM for later
nbw_habUTM = nbw_hab%>%st_transform(UTM20)%>%st_intersection(boundUTM)

#a template raster for later-------
template <- raster(extent(boundUTM), crs = projection(UTM20), res = 1000)
template_s = st_as_stars(template, as_points = FALSE, merge = FALSE)

##MPAs & conservation zones compiled from layers available as shapefiles -------
# OECMS from https://open.canada.ca/data/en/dataset/44769543-7a23-4991-a53f-c2cf7c7a946f
# Ocean's Act MPAs from https://open.canada.ca/data/en/dataset/a1e18963-25dd-4219-a33f-1a38c4971250
# Georges Bank Oil and Gas Exclusion zone from CNSOPB https://callforbids.cnsopb.ns.ca/2016/01/data-environment/gis-information
# Northern bottlenose whale Critical Habitat designations from Species at Risk
# https://open.canada.ca/data/en/dataset/db177a8c-5d7d-49eb-8290-31e6a45d786c

###### All compiled conservation zone information was cleaned, merged and clipped to the extent of the NBW habitat area used in analysis

ALL_MPAS = read_sf(here::here("Data/MPAS/All_MPAS.shp"))
ALL_MPAS_UTM= ALL_MPAS%>%
  st_transform(UTM20) 

#read in Gully Zones, transform to UTM Zone 20 
Gully_UTM <- read_sf(here::here("Data/MPAS/Gully_MPA.shp"))%>%st_transform(UTM20)
GullyZ1= Gully_UTM%>%filter(NAME == "Zone 1")

#need to divide conservation zones by period 
# MPA_data = read.csv(here::here("Data/csvs/ConservationZoneData.csv"),  stringsAsFactors = F)
# ALL_MPAS_UTM = left_join(ALL_MPAS_UTM, MPA_data, by = c("file" = "Layer"))
ALL_MPAS_UTM$area = st_area(ALL_MPAS_UTM)

#critical habitat
CritHab= ALL_MPAS_UTM%>%filter(Type == "Critical Habitat")

#MPAs before 2004
preMPA = ALL_MPAS_UTM%>%filter(Implmnt <2004)

#No AOI - only active MPAs
ACTIVE_MPAS= ALL_MPAS_UTM%>%filter(Type != "MPA AOI")
ACTIVE_MPAS = ACTIVE_MPAS%>%mutate(period = ifelse(Implmnt <2004, "Early Period", "Contemporary Period"))


# north america for reference------
land <- ne_countries( scale = "large",continent = "north america", returnclass = "sf")
landUTM = land%>%st_transform(UTM20) 

# import Ha detects
detectHa = read_sf(here::here("Data/Ha/Ha_sf.shp"))%>%st_transform(UTM20)%>%st_intersection(nbw_habUTM)
detectHa$speciesName = "Hyperoodon ampullatus"

#import Ha sightings
sightHa = read.csv(here::here("Data/Ha/HaSightings_2021.csv"))%>%mutate(LongDec  = (Longitude)*-1, LatDec = Latitude)
sightHa = st_as_sf(sightHa, coords = c("LongDec", "LatDec"), crs = 4326)%>%st_transform(UTM20)%>%st_intersection(nbw_habUTM)

#  plot basemap as object m-----
m<-ggplot() +
  scale_fill_viridis(discrete = T, option = "D")+
  theme_bw()+
  # add land region
  geom_sf(  data = land, color=NA, fill="grey50") +
  
  # add contours
  geom_sf(data = bathy, col = "gray", size = 0.2) +
  
  #add NBW area
  geom_sf(data = nbw_hab, col = "black",fill = NA, size = .15)+
  
  #add conservation zones
  geom_sf(data = ALL_MPAS, col = NA, aes( fill = Type), alpha = .5, size = .2)+
  
  #add sightings and detects of NBW
  
  geom_sf(data = sightHa, colour = "#ff593d", fill = NA, 
          size = 1.2, shape = 18) +
  geom_sf(data = detectHa, colour = "#93003a", fill = NA, 
          size = 1, shape = 1) +
  # set map limits
  coord_sf(xlim = c(-67, -51), ylim = c(40, 47)) +
  ##ff593d,#93003a
 
  guides(fill = guide_legend(ncol = 5))+
  # format axes
  ylab("") + 
  xlab("") +
  theme(plot.margin = margin(.1, .5, .1, .1, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top", legend.title = element_blank()    )
m

#build  map with box outline for inset
m1 = m +
  # add text annotation
  annotate(geom = "richtext", x = -59.5, y = 43.1, label = "Gully MPA",
           color = "black", size = 2.5, hjust=0, label.color = NA) +
  annotate(geom = "rect", xmin = -59.5, xmax = -57.7,ymin = 43.5,ymax = 44.4,
           fill = NA, 
           colour = "black",
           size = 0.6) +
 
# add scale bar
annotation_scale(location = "bl", width_hint=0.25,
                 text_cex = 0.6,
                 bar_cols = c("grey40", "white")) 

m1

#zoom map-----

mz = m+
  # add scale bar
  annotation_scale(location = "br", width_hint=0.25,
                   text_cex = 0.5,
                   bar_cols = c("grey40", "white")) +
  
    # add text annotation for canyons
   annotate(geom = "text", x = -57.88, y = 44.03, label = "Haldimand",fontface = "italic",
           color = "black", size = 2, ) +
  annotate(geom = "text", x = -58.3, y = 43.92, label = "Shortland",fontface = "italic",
           color = "black", size = 2, ) +
  annotate(geom = "text", x = -58.9, y = 43.55, label = "The Gully",fontface = "italic",
           color = "black", size = 2, ) +
  ggforce::theme_no_axes()+
  # format axes
  ylab("") + 
  xlab("") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none",
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "mm"))+
# set map limits
coord_sf(xlim = c(-59.5, -57.7), ylim = c(43.5, 44.4)) 

mz

#plot togethr---------
gg_Fig2 = cowplot::ggdraw() +
  draw_plot(m1) +
  draw_plot(mz, x = 0.66, y = 0.11, width = 0.3, height = 0.3)+

  draw_line(
    x = c(0.59, 0.666),
    y = c(.482, 0.405),
    color = "black", size = .5, type = 2
  )

 
#save map
 
 gg_Fig2path =  here::here("Results/FIGS/Fig2_map.png")
ggsave(gg_Fig2path, gg_Fig2, width = 7, height = 5, units = "in", dpi = 300)


