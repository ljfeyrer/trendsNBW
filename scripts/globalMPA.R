## geom_sf with X11
X11(type = "cairo")

#library---------
library(cowplot)
library(patchwork)
library(ggforce)
library(tidyverse)
library(sf)
library(mapdata)
library(marmap)
library(ggspatial)
library(stars)
library(raster)
library(viridis)
library("rnaturalearth")
library("rnaturalearthdata")
library(here)

# land <- map_data("world2Hires")%>% 
#   mutate(long = (360 - long)*-1)

world <- ne_countries(scale = "medium", returnclass = "sf")

crs(world)

#contours

ne_200 = read_sf("/Users/chirp/DRIVE/CODE/Mapping/shapefiles/ne_10m_bathymetry_all/ne_10m_bathymetry_K_200.shp")
ne_1000 = read_sf("/Users/chirp/DRIVE/CODE/Mapping/shapefiles/ne_10m_bathymetry_all/ne_10m_bathymetry_J_1000.shp")


ne_200 = st_buffer(ne_200, dist=0)
ne_200 = st_make_valid(ne_200)

#import global MPA dataset and count pelagic MPAs
MPA0 = read_sf("/Users/chirp/DRIVE/CODE/Mapping/shapefiles/GLobalMPAs/WDPA_WDOECM_May2021_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp_0/WDPA_WDOECM_May2021_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp-polygons.shp")
MPA1 = read_sf("/Users/chirp/DRIVE/CODE/Mapping/shapefiles/GLobalMPAs/WDPA_WDOECM_May2021_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp_1/WDPA_WDOECM_May2021_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp-polygons.shp")
MPA2 = read_sf("/Users/chirp/DRIVE/CODE/Mapping/shapefiles/GLobalMPAs/WDPA_WDOECM_May2021_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp_2/WDPA_WDOECM_May2021_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp-polygons.shp")

MPA0_old = MPA0%>%filter(STATUS_YR <2010)
MPA1_old = MPA1%>%filter(STATUS_YR <2010)
MPA2_old = MPA2%>%filter(STATUS_YR <2010)

all_old =   st_as_sf(data.table::rbindlist(list(MPA0_old, MPA1_old, MPA2_old), fill = TRUE))
all_global =   st_as_sf(data.table::rbindlist(list(MPA0, MPA1, MPA2), fill = TRUE))

all_global = all_global%>%mutate(DECADE = ifelse(STATUS_YR >= 2010, "2010",
                                                 ifelse((STATUS_YR >= 2000 & STATUS_YR <2010), "2000",
                                   
                                                                             ifelse(STATUS_YR <2000, "1990 or before", "UNK" ))))
pelagic = all_global%>%st_intersection(ne_200)
pelagic$AREA = units::set_units(st_area(pelagic), km^2)


#summary stats on pelagic MPAs (> 200m deep)
pelagic_df = as.data.frame(pelagic)
pelagic_df%>%group_by(DECADE, NAME)%>%summarise(count = n())%>%summarise(count = n())
byYear = pelagic_df%>%group_by(DECADE, STATUS_YR, NAME)%>%summarise(AREA_sum = sum(AREA), count = n())%>%filter(STATUS_YR != 0)

sumYR_MPAs = byYear%>%group_by(DECADE)%>%summarise(AREA_sum = sum(AREA_sum), count = n())

pre = sumYR_MPAs%>%filter(DECADE != "2010")
post = sumYR_MPAs%>%filter(DECADE == "2010")

#% increase
# Final Value−Starting Value/Starting Value * 100

#area
final = sum(sumYR_MPAs$AREA_sum)
start = sum(pre$AREA_sum)
((final - start)/ start)*100

#count
final = sum(sumYR_MPAs$count)
start = sum(pre$count)
((final - start)/ start)*100



write_csv(sumYR_MPAs, here::here("Results/Global_MPAstats.csv"))
hist(byYear$STATUS_YR)

crs(MPA0)

pelagic_plot = ggplot()+ geom_sf(data = world, color=NA, fill="grey50") +
  geom_sf(data = pelagic,  color=NA, aes(fill=DECADE)) +theme_bw()+scale_fill_viridis_d()+ theme(legend.title = element_blank())

ggsave("/Users/chirp/DRIVE/CODE/Trends/Results/FIGS/globalMpas.png", width = 11.5, height = 8, units = "in", dpi = 300, pelagic_plot )  
          