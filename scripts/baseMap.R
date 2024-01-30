# ### 1. Import basemap data for NBW trends analysis 2024

#updated
# - This includes Coastlines, bathymetry, NBW habitat area, and conservation zones 
# - Code imports data and transforms to UTM Zone 20 where necessary
# - Bathymetric data from Atlas of Canada available at: (https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/framework_cadre/North_America_Atlas10M/bathymetry/)
# - Conservation zones compiled from layers available as shapefiles. 
# - OECMS from https://open.canada.ca/data/en/dataset/44769543-7a23-4991-a53f-c2cf7c7a946f  
# - Ocean's Act MPAs from https://open.canada.ca/data/en/dataset/a1e18963-25dd-4219-a33f-1a38c4971250  
#     - Georges Bank Oil and Gas Exclusion zone from CNSOPB https://callforbids.cnsopb.ns.ca/2016/01/data-environment/gis-information  
#     - Northern bottlenose whale Critical Habitat designations from Species at Risk https://open.canada.ca/data/en/dataset/db177a8c-5d7d-49eb-8290-31e6a45d786c  
# 

pacman::p_load(sp, terra, dplyr, sf, viridis, ggplot2, ggrepel, stringr, here, ggtext, readr, grid,
               rnaturalearth, rnaturalearthdata, pals, tidyr, fuzzyjoin, patchwork,mapsf,
               ggforce, readr, ggspatial, lubridate, stars, patchwork, scales, RColorBrewer, grafify)

sf_use_s2(FALSE)

#projection------

          #UTM
          UTM20 <- terra::crs("+init=epsg:32620") # CODE FOR UTM Zone 20

# bound boxes for plots / study areas -----
          GEO_BOUND =  st_bbox( c(xmin = -75,ymin = 38, xmax = -40, ymax =60 ), crs = st_crs(4326))%>%
            st_as_sfc()%>% st_sf()
          
          UTM_BOUND = st_bbox(GEO_BOUND)%>%st_as_sfc()%>% st_sf()%>%st_transform(UTM20)
          
          # bound box SS data for inset-----
          Bound_boxB <- st_bbox( c(xmin = -70,ymin = 40, xmax = -48, ymax =48 ), crs = st_crs(4326))
          Bound_boxB <- Bound_boxB %>%
            st_as_sfc()%>% #turns the bounding box into a sfc object, that just describes a specific geometry
            st_sf()
          Bound_boxBUTM <- Bound_boxB%>%st_transform(UTM20)
          
          ext(Bound_boxBUTM)
          #Region boundaries ------
          SShelf <-read_sf("~/CODE/prepWSDB/shapefiles/DFO_NAFO_EEZ_Land.shp")%>%dplyr::filter(DFO_REGION == "MAR")%>%st_transform(4326)
          Gulf <-read_sf("~/CODE/prepWSDB/shapefiles/DFO_NAFO_EEZ_Land.shp")%>%dplyr::filter(DFO_REGION == "GULF")%>%st_transform(4326)
          QC <-read_sf("~/CODE/prepWSDB/shapefiles/DFO_NAFO_EEZ_Land.shp")%>%dplyr::filter(DFO_REGION == "QC")%>%st_transform(4326)
          
          
          plot(st_geometry(SShelf))
          # write_sf(SShelf, "~/CODE/shapefiles/DFORegions/ScotianShelf.shp", overwrite = T)

          ### Land ------
          #all countries
          
          land <- read_sf(here::here("~/CODE/shapefiles/coastline/worldcountries/ne_50m_admin_0_countries.shp"))%>%dplyr::filter(CONTINENT == "North America")
          landUTM = land%>%st_transform(UTM20) %>%st_intersection(UTM_BOUND)
          
          
          ### bathy data -------
          r <- terra::rast("~/CODE/shapefiles/Bathymetry/GEBCO_bathy/gebco_2020.tif")
          ext(r)
          crs(r)
          
          #need to downsample bc too big
          bathy = terra::aggregate(r, fact = 2)
          # bathy_df <- as.data.frame(bathy, xy = T)%>%dplyr::rename(Depth = gebco_2020)
          
          #now crop & project to UTM extent of GEOGRAPHIC area
          bathy_UTM = bathy%>%crop(GEO_BOUND)%>%terra::project("EPSG:32620")
          
          bathy<- as.data.frame(bathy_UTM, xy = T)%>%dplyr::rename(Depth = gebco_2020)%>%
            mutate(Depth = ifelse(Depth >=-10, NA, Depth))
          
          #now crop to extent of Gully area
          bathy_crop = bathy_UTM%>%crop(Bound_boxBUTM)
          
          crs(bathy_crop)
          
          bathy_cropUTM <- as.data.frame(bathy_crop, xy = T)%>%dplyr::rename(Depth = gebco_2020)%>%
            mutate(Depth = ifelse(Depth >=-10, NA, Depth))
          
          ###contours----
          cont <- as.contour(r, levels= c( -200, -350, -400, -500,-1000,-2000,-2500,-3000, -3200, -4000, -5000))
          cont <- st_as_sf(cont)%>%st_cast("LINESTRING")
          cont_UTM = cont%>%st_transform(UTM20)%>%st_intersection(UTM_BOUND)
                                    
    #MPAs & conservation zones compiled from layers available as shapefiles ----
   
          ###### Compile all conservation zone information
          #OA MPAS - some topology errors that need repair
          
          ALL_MPAS = read_sf(here::here("~/CODE/shapefiles/ProtectedAreas/DFO/OA_MPAs/DFO_MPA_MPO_ZPM.shp"))%>%
            st_transform(4326)%>%st_make_valid()%>%st_intersection(GEO_BOUND)
         # write_sf(ALL_MPAS, "shapes/SMIF/EastCan_OAMPAS.shp", overwrite = T)
          
           ALL_MPAS_UTM= ALL_MPAS%>%
            st_transform(UTM20) 
          # plot(st_geometry(ALL_MPAS))
          
          #OECMS
          OECMS = read_sf(here::here("~/CODE/shapefiles/ProtectedAreas/DFO/OECMS/DFO_OECM_MPO_AMCEZ.shp"))%>%st_transform(4326)%>%
            st_make_valid()%>%st_intersection(Bound_boxB)%>%dplyr::filter(REGION_E != "Gulf", REGION_E != "Quebec")
          plot(st_geometry(OECMS))
          
          # write_sf(OECMS, "shapes/SMIF/EastCan_OECMS.shp", overwrite = T)
          
          # OECMS_UTM= OECMS%>%
            # st_transform(UTM20) 
          
          
          #O&G exclusions
          gbPA = read_sf(here::here("~/CODE/shapefiles/ProtectedAreas/OilnGas/Georges_Bank_exclusion_zone.shp"))
          
          # write_sf(gbPA, "shapes/SMIF/EastCan_OilnGas_closure.shp", overwrite = T)
          
          
          # gbPA_UTM= gbPA%>%
            # st_transform(UTM20) 
          
          #NAFO Closures
          #from http://www.atlas-horizon2020.eu/layers/geonode:a__2019_Closures_sponge_coral
          #no closures on SS shelf %>%st_intersection(SShelf)
          Coral = read_sf(here::here("~/CODE/shapefiles/ProtectedAreas/NAFO_closures/a__2019_Closures_sponge_coral.shp"))%>%st_transform(4326)%>%
            st_make_valid()
          
          # plot(st_geometry(Coral))
          
          Coral_UTM= Coral%>%
            st_transform(UTM20) 
          
          # #FUNDIAN AOI
          # Fundian <- read_sf(here::here("~/CODE/shapefiles/ProtectedAreas/DFO/FundianAOI/FundianChannel_BrownsBank_AOI_poly.shp"))
          # Fundian_UTM=Fundian%>%
          #   st_transform(UTM20)
          
          #other critical habitat areas
          whale_CH <- sf::st_read("~/CODE/shapefiles/SAR_CH/NARW_CH/NARW_CH.shp")%>%st_transform(4326)%>%st_intersection(Bound_boxB)
          
          plot(st_geometry(whale_CH))
          
          ##Whale Habitat Areas ---------
          # CH - 
          NBW_CH <- read_sf(here::here("~/CODE/shapefiles/SAR_CH/NBW_CH/NorthernBottlenoseWhale_CH.shp"))
          NARW_CH <- read_sf(here::here("~/CODE/shapefiles/SAR_CH/NARW_CH/NARW_CH.shp"))
          # plot(st_geometry(NBW_CH))
          
          # write_sf(NBW_CH, "shapes/SMIF/NBW_CH.shp", overwrite = T)
          NBW_CH_UTM=NBW_CH%>%
            st_transform(UTM20)
          
          ##NBW IMP HAB 2023 area  
          nbw_ImHab2023 = read_sf(here::here("~/CODE/shapefiles/ImpHabitat/Feyreretal2024/NBW_ImHabitat2023.shp"))%>%
            st_transform(4326)%>%st_intersection(Bound_boxB)
          
          Blue_ImHab2023 = read_sf(here::here("~/CODE/shapefiles/ImpHabitat/RorqualBleu_AiresImportantes/RorqualBleu_AiresImportantes.shp"))%>%
            st_transform(4326)%>%st_difference(Gulf)%>%st_difference(QC)%>%st_intersection(Bound_boxB)
          

          plot(st_geometry(Blue_ImHab2023))
          
          # write_sf(nbw_ImHab2023, "shapes/SMIF/SS_NBW_ImpHab.shp", overwrite = T)
          # write_sf(Blue_ImHab2023, "shapes/SMIF/SS_BW_ImpHab.shp", overwrite = T)
          
          # ext(nbw_ImHab2023)
          # plot(st_geometry(nbw_ImHab2023))
          
          #read in Gully Zones, transform to UTM Zone 20 
          Gully_UTM <- read_sf(here::here("~/CODE/shapefiles/ProtectedAreas/DFO/Gully/Gully_MPA.shp"))%>%st_transform(UTM20)
          GullyZ1= Gully_UTM%>%filter(NAME == "Zone 1")


#a template raster for later???
template <- raster(extent(boundUTM), crs = projection(UTM20), res = 1000)
template_s = st_as_stars(template, as_points = FALSE, merge = FALSE)


###### Compile all conservation zone information-----
# cleaned, merged and clipped to the extent of the NBW habitat area used in analysis

              ALL_MPAS = read_sf(here::here("~/CODE/archive/trendsNBW1/Data/MPAS/All_MPAS.shp"))
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

#  PLOT basemap as object m-----
              m <- ggplot() + theme_bw() +
                # Assign a dummy variable for each layer
                geom_sf(data = SShelf, aes(fill = "Maritimes"), col = NA, alpha = .5, size = .2) +
                geom_sf(data = Gulf, aes(fill = "Gulf"), col = NA, alpha = .5, size = .2) +
                geom_sf(data = QC, aes(fill = "QC"), col = NA, alpha = .5, size = .2) +
                geom_sf(data = land, aes(fill = "Land"), color = NA) +
                geom_sf(data = cont %>% filter(level == -200 | level == -1000 | level == -2000), aes(color = "Contours")) +
                geom_sf(data = nbw_ImHab2023, aes(fill = "NBW Imp Hab"), col = "black", alpha = .2, size = .15) +
                geom_sf(data = Blue_ImHab2023, aes(fill = "Blue Imp Hab"), col = "blue", alpha = .2, size = .5) +
                geom_sf(data = ALL_MPAS, aes(fill = "OA MPAs"), col = NA, alpha = .5, size = .2) +
                geom_sf(data = OECMS, aes(fill = "OECMS"), col = NA, alpha = .5, size = .2) +
                geom_sf(data = gbPA, aes(fill = "G.B. Excl."), col = NA, alpha = .5, size = .2) +
                geom_sf(data = NBW_CH, aes(fill = "NBW CH"), col = "red", size = .15) +
                geom_sf(data = NARW_CH, aes(fill = "NARW CH"), col = "red", size = .15) +
                coord_sf(xlim = c(-67.5, -51), ylim = c(40, 48)) +
                ylab("") + xlab("") +
                theme(plot.margin = margin(.1, .5, .1, .1, "cm"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      legend.position = "bottom", legend.title = element_blank()) +
                # Define legends
                scale_fill_manual(values = c("Maritimes" = "yellow", "Gulf" = "pink", "QC" = "purple",
                                             "Land" = "grey80", "OA MPAs" = "blue", "OECMS" = "green",
                                             "G.B. Excl." = "orange", "NBW Imp Hab" = "black", 
                                             "Blue Imp Hab" = "blue", "NBW CH" = "red", "NARW CH" = "red"),
                                  name = "Layer Type") +
  scale_color_manual(values = c("Contours" = "gray"), name = "Layer Type")+
                guides(fill = guide_legend(override.aes = list(color = NA)))
              

m              
    
#save map
#  
 gg_Fig2path =  here::here("shapes/SMIF/Summary_MCAS.png")
ggsave(gg_Fig2path, m, width = 7, height = 5, units = "in", dpi = 300)

          

# #build  map with box outline for inset
# m1 = m +
#   # add text annotation
#   annotate(geom = "richtext", x = -59.5, y = 43.1, label = "Gully MPA",
#            color = "black", size = 2.5, hjust=0, label.color = NA) +
#   annotate(geom = "rect", xmin = -59.5, xmax = -57.7,ymin = 43.5,ymax = 44.4,
#            fill = NA, 
#            colour = "black",
#            size = 0.6) +
#  
# # add scale bar
# annotation_scale(location = "bl", width_hint=0.25,
#                  text_cex = 0.6,
#                  bar_cols = c("grey40", "white")) 
# 
# m1
# 
# #zoom map-----
# 
# mz = m+
#   # add scale bar
#   annotation_scale(location = "br", width_hint=0.25,
#                    text_cex = 0.5,
#                    bar_cols = c("grey40", "white")) +
#   
#     # add text annotation for canyons
#    annotate(geom = "text", x = -57.88, y = 44.03, label = "Haldimand",fontface = "italic",
#            color = "black", size = 2, ) +
#   annotate(geom = "text", x = -58.3, y = 43.92, label = "Shortland",fontface = "italic",
#            color = "black", size = 2, ) +
#   annotate(geom = "text", x = -58.9, y = 43.55, label = "The Gully",fontface = "italic",
#            color = "black", size = 2, ) +
#   ggforce::theme_no_axes()+
#   # format axes
#   ylab("") + 
#   xlab("") +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),legend.position = "none",
#         legend.title = element_blank(),
#         panel.border = element_rect(colour = "black", fill=NA, size=1)) +
#   theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "mm"))+
# # set map limits
# coord_sf(xlim = c(-59.5, -57.7), ylim = c(43.5, 44.4)) 
# 
# mz
# 
# #plot togethr---------
# gg_Fig2 = cowplot::ggdraw() +
#   draw_plot(m1) +
#   draw_plot(mz, x = 0.66, y = 0.11, width = 0.3, height = 0.3)+
# 
#   draw_line(
#     x = c(0.59, 0.666),
#     y = c(.482, 0.405),
#     color = "black", size = .5, type = 2
#   )
# 
#  
#
# 
