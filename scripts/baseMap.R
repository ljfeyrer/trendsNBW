# ### 1. Import basemap data for NBW trends analysis 2024

#updated
# - This includes Coastlines, bathymetry, NBW habitat area, and conservation zones 
# - Code imports shapefile data and transforms to UTM Zone 20 where necessary
# - Bathymetric data from Atlas of Canada available at: (https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/framework_cadre/North_America_Atlas10M/bathymetry/)
# - Conservation zones compiled from layers available as shapefiles. 
# - OECMS from https://open.canada.ca/data/en/dataset/44769543-7a23-4991-a53f-c2cf7c7a946f  
# - Ocean's Act MPAs from https://open.canada.ca/data/en/dataset/a1e18963-25dd-4219-a33f-1a38c4971250  
#     - Georges Bank Oil and Gas Exclusion zone from CNSOPB https://callforbids.cnsopb.ns.ca/2016/01/data-environment/gis-information  
#     - Northern bottlenose whale Critical Habitat designations from Species at Risk https://open.canada.ca/data/en/dataset/db177a8c-5d7d-49eb-8290-31e6a45d786c  
# 

pacman::p_load(sp, terra, dplyr, sf, viridis, ggplot2, ggrepel, stringr, here, ggtext, readr, grid,
               pals, tidyr, fuzzyjoin, patchwork, mapsf,
               ggforce, readr, ggspatial, lubridate, stars, patchwork, scales, RColorBrewer, grafify)

sf_use_s2(FALSE)

#projection------
          #UTM
          UTM20 <- terra::crs("+init=epsg:32620") # CODE FOR UTM Zone 20

# bound boxes for plots / study areas -----
          GEO_BOUND =  st_bbox( c(xmin = -75,ymin = 38, xmax = -40, ymax =60 ), crs = st_crs(4326))%>%
            st_as_sfc()%>% st_sf()
          # 
          UTM_BOUND = st_bbox(GEO_BOUND)%>%st_as_sfc()%>% st_sf()%>%st_transform(UTM20)
          
          # bound box SS data for inset-----
          Bound_boxB <- st_bbox( c(xmin = -70,ymin = 40, xmax = -48, ymax =48 ), crs = st_crs(4326))
          Bound_boxB <- Bound_boxB %>%
            st_as_sfc()%>% #turns the bounding box into a sfc object, that just describes a specific geometry
            st_sf()
          Bound_boxBUTM <- Bound_boxB%>%st_transform(UTM20)
          
          terra::ext(Bound_boxBUTM)
          #Region boundaries ------
          REGIONS <-read_sf("~/CODE/prepWSDB/shapefiles/DFO_NAFO_EEZ_Land.shp")%>%
            st_transform(4326)%>%st_transform(UTM20)
          
          SShelf <-REGIONS%>%dplyr::filter(DFO_REGION == "MAR")
          Gulf <-REGIONS%>%dplyr::filter(DFO_REGION == "GULF")
          QC <-REGIONS%>% dplyr::filter(DFO_REGION == "QC")
         NF <-REGIONS%>%dplyr::filter(DFO_REGION == "NFLD")
          
         terra::ext(SShelf)
          # plot(st_geometry(SShelf))
          # write_sf(SShelf, "~/CODE/shapefiles/DFORegions/ScotianShelf.shp", overwrite = T)

          ### Land ------
          #all countries
          
          land <- read_sf(here::here("~/CODE/shapefiles/coastline/worldcountries/ne_50m_admin_0_countries.shp"))%>%
           dplyr::filter(CONTINENT == "North America")
          landUTM = land%>%st_transform(UTM20) %>%st_intersection(Bound_boxBUTM)
          
          
          ### bathy data -------
          #original bathy data might be a bit smoother
          bathy <- read_sf(here::here("shapes/bathymetry_pl_v2/bathymetry_l_v2.shp"))%>%
            st_transform(UTM20)%>%st_intersection(Bound_boxBUTM)           #change projection from Albers to Lat Long

          
          #filter 
          bathy = bathy%>%
            filter(DEPTH >150)
       
                                    
    #MPAs & conservation zones compiled from layers available as shapefiles ----
   
          ###### Compile all conservation zone information
          #OA MPAS - some topology errors that need repair
          
          ALL_MPAS_UTM = read_sf(here::here("~/CODE/shapefiles/ProtectedAreas/DFO/OA_MPAs/DFO_MPA_MPO_ZPM.shp"))%>%
            st_transform(4326)%>%st_make_valid()%>%st_intersection(Bound_boxB)%>% st_transform(UTM20) %>%mutate(NAME = NAME_E)
          # plot(st_geometry(ALL_MPAS_UTM))
          
          #OECMS
          OECMS_UTM = read_sf(here::here("~/CODE/shapefiles/ProtectedAreas/DFO/OECMS/DFO_OECM_MPO_AMCEZ.shp"))%>%
            st_transform(4326)%>%
            st_make_valid()%>%st_intersection(Bound_boxB)%>%dplyr::filter(REGION_E != "Gulf", REGION_E != "Quebec")%>%
            st_transform(UTM20)%>%mutate(NAME = NAME_E)
          
          EC_MCA = OECMS_UTM%>%filter(NAME == "Eastern Canyons Conservation Area")
          
          # plot(st_geometry(OECMS_UTM))
          
          #O&G exclusions
          gbPA_UTM = read_sf(here::here("~/CODE/shapefiles/ProtectedAreas/OilnGas/Georges_Bank_exclusion_zone.shp"))%>%
          st_transform(UTM20)%>%mutate(NAME = "GB O&G Exclusion")
          
                    #read in Gully Zones, transform to UTM Zone 20 
                    Gully_UTM <- read_sf(here::here("~/CODE/shapefiles/ProtectedAreas/DFO/Gully/Gully_MPA.shp"))%>%
                      st_transform(UTM20)
                    GullyZ1= Gully_UTM%>%filter(NAME == "Zone 1")
                    Gully_UTM = Gully_UTM%>%mutate(NAME == "Gully MPA")
                    
          # #NAFO Closures
          # #from http://www.atlas-horizon2020.eu/layers/geonode:a__2019_Closures_sponge_coral
          # #no closures on SS shelf %>%st_intersection(SShelf)
          # 
                    Coral_UTM = read_sf(here::here("~/CODE/shapefiles/ProtectedAreas/NAFO_closures/a__2019_Closures_sponge_coral.shp"))%>%st_transform(4326)%>%
            st_make_valid()%>% st_intersection(Bound_boxB)%>%
            st_transform(UTM20) %>%mutate(NAME = "Coral Closure")
          
          # #NARW critical habitat areas
          # NARW_CH_UTM <- sf::st_read("~/CODE/shapefiles/SAR_CH/NARW_CH/NARW_CH.shp")%>%st_transform(4326)%>%
          #   st_intersection(Bound_boxB)%>%st_union()%>%
          #   st_transform(UTM20)
          # plot(st_geometry(NARW_CH_UTM))
          
          #NBW Habitat Areas ---------
          NBW_CH_UTM <- read_sf(here::here("~/CODE/shapefiles/SAR_CH/NBW_CH/NorthernBottlenoseWhale_CH.shp"))%>%
            st_transform(UTM20)%>%mutate(NAME = DESCRIP)
          
          ##NBW IMP HAB 2023 area  
          nbw_ImHab2023_UTM = read_sf(here::here("~/CODE/shapefiles/ImpHabitat/Feyreretal2024/NBW_ImHabitat2023.shp"))%>%
            st_transform(4326)%>%st_intersection(Bound_boxB)%>%
            st_transform(UTM20)
         #bluewhale imp habitat
          Blue_ImHab2023_UTM = read_sf(here::here("~/CODE/shapefiles/ImpHabitat/RorqualBleu_AiresImportantes/RorqualBleu_AiresImportantes.shp"))%>%
            st_transform(4326)%>%st_transform(UTM20)%>%st_difference(Gulf)%>%st_difference(QC)%>%
            st_intersection(Bound_boxBUTM)%>%st_union()
          
          # plot(st_geometry(Blue_ImHab2023_UTM))
          
          ##NBW sightings & detections-----------
          
          # import Ha detects
          #  = TOWED ARRAY & AMAR
          BW_detects = read_sf("~/CODE/TowedArray/Shapes/BW.PAM_sf.shp")%>%
            filter(species == "Ha")%>%st_transform(UTM20)%>%st_intersection(SShelf)
          
          # plot(st_geometry(BW_detects))
          
          #import Ha sightings
          sightHa = read_sf("~/CODE/shapefiles/Ha_locations/Ha_locations_2023.shp")%>%
           st_transform(UTM20)%>%st_intersection(SShelf)
          # plot(st_geometry(sightHa))
          
     
#create one layer for PAs
        MCAs =   rbind(Gully_UTM%>%select(NAME), NBW_CH_UTM%>%select(NAME)
                       ) %>%filter(NAME == "Zone 1" | NAME == "Haldimand Canyon"| NAME == "Shortland Canyon"|
                                     NAME == "Gully MPA (Marine Protected Area), outer boundary" )
        # plot(st_geometry(MCAs))
       #not USED MCAS:  gbPA_UTM%>%select(NAME),OECMS_UTM%>%select(NAME), ALL_MPAS_UTM%>%select(NAME), Coral_UTM%>%select(NAME)
        # %>%st_intersection(SShelf)
        
###### Compile all conservation zone information-----

              # #need to divide conservation zones by period 
              # MPA_data = read.csv(here::here("inputs/csvs/ConservationZoneData.csv"),  stringsAsFactors = F)
              # # ALL_MPAS_UTM = left_join(ALL_MPAS_UTM, MPA_data, by = c("file" = "Layer"))
              # ALL_MPAS_UTM$area = st_area(ALL_MPAS_UTM)
              # 
              # #critical habitat
              # CritHab= ALL_MPAS_UTM%>%filter(Type == "Critical Habitat")
              # 
              # #MPAs before 2004
              # preMPA = ALL_MPAS_UTM%>%filter(Implmnt <2004)
              # 
              # #No AOI - only active MPAs
              # ACTIVE_MPAS= ALL_MPAS_UTM%>%filter(Type != "MPA AOI")
              # ACTIVE_MPAS = ACTIVE_MPAS%>%mutate(period = ifelse(Implmnt <2004, "Early Period", "Contemporary Period"))

#  PLOT basemap as object m-----
        
        # Define the coordinate range in UTM 20
        x_min <- 712179.56
        x_max <- 1009419.59
        y_min <- 4695205.74
        y_max <- 4993983.86
        
        
              m <- ggplot() + theme_bw() +
                # Assign a dummy variable for each layer to hack legend
                geom_sf(data = nbw_ImHab2023_UTM, aes(col = "Important Habitat"), fill = NA, 
                        alpha = .1, linewidth = 1) +
                geom_sf(data = landUTM, aes( color = "Sable Island"), fill = "#E1BF92", linewidth = .8) +
                geom_sf(data = bathy, color = "gray") +
                geom_sf(data = Gully_UTM%>%filter(NAME == "Gully MPA (Marine Protected Area), outer boundary"), 
                        aes(col = "The Gully"), fill = NA,  linewidth = .8) +
                geom_sf(data = NBW_CH_UTM, aes(fill = "Critical Habitat"), col = "black", alpha = .5, 
                        linewidth= 1) +
               
                #add sightings and detects of NBW
                
                geom_sf(data = sightHa, colour = "#ff593d", fill = NA, 
                        size = 1.2, shape = 18) +
                geom_sf(data = BW_detects, colour = "#93003a", fill = NA, 
                        size = 1, shape = 1) +
                
                coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE) +
                ylab("") + xlab("") +
                theme(plot.margin = margin(.1, .5, .1, .1, "cm"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      legend.position = "bottom", legend.title = element_blank()) +
                # Define legends
                
                scale_fill_manual(values = c(
                                             
                                              
                                              "Critical Habitat" = NA),
                                  name = "Layer Type") +
  scale_color_manual(values = c("The Gully" = "Black", "Sable Island" = "#9D8566", "Important Habitat" = "#FFD300"), name = "Layer Type")+
                # guides(fill = guide_legend(override.aes = list(color = NA)))+
              

# # add scale bar
  annotation_scale(location = "br", width_hint=0.25,
                   text_cex = 0.75,
                   bar_cols = c("grey40", "white")) +

          annotate(geom = "text", x = 832202.7 , y = 4839000, label = "The Gully MPA", fontface = "italic",
                   color = "black", size = 3) +
          annotate(geom = "text", x = 877340.6 , y = 4873735, label = "Shortland", fontface = "italic",
                   color = "black", size = 3) +
          annotate(geom = "text", x = 910303.4 , y = 4887964, label = "Haldimand", fontface = "italic",
                   color = "black", size = 3)

        
        print(m)
        
        #save map
#  
 gg_Fig2path =  here::here("figs/Fig_1b.png")
ggsave(gg_Fig2path, m,  height = 7, width = 7,units = "in" ,dpi = 300)


# #a template raster for data processing
template <- rast(vect(SShelf), res = 1000)
template_s = st_as_stars(template, as_points = FALSE, merge = FALSE)
