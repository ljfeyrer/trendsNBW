#### D. Shipping


#  - HISTORICAL shipping 2000 based on Scotian Shelf Human Use Atlas 
# (https://www.dfo-mpo.gc.ca/oceans/publications/scotian-atlas-ecossais/index-eng.html)
# commercial shipping density for a representative year of inbound vessel
# traffic to the region. The primary source of commercial vessel data for
# Canadian waters is the Canadian Coast Guard’s Eastern Canada Vessel Traffic
# Services Zone (ECAREG) system. This is a mandatory reporting system for all
# commercial vessels over 500 gross registered tons (GRT) transiting within
# Canada’s 12-nautical-mile territorial sea. Vessel trip records include
# information on vessel size, class, cargo and departure/destination points. The
# ECAREG system also provides geo-referenced information (latitude/ longitude)
# for chronological movement reports made during individual vessel trips. This
# map does not show all international shipping through the region for the year
# 2000 for several reasons. The map includes only inbound traffic; however,
# other analyses have shown that the density pattern of departing vessels is
# much the same as the inbound traffic pattern. As well, the ECAREG system does
# not include information on vessels transiting through Canada’s
# 200-nautical-mile exclusive economic zone (EEZ) if they are not departing or
# entering the territorial sea or internal waters.

# - CONTEMPORARY From OPEN GOV 2019. Intensity raster of global shipping traffic. 
#  Shipping is not excluded by regulation from Gully MPA

# Vessel Density Mapping of 2019 AIS Data in the Northwest Atlantic.


##  Import early shipping raster -------
      #LRIT layer
      ship_pre = rast("inputs/shapes/Shipping/shipping2010.tif")

# layers taken from SSHUA

      # ship_pre = rast("inputs/shapes/Shipping/traffic2000")
      ship_pre <- terra::project(ship_pre, template, res = 1000, method = 'near')
      ship_pre <- crop(ship_pre, regions$SShelf)
      names(ship_pre) <-"pre_Ship"
      # plot(ship_pre)
      
      #make relative intensity scale
      #deciles
      quantEffort(ship_pre, writepath = "output/GRIDS/PRE/Shipping/")
      pre = "output/GRIDS/PRE/Shipping/pre_Ship_deciles.tif"
      ship_pre = rast(pre)
      # plot(ship_pre)

##  Import contemporary shipping raster -------
      
      # #HALPERN GLOBAL 2011
      # halp_ship = rast("inputs/shapes/Shipping/shipping.tif")
      # names(halp_ship) <-"postShip_halp"
      # 
      # #reproject, crop and save
      # writepath = here::here("output/GRIDS/POST/Shipping/")
      # halpernRaster(halp_ship, dsn, regions$SShelf, writepath)
      # 
      # #make relative intensity scale
      # quantEffort(rast("output/GRIDS/POST/Shipping/postShip_halp.tif"), writepath = "output/GRIDS/POST/Shipping/")
      # ship_post = rast("output/GRIDS/POST/Shipping/postShip_halpQuant.tif")      
      # halp_ship =    crop(ship_post, regions$SShelf)
      # # plot(halp_ship)

    #OPEN GOV 2019
      
      ship_post19 = rast("inputs/shapes/Shipping/All_VesselsPerDay_2019_AIS.tif")
      ship_post <- terra::project(ship_post19, template, res = 1000, method = 'near')
      ship_post =    crop(ship_post, regions$SShelf)
      names(ship_post) <-"post_Ship"
      # plot(ship_post)
      
      #make relative intensity scale
      #decile rank differences
      quantEffort(ship_post, writepath = "output/GRIDS/POST/Shipping/")
      post = "output/GRIDS/POST/Shipping/post_Ship_deciles.tif"
      ship_post = rast(post)
      # plot(ship_post)

      # #OPEN GOV 2021
      # 
      # ship_post21 = rast("inputs/shapes/Shipping/All_VesselsPerDay_2021_AIS.tif")
      # ship_post <- terra::project(ship_post21, template, res = 1000, method = 'near')
      # ship_post =    crop(ship_post, regions$SShelf)
      # names(ship_post) <-"post_Ship"
      # # plot(ship_post)
      # 
      # #make relative intensity scale
      # 
      # quantEffort(ship_post, writepath = "output/GRIDS/POST/Shipping/")
      # post = "output/GRIDS/POST/Shipping/post_ShipQuant.tif"
      # ship_post = rast(post)
      # # plot(ship_post)
      
      
      #copy raster files to CHI-----
      
      #copy to CHI folder-------
      #pre
      CHI_path = "output/GRIDS/CHI//"
      file.copy(from=pre, to=CHI_path, 
                overwrite = TRUE, recursive = FALSE, 
                copy.mode = TRUE)
      #post
      file.copy(from=post, to=CHI_path, 
                overwrite = TRUE, recursive = FALSE, 
                copy.mode = TRUE)
#map PRE MPA--------
      ship_pre_df = st_as_stars(ship_pre)

# Plot
m14 = ggplot() +
  geom_stars(data = ship_pre_df,
             na.rm = T,
             downsample = 3) +
  scale_fill_viridis_c(direction = -1, na.value=NA, option = "D", limits = c(0, 100)) +

  geom_sf(
    data = nbw_data$important_habitat,
    col = "black",
    fill = NA,
    size = .2
  ) +
  geom_sf(data = bathy, col = "gray", size = 0.2) +
  # add land region
  geom_sf(data = land, color = NA, fill = "grey50") +
  theme(legend.position = "none") +
  labs(title = "Shipping Traffic - Historical") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.6,
    bar_cols = c("grey40", "white")
  ) +
        # set map limits
        coord_sf(xlim = nbw_data$xlim, ylim = nbw_data$ylim, expand = FALSE) +
        
        # format axes
        ylab("") +
        xlab("") + theme_bw() +
        theme(
          plot.margin = margin(.5, .5, .5, .5, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "right"
        )+
   guides(fill = guide_colourbar(title = "Intensity"),
         colour = guide_colourbar(title = "Intensity")) + theme_bw() 

      

  #map POST MPA---------
      ship_post_df = st_as_stars(ship_post)
      
      # Plot 
      m15 = ggplot() +
        geom_stars(data = ship_post_df,
                   na.rm = T,
                   downsample = 3) +
        scale_fill_viridis_c(direction = -1, na.value=NA, option = "D", limits = c(0, 100)) +
        geom_sf(
          data = nbw_data$important_habitat,
          col = "black",
          fill = NA,
          size = .2
        ) +
        geom_sf(data = bathy, col = "gray", size = 0.2) +
        # add land region
        geom_sf(data = land, color = NA, fill = "grey50") +
        theme(legend.position = "none") +
        labs(title = "Shipping Traffic - Contemporary") +
        # add scale bar
        annotation_scale(
          location = "br",
          width_hint = 0.25,
          text_cex = 0.6,
          bar_cols = c("grey40", "white")
        ) +
        # set map limits
        coord_sf(xlim = nbw_data$xlim, ylim = nbw_data$ylim, expand = FALSE) +
        
        # format axes
        ylab("") +
        xlab("") + theme_bw() +
        theme(
          plot.margin = margin(.5, .5, .5, .5, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "right"
        )+
        guides(fill = guide_colourbar(title = "Intensity"),
               colour = guide_colourbar(title = "Intensity")) + theme_bw() 
      
      
      