# #### A. Regional Fishing Effort prior to 2005------
library(classInt)

# - Long line fishery 
# - Groundfish
#
# 
# > S. Butler, D. Ibarra and S. Coffen-Smout, 2023. Maritimes Region Longline and Trap Fisheries Footprint Mapping for Marine Spatial Planning and Risk Assessment. Can. Tech. Rep. Fish. Aquat. Sci. 3293: v + 30 p. shapes available at: https://open.canada.ca/shapes/en/shapesset/3d2e1a84-20f5-4a61-90e4-94760c80ebb9  
#   

# #####  i. Groundfish - Fixed Gear (bottom longline) effort------
      # - Early period data is based on bottom long line fishing effort dataset by Butler et al. 2019, where fishing effort is sum of effort in hrs/ km2 from 2002-2017. 
      # - Contemporary period data is based on Koen-Alonso percentiles of time spent/ grid cell from 2005-2021.

          ##PRE Period (1990-2004)-----
          
                #read in bottom longline shapes as raster (already in UTM)
                bot_long_fish = rast(here::here("shapes/Fishing_Effort/PRE/bottomll"))
                
                ###crop and mask to same extent as habitat area
                bot_long_fish =  terra::project(bot_long_fish, template, method = 'near')
                bot_long_fish <- crop(bot_long_fish, SShelf)
                
                names(bot_long_fish) <- "preFixedFish"
                
                # plot(bot_long_fish)
                
                # Calculate quantile breaks (excluding NA values) 
                fishdir = file.path("output", "GRIDS", "PRE" ,"Fish")
                quantEffort(bot_long_fish, writepath = fishdir)
                bottomll_Quant = rast(paste(fishdir, "preFixedFish_Quant.tif", sep = "//"))
                # plot(bottomll_Quant)
          
            
                #transform raster to stars for plotting
                bot_long_fish_df = st_as_stars(bottomll_Quant)
                
          #### Plot Map m2------
                title = "Bottom Longline Historical"
                
                m2 = ggplot() +
                  geom_stars(data = bot_long_fish_df, na.rm = T)+
                  scale_fill_continuous(
                    type = "viridis", option = "A",
                    direction = 1,
                    na.value = "transparent"
                  ) +
                  geom_sf(
                    data = nbw_ImHab_UTM,
                    col = "black",
                    fill = NA,
                    size = .2
                  ) +
                  geom_sf(data = bathy, col = "gray", size = 0.2) +
                  # add land region
                  geom_sf(data = landUTM, color = NA, fill = "grey50") +
                  labs(title = title) +
                  # add scale bar
                  annotation_scale(
                    location = "br",
                    width_hint = 0.25,
                    text_cex = 0.6,
                    bar_cols = c("grey40", "white")
                  ) +
                  # set map limits
                  coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE) +
                  
                  # format axes
                  ylab("") +
                  xlab("") + theme_bw() +
                  theme(
                    plot.margin = margin(.5, .5, .5, .5, "cm"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.position = "right"
                  ) +
                  guides(fill = guide_colourbar(title = "Effort"),
                         colour = guide_colourbar(title = "Effort")) 
                
                m2
          
          ##POST Period (2005-2023)-------
                #read in bottom longline shapes as raster (already in UTM)
                bot_long_fish_POST = rast(here::here("shapes/Fishing_Effort/POST/SS_Combined_Percentiles_Groundfish_Fixed_2005-2021.tif"))
                
                ###crop and mask to same extent as habitat area
                bot_long_fish_POST =  terra::project(bot_long_fish_POST, template, method = 'near')
                bot_long_fish_POST <- crop(bot_long_fish_POST, SShelf)
                names(bot_long_fish_POST) <- "postFixedFish"
                
#effort classes are inversed
                # Calculate quantile breaks (excluding NA values) 
                fishdir = file.path("output", "GRIDS", "POST" ,"Fish")
                quantEffort_inv(bot_long_fish_POST, writepath = fishdir)
                bottomll_Quant = rast(paste(fishdir, "postFixedFish_Quant.tif", sep = "//"))
                # plot(bottomll_Quant)
                
                  ##### Plot Map m3-------
                
                #transform raster to stars for plotting
                bot_long_fish_df = st_as_stars(bottomll_Quant)
                
                
                title = "Bottom Longline Contemporary"
                  m3 =    ggplot() +
                    geom_stars(data = bot_long_fish_df,
                               na.rm = T) +
                    scale_fill_continuous(
                      type = "viridis", option = "A",
                      direction = -1,
                     na.value = "transparent"
                    ) +
                    geom_sf(
                      data = nbw_ImHab_UTM,
                      col = "black",
                      fill = NA,
                      size = .2
                    ) +
                    geom_sf(data = bathy, col = "gray", size = 0.2) +
                    # add land region
                    geom_sf(data = landUTM, color = NA, fill = "grey50") +
                    theme(legend.position = "none") +
                    labs(title = title) +
                    # add scale bar
                    annotation_scale(
                      location = "br",
                      width_hint = 0.25,
                      text_cex = 0.6,
                      bar_cols = c("grey40", "white")
                    ) +
                    # set map limits
                    coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE) +
                    
                    # format axes
                    ylab("") +
                    xlab("") +
                    theme_bw() + theme(
                      plot.margin = margin(.5, .5, .5, .5, "cm"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      legend.position = "right"
                    ) +
                    guides(fill = guide_colourbar(title = "Effort"),
                           colour = guide_colourbar(title = "Effort")) 
                  
                  m3

                  
# ii. Pelagic (longline) effort-------
      # - Early period data is based on pelagic longline fishing effort dataset by Butler et al. 2019, 
        # where fishing effort is sum of effort in  mins/ km2 from 2002-2017. Data is in log scale
      # - Contemporary period data is based on Koen-Alonso percentiles of time spent/ grid cell from 2005-2021.
      
        ###PRE Period (1988-2004)------
        
         #import
         Pel_long_fish = rast(here::here("shapes/Fishing_Effort/PRE/PLL/PLL_mins.tif"))
          
          #write and transform to UTM
          Pel_long_fish <-terra::project(Pel_long_fish, template, method = 'near')
          
          #deal with log scale and crop to SS
           Pel_long_fish2 = 10^log(Pel_long_fish)
           Pel_long_fish2 <- crop(Pel_long_fish2, SShelf)
           names(Pel_long_fish2) <-"prePelagicFish"
      # plot(Pel_long_fish2)
      # Calculate quantile breaks (excluding NA values)
      quantEffort(Pel_long_fish2, writepath = "shapes/Fishing_Effort/PRE/")
           Pel_quant_rast = rast( "shapes/Fishing_Effort/PRE/prePelagicFish_Quant.tif")
           # plot(Pel_quant_rast)
      
      
      
      ##### Plot map m4--------
      #transform raster to stars for plotting
      Pel_long_fish_df = st_as_stars(Pel_quant_rast)
      
      m4 = ggplot() +
        scale_fill_continuous(
          type = "viridis", option = "A",
          direction = -1,
          na.value = "transparent"
        ) +
        geom_stars(data= Pel_long_fish_df,
                   na.rm = T,
                   downsample = 3) +
        geom_sf(
          data= nbw_ImHab_UTM,
          col = "black",
          fill = NA,
          size = .2
        ) +
        geom_sf(data= bathy, col = "gray", size = 0.2) +
        # add land region
        geom_sf(  data= landUTM, color=NA, fill="grey50") +
        # add scale bar
        annotation_scale(location = "br", width_hint=0.25,
                         text_cex = 0.6,
                         bar_cols = c("grey40", "white")) +
        # set map limits
        coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE) +
        # format axes
        ylab("") + 
        xlab("") +
        theme_bw()+
        theme(plot.margin = margin(.5, .5, .5, .5, "cm"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position="right")+
        labs(title = "Pelagic Longline Historical") +
                guides(fill = guide_colourbar(title = "Effort"),
               colour = guide_colourbar(title = "Effort")) 
      
      
      
      m4

## POST Period (2005-2023)-------

      #import
      Pel_fish_POST = rast(here::here("shapes/Fishing_Effort/POST/SS_Combined_Percentiles_Pelagic_2005-2021.tif"))
      
      #write and transform to UTM
      Pel_fish_POST <-terra::project(Pel_fish_POST, template, method = 'near')
      Pel_fish_POST <- crop(Pel_fish_POST, SShelf)
      # plot(Pel_fish_POST)
      
      names(Pel_fish_POST) <- "postPelFish"
      
      #effort classes are inversed
      # Calculate quantile breaks (excluding NA values) 
      fishdir = file.path("output", "GRIDS", "POST" ,"Fish")
      quantEffort_inv(Pel_fish_POST, writepath = fishdir)
      Pel_fish_POST_Quant = rast(paste(fishdir, "postPelFish_Quant.tif", sep = "//"))
      # plot(Pel_fish_POST_Quant)
      
            #### write raster----
            fishdir = file.path("output", "GRIDS", "POST" ,"Fish")
      
      
      
            writeRaster(
              Pel_fish_POST,
              filename = paste0(fishdir, "/postPel_fish.tif"),
              overwrite = TRUE
             
            )
            
            ##### Plot map m5-----
            #transform raster to stars for plotting
            Pel_long_fish_df = st_as_stars(Pel_fish_POST_Quant)
            
            m5 = ggplot() +
              scale_fill_continuous(
                type = "viridis", option = "A",
                direction = -1,
               
                na.value = NA
              ) +
              geom_stars(data= Pel_long_fish_df,
                         na.rm = T,
                         downsample = 3) +
              geom_sf(
                data= nbw_ImHab_UTM,
                col = "black",
                fill = NA,
                size = .2
              ) +
              geom_sf(data= bathy, col = "gray", size = 0.2) +
              geom_sf(data= landUTM, color = NA, fill = "grey50") +
              # add scale bar
              annotation_scale(
                location = "br",
                width_hint = 0.25,
                text_cex = 0.6,
                bar_cols = c("grey40", "white")
              ) +
              # set map limits
              coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE) +
              
              # format axes
              ylab("") +
              xlab("") +
              theme(
                plot.margin = margin(.5, .5, .5, .5, "cm"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.position = "right"
              ) +
              theme_bw() +
              guides(fill = guide_colourbar(title = "Effort"))+
              labs(title = "Pelagic Longline Contemporary") 
            
            m5


# #### iii. Groundfish Mobile --------
            # Trawl, Seine 
            # Early Period based on Human Use Atlas of the Scotian Shelf (1999-2003)
            # Contemporary period data is based on Koen-Alonso percentiles of time spent/ grid cell from 2005-2021.


##  PRE PERIOD 1999-2003 -------
     
            
            #import data
            mobile_gf_pre = vect("shapes/Fishing_Effort/PRE/SS_HUA/groundfish_stats.shp")
            mobile_gf_pre =  project(mobile_gf_pre[,21:21], crs(UTM20))
            mobile_pre_rast = rasterize(mobile_gf_pre, template, field ="TrawlCatch" ,  fun = "max")

            ### project and crop to same extent as habitat area
            mobile_gf_pre =  terra::project(mobile_pre_rast, template, method = 'near')
            mobile_gf_pre <- crop(mobile_gf_pre, SShelf)
            names(mobile_gf_pre) <-"preMobileGF"

            # Calculate quantile breaks (excluding NA values)
            fishdir = file.path("output", "GRIDS", "PRE" ,"Fish")
            quantEffort(mobile_gf_pre, writepath = fishdir)
            TrawlCatch_Quant = rast(paste(fishdir, "preMobileGF_Quant.tif", sep = "/"))
            # plot(TrawlCatch_Quant)
            
    ####write raster---------
            fishdir = file.path("output", "GRIDS", "PRE" ,"Fish") #output directory
            writeRaster(
              TrawlCatch_Quant,
              filename = paste0( fishdir, "/prePel_long_fish.tif"),
              overwrite = TRUE
            )
            
            ##### Plot map m6--------
            #transform raster to stars for plotting
            TrawlCatch_Quant_df = st_as_stars(TrawlCatch_Quant)
            
            m6 = ggplot() +
              scale_fill_continuous(
                type = "viridis", option = "A",
                direction = -1,
                                na.value = NA
              ) +
              geom_stars(data= TrawlCatch_Quant_df,
                         na.rm = T,
                         downsample = 3) +
              geom_sf(
                data= nbw_ImHab_UTM,
                col = "black",
                fill = NA,
                size = .2
              ) +
              geom_sf(data= bathy, col = "gray", size = 0.2) +
              # add land region
              geom_sf(  data= landUTM, color=NA, fill="grey50") +
              # add scale bar
              annotation_scale(location = "br", width_hint=0.25,
                               text_cex = 0.6,
                               bar_cols = c("grey40", "white")) +
              # set map limits
              coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE) +
              # format axes
              ylab("") + 
              xlab("") +
              theme_bw()+
              theme(plot.margin = margin(.5, .5, .5, .5, "cm"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.position="right")+
              guides(fill = guide_colourbar(title = "Effort"))+
              labs(title = "Groundfish Mobile Historical") 
            
            m6
  
            ## POST Period (2005-2023) GF mobile-------
            
            #import
            GFmobile_POST = rast(here::here("shapes/Fishing_Effort/POST/SS_Combined_Percentiles_Groundfish_Mobile_2005-2021.tif"))
            
            #write and transform to UTM
            GFmobile_POST <-terra::project(GFmobile_POST, template, method = 'near')
            GFmobile_POST <- crop(GFmobile_POST, SShelf)
            names(GFmobile_POST) <- "postGFmobileFish"
            
            #effort classes are inversed
            # Calculate quantile breaks (excluding NA values) 
            fishdir = file.path("output", "GRIDS", "POST" ,"Fish")
            quantEffort_inv(GFmobile_POST, writepath = fishdir)
            GFmobile_POST_Quant = rast(paste(fishdir, "postGFmobileFish_Quant.tif", sep = "//"))
            # plot(GFmobile_POST_Quant)
            
          
            ##### Plot map m7-----
            #transform raster to stars for plotting
            GFmobile_POST_df = st_as_stars(GFmobile_POST_Quant)
            
            m7 = ggplot() +
              scale_fill_continuous(
                type = "viridis", option = "A",
                direction = -1,
                
                na.value = NA
              ) +
              geom_stars(data= GFmobile_POST_df,
                         na.rm = T,
                         downsample = 3) +
              geom_sf(
                data= nbw_ImHab_UTM,
                col = "black",
                fill = NA,
                size = .2
              ) +
              geom_sf(data= bathy, col = "gray", size = 0.2) +
              geom_sf(data= landUTM, color = NA, fill = "grey50") +
              # add scale bar
              annotation_scale(
                location = "br",
                width_hint = 0.25,
                text_cex = 0.6,
                bar_cols = c("grey40", "white")
              ) +
              # set map limits
              coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE) +
              theme_bw() +
              # format axes
              ylab("") +
              xlab("") +
              theme(
                plot.margin = margin(.5, .5, .5, .5, "cm"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.position = "right"
              ) +
              
              guides(fill = guide_colourbar(title = "Effort"))+
              labs(title = "Groundfish Mobile Contemporary") 
            
            m7          
            
            
#iv.  COMBINE PRE fishing effort layers----
            path = here("output/GRIDS/PRE/Fish/")
            
            filesfish <- list.files(path=path, full.names  = T)
            filesfish = rast(filesfish)

            #sum all types and rescale to 100%
            preFish =sum(filesfish, na.rm = T) 
            names(preFish) ="pre_fish"
            preFish = preFish/3
            # plot(preFish)
            
            # write combined layer for calculating CHI----
            path = here("output/GRIDS/CHI//")
            writeRaster(preFish, filename = paste(path, names(preFish), ".tif", sep = ""), filetype ='GTiff', overwrite = T)
            
            #combine POST fishing effort layers----
            path = here("output/GRIDS/POST/Fish/")
            filesfish <- list.files(path=path, full.names  = T)
            filesfish = rast(filesfish)
            
            #sum all types and rescale to 100%
            postFish =sum(filesfish, na.rm = T) 
            names(postFish) ="post_fish"
            postFish = postFish/3
            # plot(postFish)
            
            # #MASK Gully Z1 from post 2004 fish effort----
            # postFish <- mask(postFish,GullyZ1, inverse = T, updatevalue =0)
            
            # plot(postFish)
            
            # write combined layer for calculating CHI----
            path = here("output/GRIDS/CHI//")
            writeRaster(postFish, filename = paste(path, names(postFish), ".tif", sep = ""), filetype ='GTiff', overwrite = T)
            
