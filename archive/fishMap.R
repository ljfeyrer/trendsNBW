# #### A. Regional Fishing Effort prior to 2005------
library(classInt)

fishdir_pre = file.path("output", "GRIDS", "PRE" ,"Fish")
fishdir_post = file.path("output", "GRIDS", "POST" ,"Fish")

# 
# #####  i. Pelagic longline effort------
          #PRE Period (1999-2003)
          #import data
          Pel_long_fish = vect("shapes/Fishing_Effort/PRE/PLL/pelagics_all.shp")
          Pel_long_fish =  project(Pel_long_fish[,6:6], crs(UTM20))
          Pel_long_fish = rasterize(Pel_long_fish, template, field ="lg_pelagic" ,  fun = "max")
          
          ### project and mask to same extent as habitat area
          Pel_long_fish =  terra::project(Pel_long_fish, template, method = 'near')
          Pel_long_fish <- mask(Pel_long_fish, SShelf)
          names(Pel_long_fish) <-"prePelagicFish"
          plot(Pel_long_fish)
          
          # # Calculate decile breaks (excluding NA values)
          quantEffort(Pel_long_fish, writepath = fishdir_pre)
          PLL_deciles_pre =  rast(paste(fishdir_pre, "prePelagicFish_deciles.tif", sep = "/"))
          # plot(PLL_deciles_pre)
          
          
          #transform raster to stars for plotting
          PLL_deciles_pre_df = st_as_stars(PLL_deciles_pre)
          
          m2 = ggplot() +
                              geom_stars(data = PLL_deciles_pre_df, na.rm = T)+
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
                              ) + labs(title = "Pelagic Longline Historical")+
                              guides(fill = guide_colourbar(title = "Effort"),
                                     colour = guide_colourbar(title = "Effort"))

                            m2

# ## POST Period  - Contemporary period data is based on Koen-Alonso percentiles of time spent/ grid cell from 2005-2021.
#
# #import
Pel_fish_POST = rast(here::here("shapes/Fishing_Effort/POST/SS_Combined_Percentiles_Pelagic_2005-2021.tif"))
#
# #write and transform to UTM
Pel_fish_POST <-terra::project(Pel_fish_POST, template, method = 'near')
Pel_fish_POST <- mask(Pel_fish_POST, SShelf)
# plot(Pel_fish_POST)
#
names(Pel_fish_POST) <- "postPelFish"
#
# #effort classes are inversed
# # Calculate quantile breaks (excluding NA values)
quantEffort_inv(Pel_fish_POST, writepath = fishdir_post)
PLL_deciles_post =  rast(paste(fishdir_post, "postPelFish_deciles.tif", sep = "/"))
#
#
plot(PLL_deciles_post)
#
# ##### Plot map m5
#transform raster to stars for plotting
PLL_deciles_post_df = st_as_stars(PLL_deciles_post)

m5 = ggplot() +
  scale_fill_continuous(
    type = "viridis", option = "A",
    direction = -1,
    
    na.value = NA
  ) +
  geom_stars(data= PLL_deciles_post_df,
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

         
# ii. Bottom longline effort-------
                  # Early Period based on Human Use Atlas of the Scotian Shelf (1999-2003)
                   #import data
                  BLL_pre = vect("shapes/Fishing_Effort/PRE/gf_trawl/groundfish_stats.shp")
                  BLL_pre =  project(BLL_pre[,20:20], crs(UTM20))
                  plot(BLL_pre)
                  BLL_pre_rast = rasterize(BLL_pre, template, field ="LongLCatch" ,  fun = "max")
                  
                  ### project and mask to same extent as habitat area
                  BLL_pre_rast =  terra::project(BLL_pre_rast, template, method = 'near')
                  BLL_pre_rast <- mask(BLL_pre_rast, SShelf)
                  names(BLL_pre_rast) <-"preBLL"
                  plot(BLL_pre_rast)
                  
                  # Calculate decile breaks (excluding NA values)
                  quantEffort(BLL_pre_rast, writepath = fishdir_pre)
                  BLL_pre_decile = rast(paste(fishdir_pre, "preBLL_deciles.tif", sep = "/"))
                  # plot(BLL_pre_decile)
      
      ##### Plot map m4--------
      #transform raster to stars for plotting
      BLL_fish_df = st_as_stars(BLL_pre_decile)
      
      m4 = ggplot() +
        scale_fill_continuous(
          type = "viridis", option = "A",
          direction = -1,
          na.value = "transparent"
        ) +
        geom_stars(data= BLL_fish_df,
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
        labs(title = "Bottom Longline Historical") +
                guides(fill = guide_colourbar(title = "Effort"),
               colour = guide_colourbar(title = "Effort")) 
      
      
      
      m4

      ##POST Period (2005-2023)-------
      #read in bottom longline shapes as raster (already in UTM)
      bot_long_fish_POST = rast(here::here("shapes/Fishing_Effort/POST/SS_Combined_Percentiles_Groundfish_Fixed_2005-2021.tif"))
      
      ###mask and mask to same extent as habitat area
      bot_long_fish_POST =  terra::project(bot_long_fish_POST, template, method = 'near')
      bot_long_fish_POST <- mask(bot_long_fish_POST, SShelf)
      names(bot_long_fish_POST) <- "postFixedFish"
      
      #effort classes are inversed
      # Calculate decile breaks (excluding NA values) 
      quantEffort_inv(bot_long_fish_POST, writepath = fishdir_post)
      BLL_decile_post= rast(paste(fishdir_post, "postFixedFish_deciles.tif", sep = "/"))
      # plot(BLL_decile_post)
      
      #transform raster to stars for plotting
      BLL_decile_post_df = st_as_stars(BLL_decile_post)
      
      
      
      ##### Plot Map m3-------
      
      
      title = "Bottom Longline Contemporary"
      m3 =    ggplot() +
        geom_stars(data = BLL_decile_post_df,
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
      

# #### iii. Groundfish Mobile --------
            # Trawl, Seine 
            # Early Period based on Human Use Atlas of the Scotian Shelf (1999-2003)
           ##  PRE PERIOD 1999-2003 -------
     
            
            #import data
            mobile_gf_pre = vect("shapes/Fishing_Effort/PRE/gf_trawl/groundfish_stats.shp")
            mobile_gf_pre =  project(mobile_gf_pre[,21:21], crs(UTM20))
            mobile_pre_rast = rasterize(mobile_gf_pre, template, field ="TrawlCatch" ,  fun = "max")

            ### project and mask to same extent as habitat area
            mobile_gf_pre =  terra::project(mobile_pre_rast, template, method = 'near')
            mobile_gf_pre <- mask(mobile_gf_pre, SShelf)
            names(mobile_gf_pre) <-"preMobileGF"
plot(mobile_gf_pre)
            
# Calculate decile breaks (excluding NA values)
            quantEffort(mobile_gf_pre, writepath = fishdir_pre)
            preMobileGF_decile = rast(paste(fishdir_pre, "preMobileGF_deciles.tif", sep = "/"))
            # plot(preMobileGF_decile)
            

            ##### Plot map m6--------
            #transform raster to stars for plotting
            preMobileGF_decile_df = st_as_stars(preMobileGF_decile)
            
            m6 = ggplot() +
              scale_fill_continuous(
                type = "viridis", option = "A",
                direction = -1,
                                na.value = NA
              ) +
              geom_stars(data= preMobileGF_decile_df,
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
  
            ## POST GF mobile-------
            # - Contemporary period data is based on Koen-Alonso percentiles of time spent/ grid cell from 2005-2021.
            #import
            GFmobile_POST = rast(here::here("shapes/Fishing_Effort/POST/SS_Combined_Percentiles_Groundfish_Mobile_2005-2021.tif"))
            
            #write and transform to UTM
            GFmobile_POST <-terra::project(GFmobile_POST, template, method = 'near')
            GFmobile_POST <- mask(GFmobile_POST, SShelf)
            names(GFmobile_POST) <- "postGFmobileFish"
            
            #effort classes are inversed
            # Calculate decile breaks (excluding NA values) 
            quantEffort_inv(GFmobile_POST, writepath = fishdir_post)
            GFmobile_POST_decile = rast(paste(fishdir_post, "postGFmobileFish_deciles.tif", sep = "//"))
            # plot(GFmobile_POST_decile)
            
          
            ##### Plot map m7-----
            #transform raster to stars for plotting
            GFmobile_POST_df = st_as_stars(GFmobile_POST_decile)
            
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
            
            postFish = postFish/3
            # plot(postFish)
            
            # #MASK Gully Z1 from post 2004 fish effort----
            # postFish <- mask(postFish,GullyZ1, inverse = T, updatevalue = 0)
            names(postFish) ="post_fish"
            # plot(postFish)
            
            # write combined layer for calculating CHI----
            path = here("output/GRIDS/CHI//")
            writeRaster(postFish, filename = paste(path, "post_fish.tif", sep = ""), filetype ='GTiff', overwrite = T)
            

            
            
            #      OLD PRE BLL-----
            # - Early period data is based on bottom long line fishing effort dataset by Butler et al. 2019, where fishing effort is sum of effort in hrs/ km2 from 2002-2017. 
            #       # - Contemporary period data is based on Koen-Alonso percentiles of time spent/ grid cell from 2005-2021.
            # 
            #           ##PRE Period (1990-2004)
            #           
            #                 #read in bottom longline shapes as raster (already in UTM)
            #                 bot_long_fish = rast(here::here("shapes/Fishing_Effort/PRE/bottomll"))
            #                 
            #                 ###mask and mask to same extent as habitat area
            #                 bot_long_fish =  terra::project(bot_long_fish, template, method = 'near')
            #                 bot_long_fish <- mask(bot_long_fish, SShelf)
            #                 
            #                 names(bot_long_fish) <- "preFixedFish"
            #                 
            #                 plot(bot_long_fish)
            #                 
            #                 # Calculate decile breaks (excluding NA values) 
            #                 fishdir_pre = file.path("output", "GRIDS", "PRE" ,"Fish")
            #                 quantEffort(bot_long_fish, writepath = fishdir_pre)
            #                 BLL_decile_pre = rast(paste(fishdir_pre, "preFixedFish_deciles.tif", sep = "/"))
            # 
            #                 #transform raster to stars for plotting
            #                 BLL_decile_pre_df = st_as_stars(BLL_decile_pre)
            #                 
            #           #### Plot Map m2
            #                 title = "Bottom Longline Historical"
            #                 
            #                
            #               
            
      