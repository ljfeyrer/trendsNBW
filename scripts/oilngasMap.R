# #### C. Oil & Gas Exploration and Operations
#
# - Publicly available spatial and temporal data was collated from the
# Canada-Nova Scotia Offshore Petroleum Board (C-NSOPB) and the
# Canada-Newfoundland and Labrador Offshore Petroleum Board (C-NLOPB) websites
# (CNLOPB, 2021; CNSOPB, 2021). Shapefiles for existing pipelines, wells,
# platforms, significant discovery licenses, and production licenses were downloaded, cleaned, and checked for
# duplication. 
#- Activities occurring before and after 2004 were identified
# based on metadata in the shapefile or provided elsewhere on the website. 
#
# ##### ii. Operational activities ------- 
# - Oil & Gas Operations includes wells, pipelines, and platforms which are
# treated as presence absence data as per Halpern et al. (2008) 
# - Some infrastructure has been decommissioned since it was built, however the ongoing
# risk associated with pollution is assumed to continue over the entire study
# period.

###### a. Drilled wells------
            # - Wells sorted by spud (drill) date, Operations, Considered footprint/PA data 
            
            #read in well data, make UTM
            wells = read_sf(here::here("shapes/OnG/Operations/all_wells.shp"))
            wellsUTM = wells%>%st_transform(UTM20)
            
            #  sort by period
            preWells = wellsUTM%>%filter(YearEnd <=2004)
            postWells = wellsUTM
            
            #plot all by year
            ggplot() +
              
              geom_sf(data= bathy, col = "gray", size = 0.2) +
              geom_sf(data= landUTM, color = NA, fill = "grey50") +
              geom_sf(data = wells, aes(col = YearEnd))+
              theme_bw()+
              labs(title ="All Wells") +
              geom_sf(
                data = nbw_ImHab2023_UTM,
                col = "black",
                fill = NA,
                size = .2
              )+
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
                
              ) +
              guides(fill = guide_colourbar(title = "Intensity"))+
              theme(legend.position = "none") 

    #rasterize Pre- 2004 
    dsn = here::here("output/GRIDS/PRE/Spills/OnG/")
    preWells = pointRaster(preWells, template, dsn)
   
    
    #Rasterize POST 2004 - 
    dsn2 = here::here("output/GRIDS/POST/Spills/OnG/")
    postWells = pointRaster(postWells, template, dsn2)
    

#### b. Platforms--------
      # - Platforms Operations, Considered footprint/PA data 
      
      #read in data, make UTM
      platforms = read_sf(here::here("shapes/OnG/Operations/platformsYR.shp"))
      platformsUTM = platforms%>%st_transform(UTM20)
      
      #  sort by period
      prePlatforms = platformsUTM%>%filter(YEAR <=2004)
      postPlatforms = platformsUTM
      
      #plot all by year
      ggplot()+ geom_sf(data = bathy, col = "grey",  size = 0.2)+
        geom_sf(data = nbw_ImHab2023_UTM, col = "black",size = .2, fill = NA)+
        geom_sf(data = platforms, aes(col = YEAR))+theme_bw()+
        labs(title ="All Platforms")+
        # set map limits
        coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE)

#Rasterize Pre- 
prePlatforms = pointRaster(prePlatforms, template, dsn)

#Rasterize POST 2004 - 
postPlatforms = pointRaster(postPlatforms, template, dsn2)

#### c. Production licenses-------
      # - Production licenses are similar impact to platforms, but polygons, Operations, Considered footprint/PA data 
      
      #read in data, make UTM
      ProdLic = read_sf(here::here("shapes/OnG/Operations/Production_LicenceYR.shp"))
      ProdLicUTM = ProdLic%>%st_transform(UTM20)
      
      #plot all by year
      ggplot()+ geom_sf(data = bathy, col = "grey",  size = 0.2)+
        geom_sf(data = nbw_ImHab2023_UTM, col = "black",size = .2, fill = NA)+
        geom_sf(data = ProdLic, aes(col = YEAR))+theme_bw()+
        labs(title ="All Production Licenses")+
        # set map limits
        coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE)
      
      #  sort by period (operational date)
      preProdLic = ProdLicUTM%>%filter(YEAR <=2004)
      postProdLic = ProdLicUTM

##Rasterize Pre- 2004 use function polyRaster
preProdLic = polyRaster(preProdLic, template, dsn)

#Rasterize POST 2004 - use function polyRaster
postProdLic = polyRaster(postProdLic, template, dsn2)

#### d. Significant discovery licenses  --------
          # - Significant discovery licenses require drilling and surveys, operations, Considered footprint/PA data
          #read in data, make UTM
          
          SigDisLic =read_sf(here::here("shapes/OnG/Operations/SigDis_LicenceYR.shp"))
          SigDisLicUTM = SigDisLic%>%st_transform(UTM20)
          
          #plot all by year
          ggplot()+ geom_sf(data = bathy, col = "grey",  size = 0.2)+
            geom_sf(data = SigDisLic, aes(col = YEAR))+theme_bw()+
            labs(title ="Significant Discovery Licenses")+
            geom_sf(data = nbw_ImHab2023_UTM, col = "black",size = .2, fill = NA)+
            
            # set map limits
            coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE)
          
          
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
      pipes =read_sf(here::here("shapes/OnG/Operations/all_pipes.shp"))
      pipesUTM = pipes%>%st_transform(UTM20)
      
      #change name so consistent
      prePipes= pipesUTM
      postPipes= pipesUTM
      
      #plot all by year
      ggplot()+ geom_sf(data = bathy, col = "grey",  size = 0.2)+
         geom_sf(data = pipes, col = "blue")+theme_bw()+
        labs(title ="Pipelines")+
        geom_sf(data = nbw_ImHab2023_UTM, col = "black",size = .2, fill = NA)+
        # set map limits
        coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE)

##Rasterize Pre- 2004 use function pointRaster
pointRaster(prePipes, template, dsn)

#Rasterize POST 2004 - use function pointRaster
pointRaster(postPipes, template, dsn2) 

#Import and combine all footprints of operational activities -------

          #Early Period
          #combine all operational rasters for period pre 2004  
          pre = here("output/GRIDS/PRE/Spills/pre_oil.tif")
          dsn = dsn
          footprntRaster(dsn, pre)
          pre_oil = rast(pre)
          names(pre_oil) = "pre_oil"
          
          #Contemporary Period
          #combine all operational rasters for contemporary period  
          post = here("output/GRIDS/POST/Spills/post_oil.tif")
          dsn = dsn2
          footprntRaster(dsn, post)
          post_oil = rast(post)
          names(post_oil) = "post_oil"
          
    #write raster files to CHI-----
          CHI_path = "output/GRIDS/CHI//"
          
          #pre
          writeRaster(pre_oil, filename = paste(CHI_path, "pre_oil.tif", sep = ""), overwrite = TRUE)
                   
          #post
          writeRaster(post_oil, filename = paste(CHI_path, "post_oil.tif", sep = ""), overwrite = TRUE)
          

#plot pre 2004 operational effort----------
#make stars object
Op_pre_df = read_stars(here("output/GRIDS/PRE/Spills/pre_Oil.tif"))

m10 = ggplot() +  
  scale_fill_continuous(
    type = "viridis", option = "A",
    direction = 1,
    na.value = "transparent"
  ) +
  geom_sf(data = nbw_ImHab2023_UTM, col = "black",fill = NA, size = .2)+
  geom_stars(data = Op_pre_df, na.rm = T, downsample = 0)+
  geom_sf(data = bathy, col = "gray", size = 0.2) +
  # add land region
  geom_sf(  data = landUTM, color=NA, fill="grey50") +
  # add scale bar
  annotation_scale(location = "br", width_hint=0.25,
                   text_cex = 0.6,
                   bar_cols = c("grey40", "white")) +
  # set map limits
  coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE)+
  # format axes
  ylab("") +
  xlab("") +
  theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + labs(title = "Oil & Gas - Historical") + 

  guides(fill = guide_colourbar(title = "Intensity"))+
  theme_bw() 
m10



#plot post 2004 operational effort----------
#make stars object
Op_post_df = read_stars(here("output/GRIDS/POST/Spills/post_oil.tif"))

m11 = ggplot() +  
  scale_fill_continuous(
    type = "viridis", option = "A",
    direction = 1,
    na.value = "transparent"
  ) +
  geom_sf(data = nbw_ImHab2023_UTM, col = "black",fill = NA, size = .2)+
  geom_stars(data = Op_post_df, na.rm = T, downsample = 0)+
  geom_sf(data = bathy, col = "gray", size = 0.2) +
  # add land region
  geom_sf(  data = landUTM, color=NA, fill="grey50") +
  # add scale bar
  annotation_scale(location = "br", width_hint=0.25,
                   text_cex = 0.6,
                   bar_cols = c("grey40", "white")) +
  # set map limits
  coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE)+
  # format axes
  ylab("") +
  xlab("") +
  theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + labs(title = "Oil & Gas - Contemporary") +

  guides(fill = guide_colourbar(title = "Intensity"))+
  theme_bw() 
m11

