#script to provide summary stats on rasters created by calCHI.R

#sum by Threat/ period-------
#pre
pre_CHI = mask(pre_CHI,nbw_ImHab_UTM)

summary_pre<- lapply(1:nlyr(pre_CHI), function(i) {
  # Extract each layer as a SpatRaster
  layer <- pre_CHI[[i]]
  
  # Calculate summary statistics for the layer
  data.frame(
    Layer = names(pre_CHI)[i],
    Mean = global(layer, fun = 'mean', na.rm = TRUE)[1],
    SD = global(layer, fun = 'sd', na.rm = TRUE)[1],
    Min = global(layer, fun = 'min', na.rm = TRUE)[1],
    Max = global(layer, fun = 'max', na.rm = TRUE)[1]
  )
}) %>%
  bind_rows() %>%
  mutate(Layer = gsub("pre_", "", Layer))

summary_pre

#PRE figure out percentage contribution of each threat to total score
      threatSum_pre <- global(pre_CHI, fun = 'sum', na.rm = TRUE)
      CHIsum_pre = sum(threatSum_pre, na.rm = T)
      # Calculate the contribution of the layer
      contributionPre <- (threatSum_pre / CHIsum_pre)*100

#POST
      #post
      post_CHI = mask(post_CHI,nbw_ImHab_UTM)
      
      summary_post<- lapply(1:nlyr(post_CHI), function(i) {
        # Extract each layer as a SpatRaster
        layer <- post_CHI[[i]]
        
        # Calculate summary statistics for the layer
        data.frame(
          Layer = names(post_CHI)[i],
          Mean = global(layer, fun = 'mean', na.rm = TRUE)[1],
          SD = global(layer, fun = 'sd', na.rm = TRUE)[1],
          Min = global(layer, fun = 'min', na.rm = TRUE)[1],
          Max = global(layer, fun = 'max', na.rm = TRUE)[1]
        )
      }) %>%
        bind_rows() %>%
        mutate(Layer = gsub("post_", "", Layer))
      
      summary_post
      
      # Calculate the contribution of the layers
      
      threatSum_post <- global(post_CHI, fun = 'sum', na.rm = TRUE)
      CHIsum_post = sum(threatSum_post, na.rm = T)
      contributionpost <- (threatSum_post / CHIsum_post)*100


#total area calculations------

#Important Habitat area
nbw_ImHab_UTM$area = st_area(nbw_ImHab_UTM)
area_nbwhab_km2= as.numeric(sum(nbw_ImHab_UTM$area)/1000000)

#MPA
areaMPA_km2 = as.numeric(sum(st_area(Gully_UTM%>%filter(ZONE_ID == 5)))/1000000)
Z1Gully_km2 = as.numeric(sum(st_area(Gully_UTM%>%filter(ZONE_ID == 1)))/1000000)
    
#without z1
    areaMPA_km2 - Z1Gully_km2

#Critical Habitat
areaCH_km2 = as.numeric(sum(st_area(NBW_CH_UTM))/1000000)
    #without z1
    areaCH_km2 - Z1Gully_km2


sum_NBW = (areaCH_km2+areaMPA_km2+Z1Gully_km2)

#SUMMARY STATS DIF in CHI -------
          sumDif = sumImpactpost- sumImpactPre
          sumDif = mask(sumDif,nbw_ImHab_UTM)
          Dif_StudyArea=global(sumDif, c("mean","sd","min","max","rms"),na.rm = T)
          Dif_StudyArea = tibble(Dif_StudyArea, median = median(sumDif[], na.rm = T))
         
          
          # for MPA areas 
          MPAS_Dif <- mask(sumDif,Gully_UTM)
          Dif_MPAs=global(MPAS_Dif, c("mean","sd","min","max","rms"),na.rm = T)
          Dif_MPAs = tibble(Dif_MPAs, median = median(MPAS_Dif[], na.rm = T))
          
          #look at only stats for all CH areas (Gully zone 1 only)
          CH_Dif <- mask(sumDif,NBW_CH_UTM%>%filter(DESCRIP != "The Gully"))
          Dif_CH=   global((CH_Dif), c("mean","sd","min","max","rms"),na.rm = T)
          Dif_CH = tibble(Dif_CH, median = median(CH_Dif[], na.rm = T))
          
          
          #look at only stats for zone 1 only
          GullyZ1Dif <- mask(sumDif,GullyZ1)
          Dif_GullyZ1=   global((GullyZ1Dif), c("mean","sd","min","max","rms"),na.rm = T)
          Dif_GullyZ1 = tibble(Dif_GullyZ1, median = median(GullyZ1Dif[], na.rm = T))
          
          #combine into one table
          zone = c("NBW Imp Habitat", "Gully MPA", "Gully Z1", "Critical Habitat")
          DifStats = tibble( zone, rbind(Dif_StudyArea, Dif_MPAs, Dif_GullyZ1, Dif_CH))
          
          # Round all numeric columns to 2 decimal places
          DifStats <- DifStats %>% 
            mutate(across(where(is.numeric), \(x) round(x, 1)))
          
          
          #write table
          write.csv (DifStats, here::here("output/TABLES/DifStats_tbl.csv"))
          

      #TABLES  PRE----
      
        sumDifpre = mask(sumImpactPre,nbw_ImHab_UTM)
      Dif_StudyArea=global(sumDifpre, c("mean","sd","min","max","rms"),na.rm = T)
      Dif_StudyArea = tibble(Dif_StudyArea, median = median(sumDifpre[], na.rm = T))
      
      
      # for MPA areas 
      MPAS_Dif <- mask(sumDifpre,Gully_UTM)
      Dif_MPAs=global(MPAS_Dif, c("mean","sd","min","max","rms"),na.rm = T)
      Dif_MPAs = tibble(Dif_MPAs, median = median(MPAS_Dif[], na.rm = T))
      
      #look at only stats for all CH areas (Gully zone 1 only)
      CH_Dif <- mask(sumDifpre,NBW_CH_UTM%>%filter(DESCRIP != "The Gully"))
      Dif_CH=   global((CH_Dif), c("mean","sd","min","max","rms"),na.rm = T)
      Dif_CH = tibble(Dif_CH, median = median(CH_Dif[], na.rm = T))
      
      
      #look at only stats for zone 1 only
      GullyZ1Dif <- mask(sumDifpre,GullyZ1)
      Dif_GullyZ1=   global((GullyZ1Dif), c("mean","sd","min","max","rms"),na.rm = T)
      Dif_GullyZ1 = tibble(Dif_GullyZ1, median = median(GullyZ1Dif[], na.rm = T))
      
      #combine into one table
      zone = c("NBW Imp Habitat", "Gully MPA", "Gully Z1", "Critical Habitat")
      DifStats_pre = tibble( zone, rbind(Dif_StudyArea, Dif_MPAs, Dif_GullyZ1, Dif_CH))
      
      DifStats_pre <- DifStats_pre %>% 
        mutate(across(where(is.numeric), \(x) round(x, 1)))
      
      
      #write table
      write.csv (DifStats_pre, here::here("output/TABLES/PRE_Stats_tbl.csv"))
      
      
      
      #TABLES  POST-----
      
      sumDifpost = mask(sumImpactpost, nbw_ImHab_UTM)
      Dif_StudyArea=sumImpactpostDif_StudyArea=global(sumDifpost, c("mean","sd","min","max","rms"),na.rm = T)
      Dif_StudyArea = tibble(Dif_StudyArea, median = median(sumDifpost[], na.rm = T))
      
      
      # for MPA areas 
      MPAS_Dif <- mask(sumDifpost,Gully_UTM)
      Dif_MPAs=global(MPAS_Dif, c("mean","sd","min","max","rms"),na.rm = T)
      Dif_MPAs = tibble(Dif_MPAs, median = median(MPAS_Dif[], na.rm = T))
      
      #look at only stats for all CH areas (Gully zone 1 only)
      CH_Dif <- mask(sumDifpost,NBW_CH_UTM%>%filter(DESCRIP != "The Gully"))
      Dif_CH=   global((CH_Dif), c("mean","sd","min","max","rms"),na.rm = T)
      Dif_CH = tibble(Dif_CH, median = median(CH_Dif[], na.rm = T))
      
      #look at only stats for zone 1 only
      GullyZ1Dif <- mask(sumDifpost,GullyZ1)
      Dif_GullyZ1=   global((GullyZ1Dif), c("mean","sd","min","max","rms"),na.rm = T)
      Dif_GullyZ1 = tibble(Dif_GullyZ1, median = median(GullyZ1Dif[], na.rm = T))
      
      #combine into one table
      zone = c("NBW Imp Habitat", "Gully MPA", "Gully Z1", "Critical Habitat")
      DifStats_post = tibble( zone, rbind(Dif_StudyArea, Dif_MPAs, Dif_GullyZ1, Dif_CH))
      
      DifStats_post <- DifStats_post %>% 
        mutate(across(where(is.numeric), \(x) round(x, 1)))
      
      #write table
      write.csv (DifStats_post, here::here("output/TABLES/POST_Stats_tbl.csv"))
      

      

      # T-TEST Difference of means within NBW imp habitat-----
      t.test(as.vector(sumDifpre),as.vector(sumDifpost ))

# AREA of difference between periods-------

#absolute dif in threats
 increase_thrt_km2 = (sum(sumDif[] > 0, na.rm =T )*res(sumDif)[]^2)/ 1000000
 decrease_thrt_km2 = (sum(sumDif[] < -0, na.rm =T )*res(sumDif)[]^2)/1000000

Absolute_dif = tibble(Increase = round(increase_thrt_km2[1]), Decrease = round(abs(decrease_thrt_km2[1])))

#% percent diff of total area
indif_pct = increase_thrt_km2/area_nbwhab_km2
dedif_pct = decrease_thrt_km2/area_nbwhab_km2

#for RMSE >1
summary(RMSE_grid)
increase_thrt_km2 = (sum(RMSE_grid[] >= RMSError, na.rm =T )*res(RMSE_grid)[]^2)/ 1000000
decrease_thrt_km2 = (sum(RMSE_grid[] <= -RMSError, na.rm =T )*res(RMSE_grid)[]^2)/1000000

RMSE1_dif = tibble(Increase = round(increase_thrt_km2[1]), Decrease = round(abs(decrease_thrt_km2[1])))

#% percent diff of RMSE area
indif_pct = increase_thrt_km2/area_nbwhab_km2
dedif_pct = decrease_thrt_km2/area_nbwhab_km2

# #for RMSE >2
# summary(RMSE_grid2)
# increase_thrt_km2 = (sum(RMSE_grid2[] >= 0.1, na.rm =T )*res(RMSE_grid2)[]^2)/ 1000000
# decrease_thrt_km2 = (sum(RMSE_grid2[] <= -0.1, na.rm =T )*res(RMSE_grid2)[]^2)/1000000
# 
# RMSE2_dif = cbind(increase_thrt_km2 = round(increase_thrt_km2[1]), decrease_thrt_km2 = round(abs(decrease_thrt_km2[1])))
# 

#write CHI difference table
Layer = tibble(Difference = c("Overall", "> RMSE1" ))
dif_tble = bind_cols(Layer, bind_rows(Absolute_dif, RMSE1_dif))

# Format numbers with commas
dif_tble <- dif_tble %>%
  mutate(across(where(is.numeric), \(x) scales::comma(x)))

write.csv (dif_tble, here::here("output/TABLES/dif_tbl.csv"))


