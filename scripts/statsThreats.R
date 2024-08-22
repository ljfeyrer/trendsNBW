#script to provide summary stats on threat rasters created by calcThreatDif.R

#sum by Threat/ period-------
#pre
pre_threat= mask(pre_threats,nbw_ImHab2023_UTM)

summary_pre<- lapply(1:nlyr(pre_threat), function(i) {
  # Extract each layer as a SpatRaster
  layer <- pre_threat[[i]]
  
  # Calculate summary statistics for the layer
  data.frame(
    Layer = names(pre_threat)[i],
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
threatSum_pre <- global(pre_threat, fun = 'sum', na.rm = TRUE)
threatsum_pre = sum(threatSum_pre, na.rm = T)
# Calculate the contribution of the layer
contributionPre <- (threatSum_pre / threatsum_pre)*100

#POST
#post
post_threat= mask(post_threats,nbw_ImHab2023_UTM)

summary_post<- lapply(1:nlyr(post_threat), function(i) {
  # Extract each layer as a SpatRaster
  layer <- post_threat[[i]]
  
  # Calculate summary statistics for the layer
  data.frame(
    Layer = names(post_threat)[i],
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

threatSum_post <- global(post_threat, fun = 'sum', na.rm = TRUE)
threatsum_post = sum(threatSum_post, na.rm = T)
contributionpost <- (threatSum_post / threatsum_post)*100


#total area calculations------

#Important Habitat area
nbw_ImHab2023_UTM$area = st_area(nbw_ImHab2023_UTM)
area_nbwhab_km2= as.numeric(sum(nbw_ImHab2023_UTM$area)/1000000)

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

#SUMMARY STATS DIF in threat-------
sumDif = post_threat- pre_threat
sumDif = mask(sumDif,nbw_ImHab2023_UTM)
layer = c( "Fishing Effort", 
   "Military Sonar Areas",
    "Oil & Gas Operations", 
     "Shipping Intensity")
Dif_StudyArea=global(sumDif, c("mean","sd","min","max","rms"),na.rm = T)
Dif_StudyArea = tibble(Dif_StudyArea, median = median(sumDif[], na.rm = T))
Dif_StudyArea = tibble(layer, Dif_StudyArea)

# for MPA areas 
MPAS_Dif <- mask(sumDif,Gully_UTM)
Dif_MPAs=global(MPAS_Dif, c("mean","sd","min","max","rms"),na.rm = T)
Dif_MPAs = tibble(Dif_MPAs, median = median(MPAS_Dif[], na.rm = T))
Dif_MPAs = tibble(layer, Dif_MPAs)

#look at only stats for all CH areas (Gully zone 1 only)
CH_Dif <- mask(sumDif,NBW_CH_UTM%>%filter(DESCRIP != "The Gully"))
Dif_CH=   global((CH_Dif), c("mean","sd","min","max","rms"),na.rm = T)
Dif_CH = tibble(Dif_CH, median = median(CH_Dif[], na.rm = T))
Dif_CH = tibble(layer, Dif_CH)


#look at only stats for zone 1 only
GullyZ1Dif <- mask(sumDif,GullyZ1)
Dif_GullyZ1=   global((GullyZ1Dif), c("mean","sd","min","max","rms"),na.rm = T)
Dif_GullyZ1 = tibble(Dif_GullyZ1, median = median(GullyZ1Dif[], na.rm = T))
Dif_GullyZ1 = tibble(layer, Dif_GullyZ1)

#combine into one table
zone = c(rep("NBW Imp Habitat", 4), rep("Gully MPA",4), rep("Gully Z1", 4), rep("Critical Habitat", 4))
zone = cbind(zone)

DifStats_threats = tibble( zone, rbind(Dif_StudyArea, Dif_MPAs, Dif_GullyZ1, Dif_CH))

# Round all numeric columns to 2 decimal places
DifStats_threats <- DifStats_threats %>% 
  mutate(across(where(is.numeric), \(x) round(x, 1)))


#write table
write.csv (DifStats_threats, here::here("output/TABLES/DifStats_threats_tbl.csv"))
