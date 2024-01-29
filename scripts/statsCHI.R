#script to provide summary stats on rasters created by calCHI.R

#sum by Threat/ period-------
#pre
threatStack = brick(pre)

preT =   summary(threatStack)%>%broom::tidy()%>%pivot_longer( cols = -.rownames, names_to = c("Layer"))%>%
  pivot_wider(names_from = ".rownames", values_from = "value" )%>%dplyr::select(-`NA's`)%>%mutate(Layer = gsub("pre", "", Layer))

#figure out percentage contribution of each threat to total score
pre.sum = cellStats(threatStack, sum)
pre.sumCHI = sum(pre.sum)


pct_sum = round(pre.sum/pre.sumCHI, digits = 3)
pct_sum = as.data.frame(pct_sum)%>%tibble::rownames_to_column("Layer2")

preT = cbind(preT, pct_sum)%>%select(-Layer2)%>%rename( '% CHI' =pct_sum)

preT = preT%>%select(c(1, 4,6,7))%>%arrange()%>%rename(Threat = Layer, `Median Score` = Median, `Max Score` = Max.)


#POST
threatStack = brick(post)

postT =   summary(threatStack)%>%broom::tidy()%>%pivot_longer( cols = -.rownames, names_to = c("Layer"))%>%
  pivot_wider(names_from = ".rownames", values_from = "value" )%>%dplyr::select(-`NA's`)%>%mutate(Layer = gsub("post", "", Layer))

#figure out percentage contribution of each threat to total score
post.sum = cellStats(threatStack, sum)
post.sumCHI = sum(post.sum)


pct_sum = round(post.sum/post.sumCHI, digits = 3)
pct_sum = as.data.frame(pct_sum)%>%tibble::rownames_to_column("Layer2")

postT = cbind(postT, pct_sum)%>%select(-Layer2)%>%rename( '% CHI' =pct_sum)

postT = postT%>%select(c(1, 4,6,7))%>%arrange()%>%rename(Threat = Layer, `Median Score` = Median, `Max Score` = Max.)

#write threats table
threats_tbl = cbind(preT, postT)
write.csv (threats_tbl, here::here("Results/TABLES/threats_tbl.csv"))


#total area calculations------

#Study area
nbw_habUTM$area = st_area(nbw_habUTM)
area_nbwhab_km2= as.numeric(sum(nbw_habUTM$area)/1000000)
#pre-MPAs
areapreMPA_km2 = as.numeric(sum(preMPA$area)/1000000)
#Post-MPAs
areapostMPA_km2 = as.numeric(sum(ACTIVE_MPAS$area)/1000000)
#Crit Hab
areaCritHab_km2 = as.numeric(sum(CritHab$area)/1000000)
#Gully
Gully_UTM$area_calc = st_area(Gully_UTM)
Gully_UTM$area_km2 = as.numeric((Gully_UTM$area_calc)/1000000)
areaGully_km2 = Gully_UTM%>%as_tibble()%>%filter(ZONE_ID >= 2 & ZONE_ID <5)%>%select(area_km2)%>%sum()

#Gully Z1
areaGZone1_km2 = Gully_UTM%>%as_tibble()%>%filter(ZONE_ID == 1)%>%select(area_km2)

#area of dif in MPAs
mpaAreaDif = areapostMPA_km2 - areapreMPA_km2

#sum total area and save table
names = tribble(~names, "nbwhab_km2", "areapreMPA_km2", "areapostMPA_km2", 
"mpaAreaDif", "areaCritHab_km2", "areaGully_km2" , "areaGZone1_km2")
km2 = "km2"
TotalArea = tibble(km2, area_nbwhab_km2, areapreMPA_km2, areapostMPA_km2, 
                   mpaAreaDif, areaCritHab_km2, areaGully_km2, areaGZone1_km2)

TotalArea = TotalArea%>%pivot_longer(- km2)%>%select(-km2)

#write table
write.csv (TotalArea, here::here("Results/TABLES/Area_tbl.csv"))

#ABSOLUTE DIF in CHI stats -------
sumDif = sumImpactpost- sumImpactPre
Dif_StudyArea=summary(sumDif)%>%broom::tidy()%>%pivot_longer( cols = -.rownames, names_to = c("Layer"))%>%
  pivot_wider(names_from = ".rownames", values_from = "value" )%>%select(-`NA's`)%>%mutate(Layer = "sumDif")
sdsumDif = cellStats(abs(sumDif), "sd")
meansumDif = cellStats(sumDif, "mean")

# for all MPAs areas 
MPAS_Dif <- mask(sumDif,ACTIVE_MPAS)
Dif_MPAs=summary(MPAS_Dif)%>%broom::tidy()%>%pivot_longer( cols = -.rownames, names_to = c("Layer"))%>%
  pivot_wider(names_from = ".rownames", values_from = "value" )%>%select(-`NA's`)%>%mutate(Layer = "MPAS_Dif")
# sumMPAS_Dif= cellStats(MPAS_Dif, "sum")
sdMPAS_Dif = cellStats(abs(MPAS_Dif), "sd")
meanMPAS_Dif = cellStats(MPAS_Dif, "mean")

#look at only stats for all CH areas (Gully zone 1 only)
CH_Dif <- mask(sumDif,CritHab)
Dif_CH=   summary(CH_Dif)%>%broom::tidy()%>%pivot_longer( cols = -.rownames, names_to = c("Layer"))%>%
  pivot_wider(names_from = ".rownames", values_from = "value" )%>%select(-`NA's`)%>%mutate(Layer = "CH_Dif")
# sumall_CH_Dif= cellStats(all_CH_Dif, "sum")
sdall_CH_Dif = cellStats(abs(CH_Dif), "sd")
meanall_CH_Dif = cellStats(CH_Dif, "mean")

#look at only stats for gully only
GullyDif <- mask(sumDif,Gully_UTM%>%filter(ZONE_ID >= 2 & ZONE_ID <5))
Dif_Gully=   summary(GullyDif)%>%broom::tidy()%>%pivot_longer( cols = -.rownames, names_to = c("Layer"))%>%
  pivot_wider(names_from = ".rownames", values_from = "value" )%>%select(-`NA's`)%>%mutate(Layer = "GullyDif")
# sumGullyDif= cellStats(GullyDif, "sum")
sdGullyDif = cellStats(abs(GullyDif), "sd")
meanGullyDif = cellStats(GullyDif, "mean")

#look at only stats for zone 1 only
GullyZ1Dif <- mask(sumDif,GullyZ1)
Dif_GullyZ1=   summary(GullyZ1Dif)%>%broom::tidy()%>%pivot_longer( cols = -.rownames, names_to = c("Layer"))%>%
  pivot_wider(names_from = ".rownames", values_from = "value" )%>%select(-`NA's`)%>%mutate(Layer = "GullyZ1Dif")
# sumGullyZ1Dif= cellStats(GullyZ1Dif, "sum")
sdGullyZ1Dif = cellStats(abs(GullyZ1Dif), "sd")
meanGullyZ1Dif = cellStats(GullyZ1Dif, "mean")

#combine into one table
zone = c("Study Area", "All MPAs", "Gully Z1", "Gully All", "Critical Habitat")
DifStats = tibble( rbind(Dif_StudyArea, Dif_MPAs, Dif_GullyZ1, Dif_Gully, Dif_CH), sd = signif(c(sdsumDif, sdMPAS_Dif, sdGullyZ1Dif, sdGullyDif, sdall_CH_Dif), 3), 
                   mean = signif(c(meansumDif, meanMPAS_Dif, meanGullyZ1Dif, meanGullyDif, meanall_CH_Dif), 3))

#write table
write.csv (DifStats, here::here("Results/TABLES/DifStats_tbl.csv"))

#CHI summary stats-------
#TABLES  
impact_pre=   summary(sumImpactPre)%>%broom::tidy()%>%pivot_longer( cols = -.rownames, names_to = c("Layer"))%>%
  pivot_wider(names_from = ".rownames", values_from = "value" )%>%select(-`NA's`)%>%mutate(Layer = "Pre")

impact_post=   summary(sumImpactpost)%>%broom::tidy()%>%pivot_longer( cols = -.rownames, names_to = c("Layer"))%>%
  pivot_wider(names_from = ".rownames", values_from = "value" )%>%select(-`NA's`)%>%mutate(Layer = "Post") 

summaryt = rbind(impact_pre, impact_post)

sumpre = cellStats(sumImpactPre, "sum")
sdpre = cellStats(sumImpactPre, "sd")
meanpre = cellStats(sumImpactPre, "mean")
sumpost = cellStats(sumImpactpost, "sum")
sdpost = cellStats(sumImpactpost, "sd")
meanpost = cellStats(sumImpactpost, "mean")

t.test(sumImpactPre,sumImpactpost )

sum.all = cbind(sum =round(c( sumpre, sumpost), 0), sd = signif(c(sdpre, sdpost), 3), mean = signif(c(meanpre, meanpost), 3))

 summarytc = cbind(summaryt, sum.all)
 rownames(summarytc) <- c()
 #write CHI impact table
 write.csv (summarytc, here::here("Results/TABLES/CHIsum_tbl.csv"))
 
 #calc dif between periods
 CHI_sum_dif = sumpost - sumpre
 CHI_max_dif = summaryt$Max.[2]-summaryt$Max.[1]
 CHI_med_dif = summaryt$Median[2]-summaryt$Median[1]
 
 CHI_dif_stats = tibble(CHI_sum_dif,CHI_max_dif, CHI_med_dif, mpaAreaDif)
 #write CHI dif table
 write.csv (CHI_dif_stats, here::here("Results/TABLES/CHIdif_tbl.csv"))


# area of difference between periods-------

#absolute dif in threats
 increase_thrt_km2 = (sum(sumDif[] > 0, na.rm =T )*res(sumDif)[]^2)/ 1000000
 decrease_thrt_km2 = (sum(sumDif[] < -0, na.rm =T )*res(sumDif)[]^2)/1000000

Absolute_dif = cbind(increase_thrt_km2 = round(increase_thrt_km2[1]), decrease_thrt_km2 = round(abs(decrease_thrt_km2[1])))

#for RMSE >1
summary(RMSE_grid)
increase_thrt_km2 = (sum(RMSE_grid[] >= 0.1, na.rm =T )*res(RMSE_grid)[]^2)/ 1000000
decrease_thrt_km2 = (sum(RMSE_grid[] <= -0.1, na.rm =T )*res(RMSE_grid)[]^2)/1000000

RMSE1_dif = cbind(increase_thrt_km2 = round(increase_thrt_km2[1]), decrease_thrt_km2 = round(abs(decrease_thrt_km2[1])))

#for RMSE >2
summary(RMSE_grid2)
increase_thrt_km2 = (sum(RMSE_grid2[] >= 0.1, na.rm =T )*res(RMSE_grid2)[]^2)/ 1000000
decrease_thrt_km2 = (sum(RMSE_grid2[] <= -0.1, na.rm =T )*res(RMSE_grid2)[]^2)/1000000

RMSE2_dif = cbind(increase_thrt_km2 = round(increase_thrt_km2[1]), decrease_thrt_km2 = round(abs(decrease_thrt_km2[1])))


#write CHI difference table
Layer = cbind(layer = c("Absolute_dif", "RMSE1_dif","RMSE2_dif" ))
dif_tble = cbind(Layer, rbind(Absolute_dif, RMSE1_dif, RMSE2_dif))

write.csv (dif_tble, here::here("Results/TABLES/dif_tbl.csv"))


#  table
BigDif = rbind(Absolute_dif, RMSE1_dif,RMSE2_dif)%>%as.data.frame()%>%tibble::rownames_to_column("Difference")%>%
select(Difference, `Area increase \nkm^2`= increase_thrt_km2, `Area decrease \nkm^2`= decrease_thrt_km2)%>%
  mutate(Difference = c("Absolute", ">1 RMSE", ">2 RMSE"))


