# #### A. Regional Fishing Effort ------
# - Long line fishery effort was not available as part of Halpern et al.'s global dataset for the study area. Code below imports data for the fisheries created by: 
# 
# > S. Butler, D. Ibarra and S. Coffen-Smout, 2019. Maritimes Region Longline and Trap Fisheries Footprint Mapping for Marine Spatial Planning and Risk Assessment. Can. Tech. Rep. Fish. Aquat. Sci. 3293: v + 30 p. Data available at: https://open.canada.ca/data/en/dataset/3d2e1a84-20f5-4a61-90e4-94760c80ebb9  
#   
# 
# #####  i. Bottom Longline effort------
#   
# - Uses bottom long line fishing effort data created by Butler et al. 2019, where fishing effort is sum of effort in hrs/ km2 over the entire period 2002-2017. 
# - Early Period uses this dataset as is, given overall spatial effort has not changed. Contemporary period uses this dataset with Zone 1 of Gully removed to account for fishing closures that occured there post 2004.

#read in bottom longline data as raster (already in UTM)-----
bot_long_fish = raster(here::here("Data/Fishing_Effort/bottomll"))

#crop and mask to same extent as habitat area-----
bot_long_fish <- crop(bot_long_fish, nbw_hab)
bot_long_fish <- mask(bot_long_fish, nbw_hab)
bot_long_fish =  projectRaster(bot_long_fish, template, method = 'ngb')

#write effort grid----
writeRaster(
  bot_long_fish,
  filename = here("Results/GRIDS/Effort/PRE/Fish/prebot_long_fish"),
  overwrite = TRUE,
  format = 'GTiff'
)
#transform raster to stars for plotting

bot_long_fish_df = st_as_stars(bot_long_fish)
# Plot Map m2------
m2 = ggplot() +
  geom_stars(data = bot_long_fish_df,
             na.rm = T,
             downsample = 3) +
  scale_fill_continuous(
    type = "viridis",
    direction = 1,
    trans = "log",
    na.value = NA, limits = c(1, 4000 ),
    breaks = c(100, 1000, 3000)
  ) +
  geom_sf(
    data = nbw_habUTM,
    col = "black",
    fill = NA,
    size = .2
  ) +
  geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
  # add land region
  geom_sf(data = landUTM, color = NA, fill = "grey50") +
  theme(legend.position = "none") +
  labs(title = "Bottom Longline 1988-2004 ") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.6,
    bar_cols = c("grey40", "white")
  ) +
  # set map limits
  coord_sf(
    xlim = c(26000, 1518416),
    ylim = c(4451888, 5104112),
    crs =  st_crs(UTM20)
  ) +
  # format axes
  ylab("") +
  xlab("") +
  theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  guides(fill = guide_colourbar(title = "Sum hrs/ Km2"),
         colour = guide_colourbar(title = "Sum hrs/ Km2")) + theme_bw() 

# Contemporary Period (2005-2019)-------

# write raster for contemporary LL fish effort
writeRaster(
  bot_long_fish,
  filename = here("Results/GRIDS/Effort/POST/Fish/postbot_long_fish"),
  overwrite = TRUE,
  format = 'GTiff'
)

#plot Map m3-------
m3 =    ggplot() +
  geom_stars(data = bot_long_fish_df,
             na.rm = T,
             downsample = 3) +
  scale_fill_continuous(
    type = "viridis",
    direction = 1,
    trans = "log",
    na.value = NA, limits = c(1, 4000 ),
    breaks = c(100, 1000, 3000)
  ) +
  geom_sf(
    data = nbw_habUTM,
    col = "black",
    fill = NA,
    size = .2
  ) +
  geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
  # add land region
  geom_sf(data = landUTM, color = NA, fill = "grey50") +
  theme(legend.position = "none") +
  labs(title = "Bottom Longline 2005-2019 ") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.6,
    bar_cols = c("grey40", "white")
  ) +
  # set map limits
  coord_sf(
    xlim = c(26000, 1518416),
    ylim = c(4451888, 5104112),
    crs =  st_crs(UTM20),
    expand = FALSE
  ) +
  # format axes
  ylab("") +
  xlab("") +
  theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  guides(fill = guide_colourbar(title = "Sum hrs/ Km2"),
         colour = guide_colourbar(title = "Sum hrs/ Km2")) + theme_bw() 


##### ii. Pelagic longline effort-------
#
# - Pelagic longline fishing effort data is provided in mins/ km2 from 2003-2018
# in log scale. 
# - Dataset focuses on vessels reporting to DFO Maritimes region
# and only covers the western region off Newfoundland. Additional effort occurs
# in Newfoundland waters but was not included in this dataset. - Early Period
# uses this dataset as is, given overall spatial effort has not changed.
# Contemporary period uses same dataset with Zone 1 of Gully removed to account
# for fishing closures that occured post 2004.

#import pelagic longline - data is in log scale-----
Pel_long_fish = raster(here::here("Data/Fishing_Effort/PLL/PLL_mins.tif"))

#deal with log scale
Pel_long_fish = mask(Pel_long_fish, nbw_hab, inverse = FALSE)
Pel_long_fish2 = (10 ^ (Pel_long_fish))

#write and transform to UTM
Pel_long_fish <-
  projectRaster(Pel_long_fish2, template, crs = UTM20, res = 1000)

#Early Period (1988-2004)------
writeRaster(
  Pel_long_fish,
  filename = here::here("Results/GRIDS/Effort/PRE/Fish/prePel_long_fish"),
  overwrite = TRUE,
  format = 'GTiff'
)

# Plot map m4--------
#transform raster to stars for plotting
Pel_long_fish_df = st_as_stars(Pel_long_fish)

m4 = ggplot() +
  scale_fill_continuous(
    type = "viridis",
    direction = 1,
    trans = "log",
    na.value = NA, limits = c(1, 11000 ),
    breaks = c(100, 1000, 10000)
  ) +
  geom_stars(data = Pel_long_fish_df,
             na.rm = T,
             downsample = 3) +
  geom_sf(
    data = nbw_habUTM,
    col = "black",
    fill = NA,
    size = .2
  ) +
  geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
  # add land region
  geom_sf(  data = landUTM, color=NA, fill="grey50") +
  # add scale bar
  annotation_scale(location = "br", width_hint=0.25,
                   text_cex = 0.6,
                   bar_cols = c("grey40", "white")) +
  # set map limits
  coord_sf(xlim = c(26000, 1518416), ylim = c(4451888, 5104112 ), crs =  st_crs(UTM20), expand = FALSE) +
  # format axes
  ylab("") + 
  xlab("") +
  theme(plot.margin = margin(.5, .5, .5, .5, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="right" )+
  labs(title = "Pelagic Longline 1988-2004") +
  theme_bw() +
  guides(fill = guide_colourbar(title = "mins/ Km2"))

# Contemporary Period (2005-2019)-------

# write raster for contemporary bot LL fish effort
writeRaster(
  Pel_long_fish,
  filename = here::here("Results/GRIDS/Effort/POST/Fish/postPel_long_fish"),
  overwrite = TRUE,
  format = 'GTiff'
)

# Plot map m5-----

m5 = ggplot() +
  scale_fill_continuous(
    type = "viridis",
    direction = 1,
    trans = "log",
    na.value = NA, limits = c(1, 11000 ),
    breaks = c(100, 1000, 10000)
  ) +
  geom_stars(data = Pel_long_fish_df,
             na.rm = T,
             downsample = 3) +
  geom_sf(
    data = nbw_habUTM,
    col = "blue",
    fill = NA,
    size = .2
  ) +
  geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
  geom_sf(data = landUTM, color = NA, fill = "grey50") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.6,
    bar_cols = c("grey40", "white")
  ) +
  # set map limits
  coord_sf(
    xlim = c(26000, 1518416),
    ylim = c(4451888, 5104112),
    crs =  st_crs(UTM20),
    expand = FALSE
  ) +
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
  guides(fill = guide_colourbar(title = "mins/ Km2"))+
  labs(title = "Pelagic Longline 2005-2019") 

# #### iii. Halpern et al. (2015) Fishing Effort --------
# used landings normalized globally by productivity 
# - Methods for estimating fishing effort is based annual wild caught industrial fisheries catch (tonnes) 
# by taxa and gear type at 0.5° resolution as described in: 
# - Halpern, B. S. et al. Patterns and emerging trends
# in global ocean health. PlosOne DOI:10.1371/journal.pone.0117863 (2015). 

# Layers are separated based on gear type and in units of tons of fish per tons
# of carbon in each 1 km2. 
# Data downloaded from:
# Benjamin Halpern, Melanie Frazier, John Potapenko, Kenneth Casey, Kellee Koenig, et al. 2015. Cumulative
# human impacts: raw stressor data (2008 and 2013). Knowledge Network for
# Biocomplexity. doi:10.5063/F1S180FS.

# - Gear type layers included here: 
# i. demersal destructive (e.g., bottom trawl), 
# - ii. demersal non-destructive high bycatch (e.g., pots, traps), 
# - iii. demersal non-destructive low bycatch (e.g., hook and line), 
# - iv. pelagic low bycatch (e.g., hook and line) 
# - Post 2004, consistent with other fishing effort layers uses this dataset with Zone 1
# of Gully removed to account for fishing closures that occured in 2005


    ##  Import pre 2004 Halpern fishing effort-------
# layers taken from Halpern et al. 2015, based on 2006
# read in as a stack
dsn = here::here("Data/Fishing_Effort/FISHEF_2008/")
gridList = stackRaster(dsn)

#reproject, crop and save as rasters
writepath = here::here("Results/GRIDS/original/PRE_Halpern/")
halpernRaster(gridList, dsn, nbw_habUTM, writepath)

##Import post 2004 Halpern fishing effort-------
# layers taken from Halpern et al. 2015, based on 2011
# read in as a stack
dsn = here::here("Data/Fishing_Effort/FISHEF_2015/")
#combine rasters for period post 2004    
gridList = stackRaster(dsn)

#reproject, crop and save as rasters
writepath = here::here("Results/GRIDS/original/POST_Halpern/")
halpernRaster(gridList, dsn, nbw_habUTM, writepath)

#sum all rasters for each period -------

###pre 2004 Halpern fishing effort###----
#dsn is where it reads in list of files from
dsn = here("Results/GRIDS/original/PRE_Halpern/")
#where to save files
writepath = here::here("Results/GRIDS/Effort/PRE/Fish/")
file = "pre_fishHalpern"
#function to sum raster
SumRaster(dsn, writepath, file)

###post 2004 Halpern fishing effort###----
##dsn is where it reads in list of files from
dsn = here("Results/GRIDS/original/POST_Halpern/")

#where to save files
writepath = here::here("Results/GRIDS/Effort/POST/Fish/")
file = "post_fishHalpern"
SumRaster(dsn, writepath, file)

#plot maps
##transform raster to stars for plotting
fishHalpern_df= read_stars(here::here("Results/GRIDS/Effort/PRE/Fish/pre_fishHalpern.tif"))

# Plot Map m12------
m12 = ggplot() +
  geom_stars(data = fishHalpern_df,
             na.rm = T,
             downsample = 3) +
  scale_fill_continuous(
    type = "viridis",
    direction = 1,
    trans = "log",
    na.value = NA, limits = c(.01, 25 ),
    breaks = c(1, 10, 25)
  )  +
  geom_sf(
    data = nbw_habUTM,
    col = "black",
    fill = NA,
    size = .2
  ) +
  geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
  # add land region
  geom_sf(data = landUTM, color = NA, fill = "grey50") +
  theme(legend.position = "none") +
  labs(title = "Other Fishing Gear 1988-2004 ") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.6,
    bar_cols = c("grey40", "white")
  ) +
  # set map limits
  coord_sf(
    xlim = c(26000, 1518416),
    ylim = c(4451888, 5104112),
    crs =  st_crs(UTM20),
    expand = FALSE
  ) +
  # format axes
  ylab("") +
  xlab("") +
  theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  guides(fill = guide_colourbar(title = "Landings \n(tons)"),
         colour = guide_colourbar(title = "Landings \n(tons)")) + theme_bw() 


#contemporary period---------
# Plot Map m12------
m13 = ggplot() +
  geom_stars(data = fishHalpern_df,
             na.rm = T,
             downsample = 3) +
  scale_fill_continuous(
    type = "viridis",
    direction = 1,
    trans = "log",
    na.value = NA, limits = c(.01, 25 ),
    breaks = c(1, 10, 25)
  )  +
  geom_sf(
    data = nbw_habUTM,
    col = "black",
    fill = NA,
    size = .2
  ) +
  geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
  # add land region
  geom_sf(data = landUTM, color = NA, fill = "grey50") +
  theme(legend.position = "none") +
  labs(title = "Other Fishing Gear 2005-2019 ") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.6,
    bar_cols = c("grey40", "white")
  ) +
  # set map limits
  coord_sf(
    xlim = c(26000, 1518416),
    ylim = c(4451888, 5104112),
    crs =  st_crs(UTM20),
    expand = FALSE
  ) +
  # format axes
  ylab("") +
  xlab("") +
  theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )   +
  guides(fill = guide_colourbar(title = "Landings \n(tons)"),
         colour = guide_colourbar(title = "Landings \n(tons)")) + theme_bw() 
