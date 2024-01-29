#### E. Climate Change - SST anomalies--------
# - From Halpern et al. 2015. Number of positive SST anomalies in the period relative to SST in 1985-1990.
# - From Halpern et al. 2008 "Changes in sea surface temperature as a result of climate change will have varying effects on species and ecosystems... Potential for ecological change in response to changes in sea surface temperature can be measured as the frequency of temperature anomalies, where the temperature exceeds a threshold value like the long-term mean, or by a measure of the magnitude (°C) of the anomalies themselves (S19, 20)." 

##  Import pre- SST raster -------
# layers taken from Halpern et al. 2015
dsn = here::here("Data/SST/pre_SST.tif")
gridList = raster(dsn)

#reproject, crop and save
writepath = here::here("Results/GRIDS/Effort/PRE/")
halpernRaster(gridList, dsn, nbw_habUTM, writepath)

#no negative values in pre SST 

##  Import post- SST raster -------
# layers taken from Halpern et al. 2015
dsn = here::here("Data/SST/post_SST.tif")
gridList = raster(dsn)

#reproject, crop and save
writepath = here::here("Results/GRIDS/Effort/POST/")
halpernRaster(gridList, dsn, nbw_habUTM, writepath)

postSST = raster(here::here("Results/GRIDS/Effort/POST/post_SST.tif"))
#correct negative values in post SST 
postSST[postSST <0] <- 0

writeRaster(postSST, filename = here("Results/GRIDS/Effort/POST/post_SST"), overwrite = TRUE,format='GTiff')

#maps ----
# early period
preSSTDF = read_stars(here::here("Results/GRIDS/Effort/PRE/pre_SST.tif"))

# Plot Pre Map m15------
m15 = ggplot() +
  geom_stars(data = preSSTDF,
             na.rm = T,
             downsample = 3) +
  scale_fill_continuous(
    type = "viridis",
    direction = 1,
    guide="colorbar", na.value=NA, limits = c(0, 75 ),
    breaks = c(5, 25, 50, 100)
  )+
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
  labs(title = "SST anomalies 2000-2005") +
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
  guides(fill = guide_colourbar(title = "Anomalies"),
         colour = guide_colourbar(title = "Anomalies")) + theme_bw() 

m15

#post - 
postSSTDF = st_as_stars(postSST)

# Plot Post Map m16------
m16 = ggplot() +
  geom_stars(data = postSSTDF,
             na.rm = T) +
  scale_fill_continuous(
    type = "viridis",
    direction = 1,
    guide="colorbar", na.value=NA, limits = c(0, 75 ),
    breaks = c(5, 25, 50, 100)
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
  labs(title = "SST Anomalies 2005-2010") +
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
  guides(fill = guide_colourbar(title = "Anomalies"),
         colour = guide_colourbar(title = "Anomalies")) + theme_bw() 

m16
