#B. Military activities (MFAS)-----
#
# - Military exercises– The Department of National Defence has designated Firing
# Practice and Exercise Areas off the coasts of Canada. Activities in these
# areas may include bombing practice from aircraft, air-to-air, air-to-sea or
# ground firing, and anti-aircraft firing, etc. In Atlantic Canada, this
# includes sea area employments for sub-surface operations and firing exercises
# (FIREX) (Department of National Defense Government of Canada, 2021). Polygon
# shapefiles of the designated areas were downloaded from the Canadian
# Government open data portal
# (https://open.canada.ca/data/en/dataset/73111c78-298b-4be9-97f1-7aaa73cab477).
#
# - With no effort data on military exercises in these areas across the entire
# period, we interpreted them to represent threat potential for military
# mid-frequency active sonar (MMFAS). Although noise impacts can extend outside
# these areas, the most acute threat would occur inside the exercise areas.
# According to the Canadian Coast Guard Navigational Warnings (NAVWARNs),
# previously known as Notices to Shipping (NOTSHIPS), there were 44 notices of
# military exercises in areas overlapping with beaked whale habitat, on 54 days
# in 2019 (the first year notices were made available online), lasting between
# 1-5 days each (Government of Canada, 2021). However, this included 8
# (non-consecutive) days of the biannual international CUTLASS FURY exercises.
# This biannual event began in 2016 and has been described as the “largest
# international military exercise in Canadian waters” (The Canadian Press,
# 2016).  Excluding CUTLASS FURY 2019, we used the proportion of days per year
# with notifications to mariners (46 days = 12.6%) as a baseline for effort
# across the area in any given year. Although there is little information on the
# extent of military activity in the early period, there were two large
# international exercises as part of the public record in 2016 and 2019 (i.e.,
# CUTLASS FURY). To account for the impact of the international exercises, we
# have increased the % effort to 15% per year in the period after 2004. In
# addition, we removed the MPA from this layer in the contemporary period, as
# the MPA management plan (Fisheries and Oceans Canada, 2017) indicates that
# although the Gully MPA cannot legally exclude military activities, in practise
# the government agrees that such exercises do not occur inside the MPA.

#read in polygon data-----
DND_areas = read_sf(here::here("shapes/MilitaryEx/DND_areas.shp"))
DND_areasUTM = DND_areas%>%st_transform(UTM20)
DND_areasUTM = vect(DND_areasUTM)
#rasterize by count and set overlapping polygons to value = 1----
dsn = here::here("output/GRIDS/PRE/")
DND2Raster =  rasterize(DND_areasUTM, template)

DND2Raster[DND2Raster ==0] <- NA
DND2Raster[DND2Raster >0] <- 1

#Early Period (1988-2004)------        
#Make average intensity 10% based on estimate of activity across all years----
preDND = DND2Raster*.25

#write raster file for pre
writeRaster(preDND, filename = here::here("output/GRIDS/PRE/MFAS/preDND.tif"), overwrite = TRUE, filetype = "GTiff")

#transform raster to stars for plotting
preDND_df = st_as_stars(preDND)

#plot early period      -----
m8 = ggplot() + scale_fill_gradientn(
  limits = c(0, 1),
  colours = c(NA, "red"),
  values = scales::rescale(c(0, 0.25, .5)),
  guide = "colorbar",
  na.value = NA
) +
  geom_stars(data = preDND_df,
             na.rm = T,
             downsample = 3) +
  geom_sf(
    data = nbw_ImHab2023_UTM,
    col = "black",
    fill = NA,
    size = .2
  )+

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
    
  ) +  
  labs(title = "Military Areas Historical") +
  guides(fill = guide_colourbar(title = "Estimated effort"))+
  theme(legend.position = "none") 

m8

# Contemporary Period (2005-2019)-------
# Make average intensity 15% based on estimate of increase in activity in recent years----
postDND = DND2Raster*.5

#Mask Gully out of contemporary period-----
postDND <- mask(postDND,Gully_UTM, inverse = T)
#write raster file for post
writeRaster(postDND, filename = "output/GRIDS/POST/MFAS/postDND.tif", overwrite = TRUE,filetype='GTiff')

#transform raster to stars for plotting
postDND_df = st_as_stars(postDND)

#plot contemporary period-----
m9 = ggplot() + scale_fill_gradientn(
  limits = c(0, 1),
  colours = c(NA, "red"),
  values = scales::rescale(c(0, 0.25, .5)),
  guide = "colorbar",
  na.value = NA
) +
  geom_stars(data = postDND_df,
             na.rm = T,
             downsample = 3) +
 
  geom_sf(
    data = nbw_ImHab2023_UTM,
    col = "black",
    fill = NA,
    size = .2
  )+
  
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
    
  ) +
  guides(fill = guide_colourbar(title = "Estimated effort"))+
  theme(legend.position = "none") +

  labs(title = "Military Areas Contemporary") 
m9
