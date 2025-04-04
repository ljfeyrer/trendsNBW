#B. Military activities (MFAS)-----
# # Military exercises– The Department of National Defence has designated Firing
# Practice and Exercise Areas off the coasts of Canada. Activities in these
# areas may include bombing practice from aircraft, air-to-air, air-to-sea or
# ground firing, and anti-aircraft firing, etc. In Atlantic Canada, this
# includes sea area employments for sub-surface operations and firing exercises
# (FIREX) (Department of National Defense Government of Canada, 2021). Polygon
# shapefiles of the designated areas were downloaded from the Canadian
# Government open data portal
# (<https://open.canada.ca/data/en/dataset/73111c78-298b-4be9-97f1-7aaa73cab477>).
#
# -   With no effort data on military exercises in these areas across the
# entire period, we interpreted them to represent threat potential for military
# mid-frequency active sonar (MMFAS). Although noise impacts can extend outside
# these areas, the most acute threat would occur inside the exercise areas.
# There is little information on the extent of military activity in the early
# period, however the Canadian Coast Guard Navigational Warnings (NAVWARNs),
# previously known as Notices to Shipping (NOTSHIPS), indicate there were 44
# notices of military exercises in areas overlapping with beaked whale habitat,
# on 54 days in 2019 (the first year notices were made available online),
# lasting between 1-5 days each (Government of Canada, 2021). However, this
# included 8 (non-consecutive) days of the biannual international CUTLASS FURY
# exercises. This biannual event began in 2016 and has been described as the
# “largest international military exercise in Canadian waters” (The Canadian
# Press, 2016). Excluding CUTLASS FURY 2019, there were 46 days per year with
# notifications to mariners (12.6% year). We removed the MPA from this layer in
# the contemporary period, as the MPA management plan (Fisheries and Oceans
# Canada, 2017) indicates that although the Gully MPA cannot legally exclude
# military activities, in practise the government agrees that such exercises do
# not occur inside the MPA.

#read in polygon data-----
DND_areas = read_sf(here::here("shapes/MilitaryEx/DND.shp"))
DND_areasUTM = DND_areas%>%st_transform(UTM20)
DND_areasUTM = vect(DND_areasUTM)
# plot(st_geometry(DND_areas))

#rasterize by count and set overlapping polygons to value = 1----
dsn = here::here("output/GRIDS/PRE/")
DND2Raster =  rasterize(DND_areasUTM, template)
DND2Raster = mask(DND2Raster, SShelf)
# DND2Raster[DND2Raster == 0] <- NA
DND2Raster[DND2Raster >0] <- 100
# DND2Raster[is.na(DND2Raster)] <- 0

# plot(DND2Raster)

#Early Period (1988-2004)------        

preDND = DND2Raster
names(preDND) <- "pre_MFAS"

#transform raster to stars for plotting
preDND_df = st_as_stars(preDND)

#plot early period      -----
m8 = ggplot() + 
  scale_fill_continuous(
  type = "viridis", option = "A",
  direction = -1,
  na.value = "transparent"
) +
  
  geom_stars(data = preDND_df,
             na.rm = T,
             downsample = 3) +
  geom_sf(
    data = nbw_ImHab_UTM,
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

#Mask Gully out of contemporary period-----
postDND <- mask(DND2Raster,Gully_UTM, inverse = T, updatevalue =99 )
# plot(postDND)
postDND[postDND == 0] <- NA
postDND[postDND == 99] <- 0
postDND <- mask(postDND,preDND)

# plot(postDND1)


names(postDND) <- "post_MFAS"


#transform raster to stars for plotting
postDND_df = st_as_stars(postDND)

#plot contemporary period-----
m9 = ggplot() + 
  scale_fill_continuous(
    type = "viridis", option = "A",
    direction = -1,
    na.value = "transparent"
  ) +
  geom_stars(data = postDND_df,
             na.rm = T,
             downsample = 3) +
 
  geom_sf(
    data = nbw_ImHab_UTM,
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


#write raster files-----
#pre
pre = "output/GRIDS/PRE/MFAS/pre_MFAS.tif"
writeRaster(preDND, filename = pre, overwrite = TRUE, filetype = "GTiff")

#Post
post = "output/GRIDS/POST/MFAS/post_MFAS.tif"
writeRaster(postDND, filename = post, overwrite = TRUE, filetype = "GTiff")

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

