#map samples
# based on map in de greef et al. 2023

#load libraries
# if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  terra,
  rworldmap,
  rworldxtra,
  ggplot2,
  ggpubr,
  ggrepel,
ggspatial,
marmap,
  dplyr,
  sf, ggpattern
)
install.packages("ggpattern")
# Set map boundary as SpatExtent (xmin, xmax, ymin, ymax)
boundary <- ext(-60, -57, 43.2, 44.5)

# load and crop map outlines (already in sf)
map.outline <- getMap(resolution = "high") %>%
  st_as_sf() %>%
  st_crop(xmin = xmin(boundary), xmax = xmax(boundary), ymin = ymin(boundary), ymax = ymax(boundary))

# Download ocean depth map (bathy data remains raster-like, so no need to update)
ocean_map <- getNOAA.bathy(lon1 = -60, lon2 = -57, lat1 = 43.2, lat2 = 44.5, resolution = 1)

# Convert bathy object to dataframe for plotting
ocean_df <- marmap::fortify.bathy(ocean_map)

#load basemap shapes
#NBW Habitat Areas ---------
NBW_CH<- read_sf(here::here("~/CODE/shapefiles/SAR_CH/NBW_CH/NorthernBottlenoseWhale_CH.shp"))
#read in Gully Zones, 
Gully <- read_sf(here::here("~/CODE/shapefiles/ProtectedAreas/DFO/Gully/Gully_MPA.shp"))
##NBW IMP HAB 2023 area  
nbw_ImHab = read_sf(here::here("~/CODE/shapefiles/ImpHabitat/Feyreretal2024/NBW_ImHab_edit.shp"))%>%
  st_transform(4326)
#land
land <- read_sf(here::here("~/CODE/shapefiles/coastline/worldcountries/ne_50m_admin_0_countries.shp"))%>%
  dplyr::filter(CONTINENT == "North America")

# this bathy data is a bit smoother
bathy <- read_sf(here::here("~/CODE/shapefiles/Bathymetry/bathymetry_pl_v2/bathymetry_l_v2.shp"))%>%
  st_transform(4326)
#filter 
bathy = bathy%>%
  filter(DEPTH >150)


# Plot with ggplot2
nbw_map_ocean <- ggplot() + 
  geom_raster(data = ocean_df, aes(x = x, y = y, fill = z)) +
  scale_fill_stepsn(n.breaks = 8, colors = c("#012A4A", "#013a63",  "#014f86",    "#2a6f97", "#2c7da0", "#468FAF", "#61A5c2", "#89C2D9",  "#daf0ee")) +
  # geom_sf(data = map.outline, fill = "#89C2D9", color = "#89C2D9", linewidth = 0.5)+
  geom_sf(data = Gully, fill = "#34A0A4", color = NA, linewidth = 0.5, alpha = .5)+
  geom_sf(data = Gully%>%filter(ID == 0), fill = NA, color = "red", linewidth = 1, alpha = .5)+
  geom_sf(data = Gully%>%filter(ID == 1), fill = NA, color = "red", linewidth = 1, alpha = .5)+
  
  geom_sf(data = NBW_CH, fill = "white", color = "white", linewidth = 0.5, alpha = .25)+
  
    theme(legend.position = "none", axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank()) +
  # theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  # annotation_scale(location = "bl", width_hint = 0.2, 
  #                  bar_cols = c("gray30", "white"), text_cex = 0.6, text_col = "white") +
  # annotation_north_arrow(location = "tr", which_north = "true", 
  #                        style = north_arrow_fancy_orienteering, 
  #                        height = unit(1, "cm"), width = unit(1, "cm")) +
  annotate("text", x = -58.89, y = 43.65, label = "Gully", 
           fontface = "bold.italic", size = 8, color = "white") +
  annotate("text", x = -58.35, y = 43.9, label = "Shortland", 
           fontface = "bold.italic", size = 8, color = "white") +
  annotate("text", x = -57.85, y = 44.01, label = "Haldimand", 
           fontface = "bold.italic", size = 8, color = "white") +
  theme(text = element_text(family = "open sans", size = 15)) +
  coord_sf(
    xlim = range(ocean_df$x, na.rm = TRUE),
    ylim = range(ocean_df$y, na.rm = TRUE),
    expand = FALSE,
    crs = st_crs(map.outline)
  )

print(nbw_map_ocean)

ggsave("Output/map_abstract.png",nbw_map_ocean,  dpi = 300)



#plot

map = ggplot()+ 
  geom_sf(data = nbw_ImHab, aes(col = "Important Habitat"), fill = NA, 
          alpha = .1, linewidth = 1) +
  geom_sf(data = land, aes( color = "Sable Island"), fill = "#BBB4AB", linewidth = .8) +
  geom_sf(data = bathy, color = "gray") +
  geom_sf(data = Gully%>%filter(NAME == "Gully MPA (Marine Protected Area), outer boundary"), 
          aes(col = "Gully MPA"), fill = NA,  linewidth = .8) +
  geom_sf(data = NBW_CH, aes(fill = "Critical Habitat"), col = "black", alpha = .5, 
          linewidth= 1) +
  geom_sf(data = st_jitter(LV_SS_sf, factor =.0075), alpha = .15,  
          col= "#ff593d", fill = "#ff593d", shape =21, size = 1.5)  +
  
  theme_light()+guides(fill=guide_legend(ncol=1), base_size = 18)+
  theme(legend.position = "bottom", legend.title = element_blank())+
  coord_sf(lims_method = "orthogonal", xlim = xlims, ylim = ylims, crs = 4326, expand = T)    +  
  
  scale_fill_manual(values = c("Critical Habitat" = "lightblue"), name = "") +
  scale_color_manual(values = c("Gully MPA" = "darkblue", "Sable Island" = "#827D77", "Important Habitat" = "#FFD300"), name = "")+
  
  # # add scale bar
  annotation_scale(location = "tl", width_hint=0.25,
                   text_cex = 0.75,
                   bar_cols = c("grey40", "white"))  

#View
map
#save map
#  
gg_Fig2path =  here::here("OUTPUT/FIGS/Fig_1MAP.png")
ggsave(gg_Fig2path, map,dpi = 300)

