#### D. Shipping
# - From Halpern et al. 2015. Intensity raster of global shipping traffic is the same for both periods
# - As described in Halpern et al. 2015, this layer combines several years of shipping data but are mostly a static description of shipping patterns and intensity during 2011. 
# - Shipping is not excluded from Gully MPA



##  Import shipping raster -------
# layers taken from SSHUA

ship_pre = rast("shapes/Shipping/traffic2000")

plot(ship_pre)





dsn = here::here("shapes/Shipping/shipping.tif")
gridList = rast(dsn)

#reproject, crop and save
writepath = here::here("Results/GRIDS/Effort/PRE/pre_")
halpernRaster(gridList, dsn, nbw_habUTM, writepath)

file.copy(from = paste0(writepath, "shipping.tif"),
          to = here::here("Results/GRIDS/Effort/POST/post_shipping.tif"))

#maps PRE/POST MPA
ShipDF = read_stars(here::here("Results/GRIDS/Effort/POST/post_shipping.tif"))

# Plot Map m14------
m14 = ggplot() +
  geom_stars(data = ShipDF,
             na.rm = T,
             downsample = 3) +
  scale_fill_continuous(
    type = "viridis",
    direction = 1,
    values = scales::rescale(c(0, 0.1, 5)),
    na.value = NA
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
  labs(title = "Shipping Traffic Intensity (2003-2011)") +
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
  guides(fill = guide_colourbar(title = "Ship density"),
         colour = guide_colourbar(title = "Ship density")) + theme_bw() 


