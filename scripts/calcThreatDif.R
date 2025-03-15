# Compare change in intensity of human activities ----
# - Input layers summarize effort or footprint associated with known stressors
# associated with threats to NBW in the pre and post MPA periods 
# - Threat intensity is standardized using decile % classes of underlying data values
# or as presence absence of threat footprint relative range is from from 0-1 
# - All threats considered are assessed as serious for NBW, 
# all stressors are assessed as having intensity impacts on the same scale
# - The impact of stressors are summed for each period to assess differences in 
# Cumulative impacts and intensity pre and post 2004 periods,
# highlighting areas of change and differences in NBW habitat over time

#Read in Rasters to calc CHI----
CHI_path = here("output/GRIDS/CHI//")
pre_threats <- rast(dir(path=CHI_path,pattern="pre", full.names  = T))
post_threats <- rast(dir(path=CHI_path,pattern="post", full.names  = T))

# plot(post_threats[[1]])



#Pre individual threats facet maps---------
# as stars
Impactpre_df = st_as_stars(pre_threats)

labels = as_labeller(
  c(pre_fish = "Fishing Effort", 
    pre_MFAS = "Military Activity Areas",
    pre_oil = "Oil & Gas Operations", 
    pre_Ship = "Shipping Intensity"))

pal = viridis_pal(option = "A", 
            direction = -1)(11)

pal = pal[1:10]

#plot
plotThreats1 =  ggplot() +  
  scale_fill_gradientn(colors = pal, limits=c(0, 100),
    
    na.value = "transparent"
  ) +  
  theme_bw()+
  geom_sf(data = bathy, col = "gray", size = 0.2) +
  geom_stars(data  = Impactpre_df, alpha = .95,
             na.rm = T)+
  geom_sf(
    data = nbw_ImHab_UTM,
    col = "black",
    fill = NA,
    size = .2
  ) +
  geom_sf(
    data = Gully_UTM,
    col = "blue",
    fill = NA,
    size = 2
  ) +
  geom_sf(
    data = NBW_CH_UTM,
    col = "blue",
    fill = NA,
    size = 1
  ) +
 
    # add land region
  geom_sf(data = landUTM, color = NA, fill = "grey50") +
  labs(title = "Early Period") +
  # add scale bar
  annotation_scale(
   location = "br",
     text_cex = 0.6,
    bar_cols = c("grey40", "white")
  ) +
  facet_wrap(~attributes, drop = F, 
             labeller = labels, ncol = 1) +
  # set map limits
  coord_sf(xlim = c(x_min - 50000, x_max + 5000), ylim = c(y_min, y_max), expand = FALSE) +
  
  # format axes
  ylab("") +
  xlab("") +
  theme(panel.spacing.x = unit(4, "mm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position="right"
  ) +
  # we set the left and right margins to 0 to remove 
  # unnecessary spacing in the final plot arrangement.
  theme(plot.margin = margin(0, .5, 0, 0, "pt"), plot.title = element_text(hjust=0.5), plot.subtitle  = element_text(hjust=0.5))+
  guides(fill = guide_colourbar(title = "Relative\nIntensity", direction = "vertical"),
         colour = guide_colourbar(title = "Impact")) 

plotThreats1

 ggsave(here::here("figs/plotThreats_pre.png"), plotThreats1, dpi = 300)
 
 #Post individual threats facetmap -----
 # as stars
 Impactpost_df = st_as_stars(post_threats)
 
 labels = as_labeller(
   c(post_fish = "Fishing Effort", 
     post_MFAS = "Military Activity Areas",
     post_oil = "Oil & Gas Operations", 
     post_Ship = "Shipping Intensity"))
 
 #palette without black
 pal = viridis_pal(option = "A", 
                   direction = -1)(11)
  pal = pal[1:10]
 
 #plot
 plotThreats2=  ggplot() +  
   scale_fill_gradientn(colors = pal, limits=c(0, 100),
                        
                        na.value = "transparent"
   ) +  
   theme_bw()+
   geom_sf(data = bathy, col = "gray", size = 0.2) +
   geom_stars(data  = Impactpost_df, alpha = .95,
              na.rm = T)+
   geom_sf(
     data = nbw_ImHab_UTM,
     col = "black",
     fill = NA,
     size = .2
   ) +
   geom_sf(
     data = Gully_UTM,
     col = "blue",
     fill = NA,
     size = 1
   )    +
   geom_sf(
     data = NBW_CH_UTM,
     col = "blue",
     fill = NA,
     size = 1
   ) +
   
   # add land region
   geom_sf(data = landUTM, color = NA, fill = "grey50") +
   labs(title = "Contemporary Period") +
   # add scale bar
   annotation_scale(
     location = "br",
    
     text_cex = 0.6,
     bar_cols = c("grey40", "white")
   ) +
   facet_wrap(~attributes, drop = F,  ncol = 1,
              labeller = labels) +
   # set map limits
   coord_sf(xlim = c(x_min - 50000, x_max + 5000), ylim = c(y_min, y_max), expand = FALSE) +
   
   # format axes
   ylab("") +
   xlab("") +
   theme(panel.spacing.x = unit(4, "mm"),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     legend.position="right", axis.ticks.y = element_blank(), axis.text.y = element_blank(),
     axis.title.y = element_blank()
   ) +
   # we set the left and right margins to 0 to remove 
   # unnecessary spacing in the final plot arrangement.
   theme(plot.margin = margin(0, 0, 0, 0, "pt"), 
         plot.title = element_text(hjust=0.5), plot.subtitle  = element_text(hjust=0.5))+
   guides(fill = guide_colourbar(title = "Relative\nIntensity", direction = "vertical")) 
 
 plotThreats2
 
 ggsave(here::here("figs/plotThreats_post.png"), plotThreats2, dpi = 300)
 
 # plot combined threats
 threats2_combined = (plotThreats1 + theme(plot.margin = unit(c(0,-30,0,0), "pt")) ) + 
   # plot_spacer() +
   plotThreats2+
   plot_layout(guides = 'collect', axes = 'collect',  ncol = 2)+ plot_annotation(tag_levels = 'A' )&
   theme(legend.position = "right")
 ggsave(here::here("figs/plotThreats_combined.png"), threats2_combined, dpi = 300)
 
######
 
## Difference between pre and post periods-----
 dif_threats = (post_threats - pre_threats)
 # plot(post_threats)
 # plot(dif_threats)
 
 dif_path = here("output/GRIDS/DIF//")
 #
 #
 writeRaster(dif_threats, filename = c("output/GRIDS/DIF/dif_fish.tif",
                                       "output/GRIDS/DIF/dif_MFAS.tif",
                                       "output/GRIDS/DIF/dif_OG.tif",
                                       "output/GRIDS/DIF/dif_Ship.tif"),
             filetype = "GTiff", overwrite = TRUE)

 dif_threats = rast(dir(path=dif_path,pattern="dif", full.names  = T))

 
 labels = as_labeller(
   c(post_fish = "Fishing Effort", 
     post_MFAS = "Military Activity Areas",
     post_oil = "Oil & Gas Operations", 
     post_Ship = "Shipping Intensity"))
 
 # dir(path=dif_path,pattern="dif", full.names  = T)
 # #as stars
 dif_threats_df= st_as_stars(dif_threats)
 
 diffThreats_plot = ggplot() +  
   scale_fill_gradientn(colours = 
                          rev(c("#ff5400", "#ff6d00", "#ff8500",
                                "#ff9e00","gray", "#00b4d8","#0096c7",
                                "#0077b6","#023e8a")), 
                        limits=c(-100, 100), 
                        values = scales::rescale(c( -100, -.5, .5, 100)),
               na.value = "transparent"
               ) +
   theme_bw()+
   # geom_sf(data = bathy, col = "gray", size = 0.2) +
   geom_stars(data  = dif_threats_df, 
              na.rm = T)+
   geom_sf(
     data = nbw_ImHab_UTM,
     col = "#FFD300",
     fill = NA,
     linewidth = .8
   ) +
   geom_sf(
     data = Gully_UTM,
     col = "black",
     fill = NA,
     linewidth  = .75
   )    +
   geom_sf(
     data = NBW_CH_UTM%>%filter(DESCRIP != "The Gully"),
     col = "black",
     fill = NA,
     linewidth  = .75
   ) +
   
   # add land region
   geom_sf(data = landUTM, color = "darkgray", fill = "#9D8566") +
   labs(title = "") +
   # add scale bar
   annotation_scale(
     location = "br",
     
     text_cex = 0.6,
     bar_cols = c("grey40", "white")
   ) +
   facet_wrap(~attributes, drop = F, 
              labeller = labels) +
   # set map limits
   coord_sf(xlim = c(x_min - 50000, x_max + 5000), ylim = c(y_min, y_max), expand = FALSE) +
   
   # format axes
   ylab("") +
   xlab("") +
   theme(
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     legend.position="bottom"
   ) +
   # we set the left and right margins to 0 to remove 
   # unnecessary spacing in the final plot arrangement.
   theme(plot.margin = margin(0, 0, 0, 0), plot.title = element_text(hjust=0.5), plot.subtitle  = element_text(hjust=0.5))+
   guides(fill = guide_colourbar(title = "Intensity Difference")) 
 
   
 diffThreats_plot
 
 ggsave(here::here("figs/plotDifThreats.png"), diffThreats_plot, height = 8, width = 8, units = "in", dpi = 300)
 
