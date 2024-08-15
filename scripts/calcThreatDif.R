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

# plot(pre_CHI[[1]])



#Pre individual threats facet maps---------
# as stars
Impactpre_df = st_as_stars(pre_threats)

labels = as_labeller(
  c(pre_fish = "Fishing Effort", 
    pre_MFAS = "Military Sonar Areas",
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
    data = nbw_ImHab2023_UTM,
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
  guides(fill = guide_colourbar(title = "Relative Threat Intensity"),
         colour = guide_colourbar(title = "Impact")) 

plotThreats1

 ggsave(here::here("figs/plotThreats_pre.png"), plotThreats1, dpi = 300)
 
 #Post individual threats facetmap -----
 # as stars
 Impactpost_df = st_as_stars(post_threats)
 
 labels = as_labeller(
   c(post_fish = "Fishing Effort", 
     post_MFAS = "Military Sonar Areas",
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
     data = nbw_ImHab2023_UTM,
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
   guides(fill = guide_colourbar(title = "Relative Threat Intensity")) 
 
 plotThreats2
 
 ggsave(here::here("figs/plotThreats_post.png"), plotThreats2, dpi = 300)
 
 # plot combined threats
 threats2_combined = plotThreats1 + plotThreats2 + plot_layout(guides = "collect")& theme(legend.position = 'bottom')
 ggsave(here::here("figs/plotThreats_combined.png"), threats2_combined, dpi = 300)
 

 
## Difference between pre and post periods-----
 dif_threats = post_threats - pre_threats
 dif_path = here("output/GRIDS/DIF//")
 
 
 writeRaster(dif_threats, filename = c("output/GRIDS/DIF/dif_fish.tif", 
                                       "output/GRIDS/DIF/dif_MFAS.tif",
                                       "output/GRIDS/DIF/dif_OG.tif",
                                       "output/GRIDS/DIF/dif_Ship.tif"),
             filetype = "GTiff", overwrite = TRUE)
 
 dif_threats = rast(dir(path=dif_path,pattern="dif", full.names  = T))

 
 labels = as_labeller(
   c(post_fish = "Fishing Effort", 
     post_MFAS = "Military Sonar Areas",
     post_oil = "Oil & Gas Operations", 
     post_Ship = "Shipping Intensity"))
 
 # #as stars
 dif_threats_df= read_stars(dir(path=dif_path,pattern="dif", full.names  = T))
 
 diffThreats_plot = ggplot() +
   geom_stars(data = dif_threats_df,
              na.rm = T,
              downsample = 3) +
   facet_wrap(~attribute, drop = F,
              # labeller = labels
              ) +
   scale_fill_gradientn(  colours = rev(c("dark orange", "white", "purple")),
                          na.value=NA)+
   geom_sf(
     data = nbw_ImHab2023_UTM ,
     col = "dark grey",
     fill = NA,
     size = .2
   ) +
   # geom_sf(data = bathy, col = "gray", size = 0.2) +
   geom_sf(data = Gully_UTM, col = "black",fill = NA, size = .3)+
   geom_sf(data = NBW_CH_UTM, col = "black",fill = NA, size = .3)+
   
   # add land region
   geom_sf(data = landUTM, color = NA, fill = "grey50") +
      # add scale bar
   annotation_scale(
     location = "br",
     width_hint = 0.25,
     text_cex = 0.6,
     bar_cols = c("grey40", "white")
   ) +
   # set map limits
   coord_sf(xlim = c(x_min - 50000, x_max + 5000), ylim = c(y_min, y_max), expand = FALSE) +
   # format axes
   ylab("") +
   xlab("") +
   
   theme(
     plot.margin = margin(0, 0, 0, 0, "cm"),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank()
   ) + 
   guides(fill = guide_colourbar(title = "Intensity %"),
          colour = guide_colourbar(title = "Intensity %"),
          line.type = guide_legend(title = NULL, nrow = 2)) +
   theme(legend.position = "bottom") 
 
 diffThreats_plot
 
 
sumDif = sumImpactpost- sumImpactPre
summary(sumDif)
sumDif = mask(sumDif,nbw_ImHab2023_UTM)

writeRaster(sumDif, filename = here::here("output/GRIDS/CHI/Sum/sumDif.tif"), overwrite = TRUE,filetype='GTiff')

# Plot Absolute Difference------
# #as stars
      sumDif_df= st_as_stars(sumDif)

A = ggplot() +
  geom_stars(data = sumDif_df,
             na.rm = T,
             downsample = 3) +
  scale_fill_gradientn(  colours = rev(c("dark orange", "white", "purple")),
                         na.value=NA)+
  geom_sf(
    data = nbw_ImHab2023_UTM ,
    col = "dark grey",
    fill = NA,
    size = .2
  ) +
  geom_sf(data = bathy, col = "gray", size = 0.2) +
  # geom_sf(data = bathy, col = "gray", size = 0.2) +
  geom_sf(data = Gully_UTM, col = "black",fill = NA, size = .3)+
  geom_sf(data = NBW_CH_UTM, col = "black",fill = NA, size = .3)+
  
  # add land region
  geom_sf(data = landUTM, color = NA, fill = "grey50") +
  labs(subtitle = "Total Difference") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.6,
    bar_cols = c("grey40", "white")
  ) +
  # set map limits
  coord_sf(xlim = c(x_min - 50000, x_max + 5000), ylim = c(y_min, y_max), expand = FALSE) +
  # format axes
  ylab("") +
  xlab("") +
  
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  guides(fill = guide_colourbar(title = "Intensity %"),
         colour = guide_colourbar(title = "Intensity %"),
         line.type = guide_legend(title = NULL, nrow = 2)) +
  theme(legend.position = "bottom") 

A

### RMSE difference-----
      # RMS error = Square Root(sum(e^2)/n) 
      # - A measure of the cell-by-cell difference between the two grids. 
      # - About 2/3 of all the cells will differ by less than the rmse.
      # - About 95% of all the cells will differ by less than twice the rmse.

      averageDif = mean(abs(sumDif))
      RMSE <- function(x, y) { sqrt(mean((x - y)^2, na.rm = T)) } 
# Now use it:
x = RMSE(values(sumImpactpost), values(sumImpactPre) )

RMSError = x

# RMSE = print(paste("Root Mean Square Error (RMSE)", round(x, digits = 4), sep = " = "))

#look at > 1RMSE?

RMSE_grid = sumDif
sumDif = mask(sumDif,nbw_ImHab2023_UTM)

RMSE_grid[RMSE_grid <x & RMSE_grid>=0] <- NA
RMSE_grid[RMSE_grid >-x & RMSE_grid<=0] <- NA

# Plot RMSE >1 Difference------
##as stars
RMSE_grid_df= st_as_stars(RMSE_grid)


B = ggplot() +
  geom_stars(data = RMSE_grid_df,
             na.rm = T,
             downsample = 3) +
  scale_fill_gradientn(  colours = rev(c("dark orange", "white", "purple")),
                         na.value=NA)+
  geom_sf(
    data = nbw_ImHab2023_UTM ,
    col = "dark grey",
    fill = NA,
    size = .2
  ) +
  geom_sf(data = bathy, col = "gray", size = 0.2) +
  # geom_sf(data = bathy, col = "gray", size = 0.2) +
  geom_sf(data = Gully_UTM, col = "black",fill = NA, size = .3)+
  geom_sf(data = NBW_CH_UTM, col = "black",fill = NA, size = .3)+
  
  # add land region
  geom_sf(data = landUTM, color = NA, fill = "grey50") +
  labs(subtitle = "Significant Difference (RMSE)") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.6,
    bar_cols = c("grey40", "white")
  ) +
  # set map limits
  # set map limits
  coord_sf(xlim = c(x_min - 50000, x_max + 5000), ylim = c(y_min, y_max), expand = FALSE) +
  # format axes
  ylab("") +
  xlab("") +
  theme_bw()+
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  guides(fill = guide_colourbar(title = "Intensity %"),
         colour = guide_colourbar(title = "Intensity %"),
         line.type = guide_legend(title = NULL, nrow = 2)) +
  theme(legend.position = "bottom") 
B


# Plot together------
#for some reason patchwork requires '&' to make legend move to bottom postion

CHI_dif = A + B +plot_layout(guides = 'collect')+

  plot_annotation(title = 'Difference in Threats Between Periods', tag_levels = 'A')& theme(legend.position = 'bottom')


#save here
fig5CHIpath = here::here("figs/Fig5_CHIdif.png")

ggsave(fig5CHIpath, CHI_dif, dpi = 300)

