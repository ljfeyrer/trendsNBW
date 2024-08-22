#calc CHI
# - The impact of stressors are summed for each period to assess differences in 
# Cumulative impacts and intensity pre and post 2004 periods,
# highlighting areas of change and differences in NBW habitat over time

#Read in Rasters to calc CHI----
CHI_path = here("output/GRIDS/CHI//")
pre_CHI <- rast(dir(path=CHI_path,pattern="pre", full.names  = T))
post_CHI <- rast(dir(path=CHI_path,pattern="post", full.names  = T))

## Sum Cumulative impact of individual threats-----

### Pre MPA

sumImpactPre =sum(pre_CHI, na.rm = T) 
sumImpactPre[sumImpactPre ==0] <- NA
sumImpactPre = sumImpactPre/4
# summary(sumImpactPre)
dsn = here::here("output/GRIDS/CHI/sum//")

#write Raster SUM CHI for Pre period
writeRaster(sumImpactPre, filename = paste0(dsn, "sumImpactPre.tif", sep = ""), overwrite = TRUE,filetype='GTiff')

### Post 2004
sumImpactpost =sum(post_CHI, na.rm = T) 
sumImpactpost[sumImpactpost ==0] <- NA
sumImpactpost = sumImpactpost/4
# summary(sumImpactpost)
#write Raster SUM CHI for Post period
writeRaster(sumImpactpost, filename = paste0(dsn, "sumImpactPost.tif", sep = ""), overwrite = TRUE,filetype='GTiff')

#plot total sum CHI Pre/Post-----
#plot Pre CHI
sumImpactPre_df = st_as_stars(sumImpactPre)

PlotPre =  ggplot() +  
  scale_fill_viridis_c(direction = 1, na.value=NA, option = "C", limits = c(0, 100))  +
  geom_stars(data = sumImpactPre_df,
             na.rm = T,
             downsample = 3) +
  geom_sf(
    data = nbw_ImHab2023_UTM,
    col = "black",
    fill = NA,
    size = .2
  ) +
  # geom_sf(data = bathy, col = "gray", size = 0.2) +
  geom_sf(data = Gully_UTM, col = "white",fill = NA, size = .3)+
  geom_sf(data = NBW_CH_UTM, col = "white",fill = NA, size = .3)+
  
  # add land region
  geom_sf(data = landUTM, color = NA, fill = "grey50") +
  theme(legend.position = "none") +
  labs(title = "Threats Prior to 2004") +
  # # add scale bar
  # annotation_scale(
  #   location = "br",
  #   width_hint = 0.25,
  #   text_cex = 0.6,
  #   bar_cols = c("grey40", "white")
  # ) +
  # set map limits
  coord_sf(xlim = c(x_min - 50000, x_max + 5000), ylim = c(y_min, y_max), expand = FALSE) +
  
  # format axes
  ylab("") +
  xlab("") +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +theme_bw()+
  guides(fill = guide_colourbar(title = "Intensity %"),
         colour = guide_colourbar(title = "Intensity %")) 

PlotPre

#plot Post CHI
sumImpactPost_df= st_as_stars(sumImpactpost)

PlotPost =  ggplot() +  
  scale_fill_viridis_c(direction = 1, na.value=NA, option = "C", limits = c(0,100))  +
  geom_stars(data = sumImpactPost_df,
             na.rm = T,
             downsample = 3) +
  geom_sf(
    data = nbw_ImHab2023_UTM,
    col = "black",
    fill = NA,
    size = .2
  ) +
  # geom_sf(data = bathy, col = "gray", size = 0.2) +
  geom_sf(data = Gully_UTM, col = "white",fill = NA, size = .3)+
  geom_sf(data = NBW_CH_UTM, col = "white",fill = NA, size = .3)+
  # add land region
  geom_sf(data = landUTM, color = NA, fill = "grey50") +
  theme(legend.position = "none") +
  labs(title = "Threats Post 2004") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.6,
    bar_cols = c("grey40", "white"),
    line_col = NA,
    text_col = "white"
  ) +
  # set map limits
  coord_sf(xlim = c(x_min - 50000, x_max + 5000), ylim = c(y_min, y_max), expand = FALSE) +
  # format axes
  ylab("") +
  xlab("") +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +theme_bw()+
  guides(fill = guide_colourbar(title = "Intensity %"),
         colour = guide_colourbar(title = "Intensity %")) 
PlotPost


#plot CHI periods together --------
CHI_map = PlotPre + PlotPost + plot_layout(guides = "collect")+ plot_annotation(title = "Distribution of Serious Threats", 
                                                                                theme = theme(plot.title = element_text(hjust=0.5)),
                                                                                tag_levels = 'A')&
  theme(legend.position = 'bottom', plot.tag.position = c(0, 1))

#save here
fig4CHIpath = here::here("figs/FigSum_CHI.png")

ggsave(fig4CHIpath, CHI_map, dpi = 300)




sumDif = sumImpactpost- sumImpactPre
summary(sumDif)
sumDif = mask(sumDif,nbw_ImHab2023_UTM)

writeRaster(sumDif, filename = here::here("output/GRIDS/CHI/Sum/sumDif.tif"), overwrite = TRUE,filetype='GTiff')

# Plot Total Difference------
# #as stars
sumDif_df= st_as_stars(sumDif)

A = ggplot() +
  geom_stars(data = sumDif_df,
             na.rm = T,
             downsample = 3) +
  scale_fill_gradientn(  colours = rev(c("dark orange", "white", "purple")),
                         na.value="transparent")+
  geom_sf(
    data = nbw_ImHab2023_UTM ,
    col = "dark grey",
    fill = NA,
    size = .2
  ) +
  geom_sf(data = bathy, col = "gray", fill = NA, size = 0.2) +
  # geom_sf(data = bathy, col = "gray", size = 0.2) +
  geom_sf(data = Gully_UTM, col = "black",fill = NA, size = .3)+
  geom_sf(data = NBW_CH_UTM, col = "black",fill = NA, size = .3)+
  
  # add land region
  geom_sf(data = land, color = NA, fill = "grey50") +
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

