# Calcluate Cumulative Impact of Stressors----
# Assessing Cumulative impacts from individual threats
# - Input layers summarize effort or footprint associated with known stressors
# associated with threats to NBW in the pre and post 2004 periods 
# - Measurements of effort or intensity of stressors are normalized by the max value across
# both study periods, or as presence absence of stressor footprint so all layers
# range from from 0-1 
# - Stressor layers are multiplied by the NBW vulnerability
# score assigned based on sensitivity, temporal frequency and extent of stressor
# (See Table 1) 
# - The impact of stressors are summed for each period to assess
# Cumulative impact of stressors pre and post 2004 
# - The difference in Cumulative impact pre and post 2004 periods highlights areas of change and
# intensity of differences in threats to NBW over time

#Copy Rasters to new folder to calc CHI----
path = here("Results/GRIDS/scaled/")
fromfiles <- dir(path=path,pattern=".tif", all.files = TRUE, full.names  = T)

dsn = here::here("Results/GRIDS/impact/Threats/")
save = rep(dsn, 6)

file.copy(from = fromfiles, to = paste0(save,basename(fromfiles), sep = ""), overwrite = T )

# #bring in all threat rasters as stacks------
files <- dir(path=dsn,pattern=".tif", full.names  = T)
Grid = lapply(files, raster)
Grid = stack(Grid)

####Join impact scores with datatable based on layer names----
#get vector of layer names
gridNames = (names(Grid))
layers  <- tibble(layer =gridNames)

## Impact Score Rationale Table  -------
ImpactScore = read.csv(here::here("Data/csvs/ImpactScoreRationale.csv"), stringsAsFactors = F)
ImpactScore = regex_left_join(layers,ImpactScore, c("layer" = "Layer"))

ImpactScore = ImpactScore%>%arrange(layer)%>%
  dplyr::select(Layer, Stressor, Frequency, Spatial.Extent, Impact, Sensitivity.Weight = `NBW.Sensitivity.Weight`)

### Impact Scores calc by Layer ---
#stack list of rasters as brick
Grid_impact = brick(Grid)

#Impact score calculation
Grid_impact = na.omit(Grid_impact*ImpactScore$Sensitivity.Weight)

# Grid_impact[Grid_impact ==0] <- NA
summary(Grid_impact)

#write each layer separately 
dsn = here::here("Results/GRIDS/impact/byLayer/")
writeRaster(stack(Grid_impact), filename = paste0(dsn,names(Grid_impact), sep = ""), bylayer=TRUE,format='GTiff', overwrite = T)

#threats-----
# rename layers
threats = c('OG Exploration', 'Fishing Effort', 'OG Operations', "Shipping Traffic", 'SST Anomalies','Military Exercises')
save = rep(threats, 2)
names(Grid_impact) = save

# as stars
pre = stack(Grid_impact[[c(7,9,8,10:12)]])
Impactpre_df = st_as_stars(pre)

#pre
plotThreats1 =  ggplot() +  
  scale_fill_viridis(direction = 1, na.value=NA)  +
  theme_bw()+
  geom_stars(data  = Impactpre_df, 
             na.rm = T)+
  facet_wrap(~band, ncol = 1, nrow = 6, drop = F, labeller = as_labeller(
    c(OG.Exploration.2 = "Oil & Gas Exploration", 
      OG.Operations.2 = "Oil & Gas Operations", 
      Fishing.Effort.2 = "Fishing Effort", 
      SST.Anomalies.2 = "SST Anomalies", 
      Military.Exercises.2 = "Military Exercises", 
      Shipping.Traffic.2 = "Shipping Traffic")) ) +
  geom_sf(
    data = nbw_habUTM,
    col = "black",
    fill = NA,
    size = .2
  ) +
  geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
    # add land region
  geom_sf(data = landUTM, color = NA, fill = "grey50") +
  labs(title = "1988-2004") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.6,
    bar_cols = c("grey40", "white")
  ) +
  # set map limits
  coord_sf(
    xlim = c(154000, 1518416),
    ylim = c(4481888, 5104112),
    crs =  st_crs(UTM20)
  ) +
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
  guides(fill = guide_colourbar(title = "Impact"),
         colour = guide_colourbar(title = "Impact")) 

# ggsave(here::here("Results/FIGS/plotThreats_pre.png"), plotThreats1, width = 8.5, height = 11, units = "in")

#post
post = stack(Grid_impact[[c(1,3, 2, 4:6)]])

Impactpost_df = st_as_stars(post)

plotThreats2 =  ggplot() +  
  scale_fill_viridis(direction = 1, na.value=NA)  +
  theme_bw()+
  geom_stars(data  = Impactpost_df, 
             na.rm = T)+
  facet_wrap(~band, ncol = 1, nrow = 6, drop = F, labeller = as_labeller(
    c(OG.Exploration.1 = "Oil & Gas Exploration", 
      OG.Operations.1 = "Oil & Gas Operations", 
      Fishing.Effort.1 = "Fishing Effort", 
      SST.Anomalies.1 = "SST Anomalies", 
      Military.Exercises.1 = "Military Exercises", 
      Shipping.Traffic.1 = "Shipping Traffic")) ) +
  geom_sf(
    data = nbw_habUTM,
    col = "black",
    fill = NA,
    size = .2
  ) +
  geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
  # add land region
  geom_sf(data = landUTM, color = NA, fill = "grey50") +
  labs(title = "2005 -2019") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.6,
    bar_cols = c("grey40", "white")
  ) +
  # set map limits
  coord_sf(
    xlim = c(154000, 1518416),
    ylim = c(4481888, 5104112),
    crs =  st_crs(UTM20)
  ) +
  # format axes
  ylab(NULL) +
  xlab("") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), legend.position="bottom"
  ) +
  guides(fill = guide_colourbar(title = "Impact"),
         colour = guide_colourbar(title = "Impact")) +
  # we set the left and right margins to 0 to remove 
  # unnecessary spacing in the final plot arrangement.
  theme(plot.margin = margin(6, 0, 6, -6), plot.subtitle  = element_text(hjust=0.5), plot.title  = element_text(hjust=0.5),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

# plot pre/post threats side by side------
# use patchwork 

p = plotThreats1 + plotThreats2 + plot_layout(guides = "collect")& theme(legend.position = 'bottom')

#save here
fig3threatspath = here::here("Results/FIGS/Fig3_threats.png")

ggsave(fig3threatspath, p, width = 8, height = 11.5, units = "in")

#plot togethr using cowplot
# # arrange the three plots in a single row
# prow <- cowplot::plot_grid(
#   plotThreats1 + theme(legend.position="none", plot.margin = margin(r = 0)), 
#   plotThreats2 + theme(legend.position="none", axis.text.y = element_blank(), axis.ticks.y = element_blank()),
#   rel_widths = c(1, 1), labels = c('A', 'B'), 
#   align = 'vh',
#    nrow = 1
# )
# # prow
# 
# # extract the legend from one of the plots
# legend <- get_legend(
#   # create some space to the left of the legend
#   plotThreats1 + theme(legend.box.margin = margin(0,0,0,12))
# )
# 
# # add the legend to the row we made earlier. Give it one-third of 
# # the width of one plot (via rel_widths).
# 
# # p = plot_grid(plotThreats1 + theme(legend.position="none", plot.margin = margin(r = 0)), 
# #               plotThreats2 + theme(legend.position="none", axis.text.y = element_blank(), 
# #                                    axis.ticks.y = element_blank(), plot.margin = margin(l = 3, r = 1)),
# #               legend, rel_widths = c(3, 2.5, .3), nrow = 1, align = 'h', axis = "t")
# 
# p = plot_grid(prow,
#               legend, rel_widths = c(3, .4), nrow = 1, align = 'v', axis = "t")



## Sum Cumulative impact of individual threats-----

### Pre 2004

sumImpactPre =sum(Grid_impact[[-c(1:6)]], na.rm = T) 
sumImpactPre[sumImpactPre ==0] <- NA
summary(sumImpactPre)
dsn = here::here("Results/GRIDS/impact/Cumulative/")

#write Raster SUM CHI for Pre period
writeRaster(sumImpactPre, filename = paste0(dsn, "sumImpactPre", sep = ""), overwrite = TRUE,format='GTiff')

### Post 2004
sumImpactpost = sum(Grid_impact[[c(1:6)]], na.rm = T) 
sumImpactpost[sumImpactpost ==0] <- NA
summary(sumImpactpost)
#write Raster SUM CHI for Post period
writeRaster(sumImpactpost, filename = paste0(dsn, "sumImpactPost", sep = ""), overwrite = TRUE,format='GTiff')

#plot total sum CHI Pre/Post

      # chiGrid = stack(sumImpactPre, sumImpactpost)
      # 
      # # as stars
      # sumCHI_df = st_as_stars(chiGrid)
      # 
      # # plot as facet
      # 
      # plotCHI_both =  ggplot() +  
      #   scale_fill_viridis(direction = 1, option = "C", na.value=NA)  +
      #   scale_color_manual(values = c("white", "blue"))+
      #   theme_bw()+
      #   geom_stars(data  = sumCHI_df, 
      #              na.rm = T)+
      #   facet_wrap(~band, ncol = 1, nrow = 2, drop = F, labeller = as_labeller(
      #     c(layer.1 = "1988-2004", 
      #       layer.2 = "2005-2019"))) +
      #   geom_sf(
      #     data = nbw_habUTM,
      #     col = "black",
      #     fill = NA,
      #     size = .2
      #   ) +
      #   geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
      #   # geom_sf(data = ACTIVE_MPAS, aes(col = period), fill = NA, size = .3)+
      #   
      #   # add land region
      #   geom_sf(data = landUTM, color = NA, fill = "grey50") +
      #   # add scale bar
      #   annotation_scale(
      #     location = "br",
      #     width_hint = 0.25,
      #     text_cex = 0.6,
      #     bar_cols = c("grey40", "white")
      #   ) +
      #   # set map limits
      #   coord_sf(
      #     xlim = c(154000, 1518416),
      #     ylim = c(4481888, 5104112),
      #     crs =  st_crs(UTM20),
      #     expand = FALSE
      #   ) +
      #   # format axes
      #   ylab("") +
      #   xlab("") +
      #   theme(
      #     panel.grid.major = element_blank(),
      #     panel.grid.minor = element_blank(),
      #     legend.position = "right"
      #   ) +
      #   # we set the left and right margins to 0 to remove 
      #   # unnecessary spacing in the final plot arrangement.
      #   theme(plot.margin = margin(6, 0, 6, 1))+
      #   guides(fill = guide_colourbar(title = "CHI"),
      #          colour = guide_colourbar(title = "CHI")) 
      # 
      # 
      # # #save here
      # fig4CHIpath = here::here("Results/FIGS/Fig4_CHI.png")
      # 
      # ggsave(fig4CHIpath, plotCHI_both, width = 8, height = 11, units = "in")


#CHI----------
#plot Pre CHI
sumImpactPre_df = st_as_stars(sumImpactPre)

PlotPre =  ggplot() +  
  scale_fill_viridis_b(direction = 1, na.value=NA, option = "C",limits = c(.01, 8 ),
                       breaks = c(1, 2, 3, 4,5,6,7))  +
  geom_stars(data = sumImpactPre_df,
             na.rm = T,
             downsample = 3) +
  geom_sf(
               data = nbw_habUTM,
               col = "black",
               fill = NA,
               size = .2
             ) +
  geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
  geom_sf(data = preMPA, col = "white",fill = NA, size = .3)+
  
  # add land region
  geom_sf(data = landUTM, color = NA, fill = "grey50") +
  theme(legend.position = "none") +
  labs(title = "1988-2004") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.6,
    bar_cols = c("grey40", "white")
  ) +
  # set map limits
  coord_sf(
    xlim = c(154000, 1518416),
    ylim = c(4481888, 5104112),
    crs =  st_crs(UTM20),
    expand = FALSE
  ) +
  # format axes
  ylab("") +
  xlab("") +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +theme_bw()+
  guides(fill = guide_colourbar(title = "CHI"),
         colour = guide_colourbar(title = "CHI")) 

# PlotPre

#plot Post CHI
sumImpactPost_df= st_as_stars(sumImpactpost)

PlotPost =  ggplot() +  
  scale_fill_viridis_b(direction = 1, na.value=NA, option = "C",limits = c(.01, 8 ),
                       breaks = c(1, 2, 3, 4,5,6,7))  +
  geom_stars(data = sumImpactPost_df,
             na.rm = T,
             downsample = 3) +geom_sf(
               data = nbw_habUTM,
               col = "black",
               fill = NA,
               size = .2
             ) +
  geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
  geom_sf(data = ACTIVE_MPAS, col = "white", fill = NA, size = .25)+
  
  # add land region
  geom_sf(data = landUTM, color = NA, fill = "grey50") +
  theme(legend.position = "none") +
  labs(title = "2005-2019") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.6,
    bar_cols = c("grey40", "white")
  ) +
  # set map limits
  coord_sf(
    xlim = c(154000, 1518416),
    ylim = c(4481888, 5104112),
    crs =  st_crs(UTM20),
    expand = FALSE
  ) +
  # format axes
  ylab("") +
  xlab("") +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +theme_bw()+
  guides(fill = guide_colourbar(title = "CHI"),
         colour = guide_colourbar(title = "CHI")) 
# # PlotPost
#plot CHI periods together --------
CHI = PlotPre / PlotPost + plot_layout(guides = "collect")+ plot_annotation(title = "Sum of Stressors - Cumulative Human Impacts", 
                                                                            theme = theme(plot.title = element_text(hjust=0.5)),
                                                                                            tag_levels = 'A')&
  theme(legend.position = 'bottom', plot.tag.position = c(0, 1))

#save here
fig4CHIpath = here::here("Results/FIGS/Fig4_CHI.png")

ggsave(fig4CHIpath, CHI, width = 8, height = 11.5, units = "in")

### Difference between pre and post periods-----
sumDif = sumImpactpost- sumImpactPre
summary(sumDif)
writeRaster(sumDif, filename = here::here("Results/GRIDS/impact/Cumulative/sumDif"), overwrite = TRUE,format='GTiff')

# Plot Absolute Difference------
# #as stars
      sumDif_df= st_as_stars(sumDif)

A = ggplot() +
  geom_stars(data = sumDif_df,
             na.rm = T,
             downsample = 3) +
  scale_fill_gradientn(  limits = c(-4,4), colours = rev(c("dark orange", "white", "purple")),
                         values = c(0, .4,.5, .6, 1), na.value=NA)+
  geom_sf(
    data = nbw_habUTM,
    col = "dark grey",
    fill = NA,
    size = .2
  ) +
  geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
  geom_sf(data = ACTIVE_MPAS, col = "black", fill = NA, size = .3)+
  
  # add land region
  geom_sf(data = landUTM, color = NA, fill = "grey50") +
  labs(subtitle = "Absolute Difference in CHI") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.6,
    bar_cols = c("grey40", "white")
  ) +
  # set map limits
  coord_sf(
    xlim = c(154000, 1518416),
    ylim = c(4481888, 5104112),
    crs =  st_crs(UTM20),
    expand = FALSE
  ) +
  # format axes
  ylab("") +
  xlab("") +
  
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  guides(fill = guide_colourbar(title = "CHI"),
         colour = guide_colourbar(title = "CHI"),
         line.type = guide_legend(title = NULL, nrow = 2)) +theme_bw() +
  theme(legend.position = "bottom") 

A
### RMSE difference-----
      # RMS error = Square Root(sum(e^2)/n) 
      # - A measure of the cell-by-cell difference between the two grids. 
      # - About 2/3 of all the cells will differ by less than the rmse.
      # - About 95% of all the cells will differ by less than twice the rmse.

      averageDif = cellStats(abs(sumDif), mean)
      RMSE <- function(x, y) { sqrt(mean((x - y)^2, na.rm = T)) } 
# Now use it:
x = RMSE(values(sumImpactpost), values(sumImpactPre) )

RMSError = x

# RMSE = print(paste("Root Mean Square Error (RMSE)", round(x, digits = 4), sep = " = "))

#look at > 1RMSE?

RMSE_grid = sumDif
RMSE_grid[RMSE_grid <x & RMSE_grid>=0] <- NA
RMSE_grid[RMSE_grid >-x & RMSE_grid<=0] <- NA

# Plot RMSE >1 Difference------
##as stars
RMSE_grid_df= st_as_stars(RMSE_grid)


B = ggplot() +
  geom_stars(data = RMSE_grid_df,
             na.rm = T,
             downsample = 3) +
  scale_fill_gradientn(  limits = c(-4, 4), colours = rev(c("dark orange", "white", "purple")),
                         values = c(0, .4,.5, .6, 1), na.value=NA)+
  geom_sf(
    data = nbw_habUTM,
    col = "dark grey",
    fill = NA,
    size = .2
  ) +
  geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
  geom_sf(data = ACTIVE_MPAS, col = "black",fill = NA, size = .3)+
  
  # add land region
  geom_sf(data = landUTM, color = NA, fill = "grey50") +
  labs(subtitle = "Root Mean Square Error Difference >1") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.6,
    bar_cols = c("grey40", "white")
  ) +
  # set map limits
  coord_sf(
    xlim = c(154000, 1518416),
    ylim = c(4481888, 5104112),
    crs =  st_crs(UTM20),
    expand = FALSE
  ) +
  # format axes
  ylab("") +
  xlab("") +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) +
  guides(fill = guide_colourbar(title = "CHI"),
         colour = guide_colourbar(title = "CHI")) +theme_bw() +
  theme(legend.position = "bottom") 

B

####Differences > 2 x RMSE-------
RMSE_grid2 = sumDif
RMSE_grid2[RMSE_grid2 <(2*x) & RMSE_grid2>=0] <- NA
RMSE_grid2[RMSE_grid2 >=-(2*x) & RMSE_grid2<=0] <- NA


#plot
RMSE_grid2_df= st_as_stars(RMSE_grid2)

# Plot RMSE Difference >2------
##as stars
RMSE_grid2_df= st_as_stars(RMSE_grid2)

C = ggplot() +
  geom_stars(data = RMSE_grid2_df,
             na.rm = T,
             downsample = 3) +
  scale_fill_gradientn(  limits = c(-4,4), colours = rev(c("dark orange", "white", "purple")),
                         values = c(0, .4,.5, .6, 1), na.value=NA)+
  geom_sf(
    data = nbw_habUTM,
    col = "dark grey",
    fill = NA,
    size = .2
  ) +
  geom_sf(data = bathyUTM, col = "gray", size = 0.2) +
  geom_sf(data = ACTIVE_MPAS, col = "black",fill = NA, size = .3)+
  
  # add land region
  geom_sf(data = landUTM, color = NA, fill = "grey50") +
  labs(subtitle = "Root Mean Square Error Difference >2") +
  # add scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.6,
    bar_cols = c("grey40", "white")
  ) +
  # set map limits
  coord_sf(
    xlim = c(154000, 1518416),
    ylim = c(4481888, 5104112),
    crs =  st_crs(UTM20),
    expand = FALSE
  ) +
  # format axes
  ylab("") +
  xlab("") +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_colourbar(title = "CHI"),
         colour = guide_colourbar(title = "CHI")) + theme_bw()+
  theme(legend.position = "bottom") 
C

# Plot together------
#for some reason patchwork requires '&' to make legend move to bottom postion

CHI_dif = A / B / C +plot_layout(guides = 'collect')+

  plot_annotation(title = 'Difference in Cumulative Human Impacts', tag_levels = 'A')& theme(legend.position = 'bottom')


#save here
fig5CHIpath = here::here("Results/FIGS/Fig5_CHIdif.png")

ggsave(fig5CHIpath, CHI_dif, width = 8, height = 11.5, units = "in")

