# Compare change in intensity of human activities ----
# - Input layers summarize effort or footprint associated with known stressors
# associated with threats to NBW in the pre and post MPA periods 
# - Threat intensity is standardized using decile % classes of underlying data values
# or as presence absence of threat footprint relative range is from from 0-1 
# - All threats considered are assessed as serious for NBW, 
# all stressors are assessed as having intensity impacts on the same scale
# - The impact of stressors are summed for each period to assess differences in intensity pre and post 2004 periods,
# highlighting areas of change and differences in NBW habitat over time

pacman::p_load(terra, stars,sf,ggplot2, patchwork, scales,ggspatial, dplyr, here)

#Read in Rasters to calc CHI----
CHI_path = here("output/GRIDS/CHI//")
pre_threats <- rast(dir(path=CHI_path,pattern="pre", full.names  = T))
post_threats <- rast(dir(path=CHI_path,pattern="post", full.names  = T))

# Create common visualization parameters ----
# Map extent parameters
map_extent <- list(
   xlim = nbw_data$xlim,
   ylim = nbw_data$ylim
)

# Common theme elements
theme_threats <- theme_bw() +
   theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
   )

# Color palette without black
intensity_pal <- viridis_pal(option = "A", direction = -1)(11)[1:10]

# Threat type labels
threat_labels <- list(
   pre = c(
      pre_fish = "Fishing Effort", 
      pre_MFAS = "Military Activity Areas",
      pre_oil = "Oil & Gas Operations", 
      pre_Ship = "Shipping Intensity"
   ),
   post = c(
      post_fish = "Fishing Effort", 
      post_MFAS = "Military Activity Areas",
      post_oil = "Oil & Gas Operations", 
      post_Ship = "Shipping Intensity"
   ),
   diff = c(
      dif_fish = "Fishing Effort", 
      dif_MFAS = "Military Activity Areas",
      dif_OG = "Oil & Gas Operations", 
      dif_Ship = "Shipping Intensity"
   )
)

# Function to add common map features ----
add_map_features <- function(plot, land_fill = "grey50", nbw_color = "black", 
                             gully_color = "blue", ch_color = "blue") {
   plot +
      geom_sf(data = nbw_data$important_habitat, col = nbw_color, fill = NA, size = 0.2) +
      geom_sf(data = conservation_areas$gully, col = gully_color, fill = NA, size = 1) +
      geom_sf(data = nbw_data$critical_habitat, col = ch_color, fill = NA, size = 1) +
      geom_sf(data = land, color = NA, fill = land_fill) +
      coord_sf(xlim = map_extent$xlim, ylim = map_extent$ylim, expand = FALSE) +
      ylab("") + xlab("")
}

# Pre individual threats facet maps ----
# Convert to stars object
pre_threats_stars <- st_as_stars(pre_threats)

# Plot pre-period threats
plotThreats_pre <- ggplot() +  
   scale_fill_gradientn(colors = intensity_pal, limits = c(0, 100),
                        na.value = "transparent") +  
   theme_threats +
   geom_sf(data = bathy, col = "gray", size = 0.2) +
   geom_stars(data = pre_threats_stars, alpha = 0.95, na.rm = TRUE) +
   labs(title = "Early Period") +
   facet_wrap(~attributes, drop = FALSE, 
              labeller = as_labeller(threat_labels$pre), ncol = 1) +
   theme(
      panel.spacing.x = unit(4, "mm"),
      plot.margin = margin(0, 0.5, 0, 0, "pt")
   ) +
   guides(fill = guide_colourbar(title = "Relative\nIntensity", direction = "vertical"))

# Apply common map features
plotThreats_pre <- add_map_features(plotThreats_pre)

plotThreats_pre
# Save pre-threats plot
ggsave(here::here("output/figs/plotThreats_pre.png"), plotThreats_pre, dpi = 300)
 
# Post individual threats facet map ----
# Convert to stars object
post_threats_stars <- st_as_stars(post_threats)

# Plot post-period threats 
plotThreats_post <- ggplot() +  
   scale_fill_gradientn(colors = intensity_pal, limits = c(0, 100),
                        na.value = "transparent") +  
   theme_threats +
   geom_sf(data = bathy, col = "gray", size = 0.2) +
   geom_stars(data = post_threats_stars, alpha = 0.95, na.rm = TRUE) +
   labs(title = "Contemporary Period") +
   facet_wrap(~attributes, drop = FALSE, ncol = 1,
              labeller = as_labeller(threat_labels$post)) +
   theme(
      panel.spacing.x = unit(4, "mm"),
      plot.margin = margin(0, 0, 0, 0, "pt"),
      axis.ticks.y = element_blank(), 
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
   ) +
   guides(fill = guide_colourbar(title = "Relative\nIntensity"))

# Apply common map features
plotThreats_post <- add_map_features(plotThreats_post)

# Save post-threats plot
ggsave(here::here("output/figs/plotThreats_post.png"), plotThreats_post, dpi = 300)

# Combine pre and post threat plots
threats_combined <- (plotThreats_pre + theme(plot.margin = unit(c(0, -30, 0, 0), "pt"))) + 
   plotThreats_post +
   plot_layout(guides = 'collect', axes = 'collect', ncol = 2) + 
   plot_annotation(tag_levels = 'A') &
   theme(legend.position = "right")

threats_combined
# Save combined threats plot
ggsave(here::here("output/figs/plotThreats_combined.png"), threats_combined, dpi = 300, width = 10, height = 12)

 
######
 
## Calculate difference between pre and post periods ----
# Create function to calculate differences handling NAs appropriately
calculate_difference <- function(post_raster, pre_raster) {
   # Create temporary copies for manipulation of NA behavior
   post_temp <- post_raster
   pre_temp <- pre_raster
   
   # Get a mask of where each has NA values
   post_na <- is.na(post_raster)
   pre_na <- is.na(pre_raster)
   both_na <- post_na & pre_na
   
   # Set NAs to 0 temporarily for calculation
   post_temp[post_na] <- 0
   pre_temp[pre_na] <- 0
   
   # Calculate difference
   diff <- post_temp - pre_temp
   
   # Restore NAs where both inputs had NAs
   diff[both_na] <- NA
   
   return(diff)
}

# Create a modified version of the post-fishing raster
# Extract just the fishing layer
post_fish <- post_threats[["post_fish"]]

# Convert polygon to a spatial vector that terra can use
gully_z1_vect <- vect(conservation_areas$gully_zone1)

# Set values inside Zone 1 to 0 in the post-fishing raster
# This simulates reduced fishing effort in Zone 1 after MPA designation

post_fish_masked <- mask(post_fish, gully_z1_vect, inverse=TRUE, updatevalue=0)

# Replace the post-fishing layer with the masked version
post_threats[["post_fish"]] <- post_fish_masked

# plot(pre_threats[["pre_fish"]] )
# plot(post_threats[["post_fish"]] )

# Calculate difference
diff_threats <- calculate_difference(post_threats, pre_threats)
names(diff_threats) = c(
   "dif_fish",
   "dif_MFAS" ,
   "dif_OG", 
   "dif_Ship" )
   
# Save difference rasters
diff_path <- here("output/GRIDS/DIF/")
writeRaster(diff_threats, 
            filename = c(paste0(diff_path, "dif_fish.tif"),
                         paste0(diff_path, "dif_MFAS.tif"),
                         paste0(diff_path, "dif_OG.tif"),
                         paste0(diff_path, "dif_Ship.tif")),
            filetype = "GTiff", 
            overwrite = TRUE)

# Read in difference rasters (to ensure consistency with saved files)
diff_threats <- rast(dir(path = diff_path, pattern = "dif", full.names = TRUE))



# Convert to stars object
diff_threats_stars <- st_as_stars(diff_threats)

# Create custom color gradient for difference plot
diff_colors <- rev(c("#ff5400", "#ff6d00", "#ff8500", "#ff9e00", 
                     "darkgrey", 
                     "#00b4d8", "#0096c7", "#0077b6", "#023e8a"))

diff_values <- c(-100, -50, -25, -9, 0, 9, 25, 50, 100)

# Create difference plot with a single scale bar
diff_threats_plot <- ggplot() +  
   scale_fill_gradientn(
      colours = diff_colors, 
      limits = c(-100, 100), 
      values = scales::rescale(diff_values),
      na.value = "transparent",
      breaks = c(-100, -50,  0, 50, 100),  # ensures a tick at 0
      labels = c("-100", "-50", "None", "50", "100")
   ) +
   theme_threats +
   geom_stars(data = diff_threats_stars, na.rm = TRUE) +
   # Add a single scale bar to the bottom-right panel only
   annotation_scale(
      location = "br",
      text_cex = 0.6,
      bar_cols = c("grey40", "white"),
      # This is the key change - only add scale to one panel
      data = data.frame(attributes = "dif_OG") 
   ) +
   facet_wrap(~attributes, drop = FALSE, 
              labeller = as_labeller(threat_labels$diff)) +
   theme(
      legend.position = "bottom",
      plot.margin = margin(0, 0, 0, 0)
   ) +
   guides(fill = guide_colourbar(
      title = "Intensity Difference",
      barwidth = 15, 
      barheight = 0.5, title.position = "top" 
   ))

# Apply map features with custom colors for difference plot
diff_threats_plot <- diff_threats_plot +
   geom_sf(data = nbw_data$important_habitat, col = "#FFD300", fill = NA, linewidth = 0.5) +
   geom_sf(data = conservation_areas$gully, col = "black", fill = NA, linewidth = 0.55) +
   geom_sf(data = nbw_data$critical_habitat %>% filter(DESCRIP != "The Gully"), 
           col = "black", fill = NA, linewidth = 0.75) +
   geom_sf(data = land, color = "darkgray", fill = "#9D8566") +
   coord_sf(xlim = map_extent$xlim, ylim = map_extent$ylim, expand = FALSE) +
   ylab("") + xlab("")

diff_threats_plot

# Save difference plot
ggsave(here::here("output/figs/Fig3_Difs.png"), diff_threats_plot, dpi = 300, width = 10, height = 8)
