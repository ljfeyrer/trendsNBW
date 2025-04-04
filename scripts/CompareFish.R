#############################################################
# Regional Fishing Effort Analysis: Pre-2005 vs. Post-2005
# Analysis of spatial fishing patterns on the Scotian Shelf
#Laura Feyrer 2025
#############################################################


# Required scripts basemap and functionsCHI + objects - 
    # template ( raster template)
    # SShelf ( Scotian Shelf mask)
    # UTM20 ( coordinate reference system)
    # nbw_ImHab_UTM, bathy, landUTM ( spatial layers for plotting)
    # x_min, x_max, y_min, y_max ( map extent)


# Create output directories
dirs <- list(
  pre_fish = file.path("output", "GRIDS", "PRE", "Fish"),
  post_fish = file.path("output", "GRIDS", "POST", "Fish"),
  chi = file.path("output", "GRIDS", "CHI")
)

# Create directories if they don't exist
for (dir in dirs) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

#############################################################
# Helper Functions
#############################################################

# Function to calculate deciles for effort data
quantEffort <- function(effort, writepath) {
  # Get all data values
  effort_values <- values(effort)
  # set 0 to NA
  effort_values[effort_values == 0] <-NA
  # Calculate quantile breaks
  probs <- seq(0, 1, 0.1)
  quantiles <- quantile(effort_values, probs = probs, na.rm = TRUE)
  
  # Cut the values into quantile classes
  effort_binned_values <- .bincode(effort_values, breaks = quantiles, include.lowest = T)
  
  # Create a new raster with these binned values
  Eff_quant_rast <- effort
  values(Eff_quant_rast) <- effort_binned_values
  
  # Scale to 0-100% by multiplying by 10 and write to file
  writeRaster(
    Eff_quant_rast * 10, 
    filename = file.path(writepath, paste0(names(effort), "_deciles.tif")), 
    overwrite = TRUE
  )
  
  # Return the scaled raster
  return(Eff_quant_rast * 10)
}

invert_raster <- function(r) {
  max_val <- global(r, "max", na.rm = TRUE)[[1]]
  return(max_val - r)
}



# Standard plotting function for fishing effort maps
plot_fishing_effort <- function(stars_data, title, direction = -1) {
  ggplot() +
    geom_stars(data = stars_data, na.rm = TRUE) +
    scale_fill_continuous(
      type = "viridis", 
      option = "A",
      direction = direction,
      na.value = "transparent",
      limits = c(0, 100)
    ) +
    geom_sf(
      data = nbw_ImHab_UTM,
      col = "black",
      fill = NA,
      size = 0.2
    ) +
    geom_sf(data = bathy, col = "gray", size = 0.2) +
    geom_sf(data = landUTM, color = NA, fill = "grey50") +
    annotation_scale(
      location = "br",
      width_hint = 0.25,
      text_cex = 0.6,
      bar_cols = c("grey40", "white")
    ) +
    coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE) +
    ylab("") + xlab("") + 
    theme_bw() +
    theme(
      plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
      plot.title = element_text(margin = margin(b = 4)),  # light buffer under titles
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    labs(title = title) +
    guides(fill = guide_colourbar(title = "Effort %"))
}

#############################################################
# Data Processing Workflow----
#############################################################

# === 1. Pelagic Longline Fishing Effort -----

process_pelagic_longline <- function() {
  # PRE Period (1999-2003)
  Pel_long_fish <- vect("shapes/Fishing_Effort/PRE/PLL/pelagics_all.shp")
  Pel_long_fish <- project(Pel_long_fish[, 6:6], crs(UTM20))
  Pel_long_fish <- rasterize(Pel_long_fish, template, field = "lg_pelagic", fun = "max")
  
  # Project and mask
  Pel_long_fish <- terra::project(Pel_long_fish, template, method = 'near')
  Pel_long_fish <- mask(Pel_long_fish, SShelf)
  names(Pel_long_fish) <- "prePelagicFish"
  
  # Calculate deciles and save
  quantEffort(Pel_long_fish, writepath = dirs$pre_fish)
  PLL_deciles_pre <- rast(file.path(dirs$pre_fish, "prePelagicFish_deciles.tif"))
  
  # POST Period (2005-2021)
  Pel_fish_POST <- rast(here::here("shapes/Fishing_Effort/POST/SS_Combined_Percentiles_Pelagic_2005-2021.tif"))
  Pel_fish_POST <- terra::project(Pel_fish_POST, template, method = 'near')
  Pel_fish_POST <- mask(Pel_fish_POST, SShelf)
  Pel_fish_POST <- invert_raster(Pel_fish_POST)
  names(Pel_fish_POST) <- "postPelFish"
  
  # Calculate inverted deciles and save
  quantEffort(Pel_fish_POST, writepath = dirs$post_fish)
  PLL_deciles_post <- rast(file.path(dirs$post_fish, "postPelFish_deciles.tif"))
  
  # Generate plots
  m_pre <- plot_fishing_effort(
    st_as_stars(PLL_deciles_pre), 
    "Pelagic Longline",
    direction = -1
  )
  
  m_post <- plot_fishing_effort(
    st_as_stars(PLL_deciles_post), 
    "",
    direction = -1
  )
  
  return(list(
    pre_raster = PLL_deciles_pre,
    post_raster = PLL_deciles_post,
    pre_plot = m_pre,
    post_plot = m_post
  ))
}

# === 2. Bottom Longline Fishing Effort ----

process_bottom_longline <- function() {
  # PRE Period (1999-2003)
  BLL_pre <- vect("shapes/Fishing_Effort/PRE/gf_trawl/groundfish_stats.shp")
  BLL_pre <- project(BLL_pre[, 20:20], crs(UTM20))
  BLL_pre_rast <- rasterize(BLL_pre, template, field = "LongLCatch", fun = "max")
  
  # Project and mask
  BLL_pre_rast <- terra::project(BLL_pre_rast, template, method = 'near')
  BLL_pre_rast <- mask(BLL_pre_rast, SShelf)
  names(BLL_pre_rast) <- "preBLL"
  
  # Calculate deciles and save
  quantEffort(BLL_pre_rast, writepath = dirs$pre_fish)
  BLL_pre_decile <- rast(file.path(dirs$pre_fish, "preBLL_deciles.tif"))
  
  # POST Period (2005-2021)
  bot_long_fish_POST <- rast(here::here("shapes/Fishing_Effort/POST/SS_Combined_Percentiles_Groundfish_Fixed_2005-2021.tif"))
  bot_long_fish_POST <- terra::project(bot_long_fish_POST, template, method = 'near')
  bot_long_fish_POST <- mask(bot_long_fish_POST, SShelf)
  bot_long_fish_POST <- invert_raster(bot_long_fish_POST)
  
  names(bot_long_fish_POST) <- "postFixedFish"
  
  # Calculate inverted deciles and save
  quantEffort(bot_long_fish_POST, writepath = dirs$post_fish)
  BLL_decile_post <- rast(file.path(dirs$post_fish, "postFixedFish_deciles.tif"))
  
  # Generate plots
  m_pre <- plot_fishing_effort(
    st_as_stars(BLL_pre_decile), 
    "Bottom Longline",
    direction = -1
  )
  
  m_post <- plot_fishing_effort(
    st_as_stars(BLL_decile_post), 
    "",
    direction = -1
  )
  
  return(list(
    pre_raster = BLL_pre_decile,
    post_raster = BLL_decile_post,
    pre_plot = m_pre,
    post_plot = m_post
  ))
}

# === 3. Groundfish Mobile Fishing Effort ----

process_groundfish_mobile <- function() {
  # PRE Period (1999-2003)
  mobile_gf_pre <- vect("shapes/Fishing_Effort/PRE/gf_trawl/groundfish_stats.shp")
  mobile_gf_pre <- project(mobile_gf_pre[, 21:21], crs(UTM20))
  mobile_pre_rast <- rasterize(mobile_gf_pre, template, field = "TrawlCatch", fun = "max")
  
  # Project and mask
  mobile_gf_pre <- terra::project(mobile_pre_rast, template, method = 'near')
  mobile_gf_pre <- mask(mobile_gf_pre, SShelf)
  names(mobile_gf_pre) <- "preMobileGF"
  
  # Calculate deciles and save
  quantEffort(mobile_gf_pre, writepath = dirs$pre_fish)
  preMobileGF_decile <- rast(file.path(dirs$pre_fish, "preMobileGF_deciles.tif"))
  
  # POST Period (2005-2021)
  GFmobile_POST <- rast(here::here("shapes/Fishing_Effort/POST/SS_Combined_Percentiles_Groundfish_Mobile_2005-2021.tif"))
  GFmobile_POST <- terra::project(GFmobile_POST, template, method = 'near')
  GFmobile_POST <- mask(GFmobile_POST, SShelf)
  GFmobile_POST <- invert_raster(GFmobile_POST)
  names(GFmobile_POST) <- "postGFmobileFish"
  
  # Calculate inverted deciles and save
  quantEffort(GFmobile_POST, writepath = dirs$post_fish)
  GFmobile_POST_decile <- rast(file.path(dirs$post_fish, "postGFmobileFish_deciles.tif"))
  
  # Generate plots
  m_pre <- plot_fishing_effort(
    st_as_stars(preMobileGF_decile), 
    "Groundfish Mobile",
    direction = -1
  )
  
  m_post <- plot_fishing_effort(
    st_as_stars(GFmobile_POST_decile), 
    "",
    direction = -1
  )
  
  return(list(
    pre_raster = preMobileGF_decile,
    post_raster = GFmobile_POST_decile,
    pre_plot = m_pre,
    post_plot = m_post
  ))
}

# === 4. Combine All Fishing Effort Layers ===

combine_fishing_efforts <- function() {
  # Combine PRE fishing effort layers
  pre_files <- list.files(path = dirs$pre_fish, pattern = "*_deciles.tif$", full.names = TRUE)
  pre_rasters <- rast(pre_files)
  
  # Sum all types and rescale to percentage (divide by number of layers)
  pre_fish_combined <- sum(pre_rasters, na.rm = TRUE)
  names(pre_fish_combined) <- "pre_fish"
  pre_fish_combined <- pre_fish_combined / nlyr(pre_rasters)
  # print(paste("Min value:", min(values(pre_fish_combined), na.rm=TRUE), 
  #             "Max value:", max(values(pre_fish_combined), na.rm=TRUE)))
  # Write combined layer for CHI calculation
  writeRaster(
    pre_fish_combined, 
    filename = file.path(dirs$chi, "pre_fish.tif"),
    overwrite = TRUE
  )
  
  # Combine POST fishing effort layers
  post_files <- list.files(path = dirs$post_fish, pattern = "*_deciles.tif$", full.names = TRUE)
  post_rasters <- rast(post_files)
  
  # Sum all types and rescale to percentage
  post_fish_combined <- sum(post_rasters, na.rm = TRUE)
  names(post_fish_combined) <- "post_fish"
  post_fish_combined <-post_fish_combined / nlyr(post_rasters)
  
  # Optional: Mask Gully Z1 from post-2004 fish effort
  # post_fish_combined <- mask(post_fish_combined, GullyZ1, inverse = TRUE, updatevalue = 0)
  
  # Write combined layer for CHI calculation
  writeRaster(
    post_fish_combined, 
    filename = file.path(dirs$chi, "post_fish.tif"),
    overwrite = TRUE
  )
  
  return(list(
    pre_combined = pre_fish_combined,
    post_combined = post_fish_combined
  ))
}

#############################################################
# Main Execution-----
#############################################################

main <- function() {
  # Process all fishing effort types
  pelagic_results <- process_pelagic_longline()
  bottom_results <- process_bottom_longline()
  mobile_results <- process_groundfish_mobile()
  
  # Combine all fishing effort layers
  combined_results <- combine_fishing_efforts()
  
  # Arrange plots in a grid for publication
  combined_plots <- list(
    pelagic_results$pre_plot, pelagic_results$post_plot,
    bottom_results$pre_plot, bottom_results$post_plot,
    mobile_results$pre_plot, mobile_results$post_plot
  )
  
  # Save plots if needed
  final_plot <- patchwork::wrap_plots(combined_plots, ncol = 2, guides = "collect") +
    patchwork::plot_layout(axes = "collect")+
    theme(legend.position = "right")+ 
    plot_annotation(title = "Differences in Historical and Contemporary Fishing Effort")# requires patchwork package
  ggsave(
    filename = file.path("output", "plots", "all_fishing_efforts.png"),
    plot = final_plot,
    width = 12, height = 18
  )
  print(final_plot)
  # Calculate difference between post and pre periods if needed
  fishing_difference <- combined_results$post_combined - combined_results$pre_combined
  names(fishing_difference) <- "fishing_difference"
  
  return(combined_results)
}

# Run the analysis
results <- main()

