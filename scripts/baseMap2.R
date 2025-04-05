# ### 1. Import basemap data for NBW trends analysis 
# Northern Bottlenose Whale (NBW) Trends Analysis
# Author: [Name]
# Last updated: [2025]

# - This includes Coastlines, bathymetry, NBW habitat area, and conservation zones 
# - Code imports shapefile data and transforms to UTM Zone 20 where necessary
# - Bathymetric data from Atlas of Canada available at: (https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/framework_cadre/North_America_Atlas10M/bathymetry/)
# - Conservation zones compiled from layers available as shapefiles. 
# - Ocean's Act MPAs from https://open.canada.ca/data/en/dataset/a1e18963-25dd-4219-a33f-1a38c4971250  
#     - Northern bottlenose whale Critical Habitat designations from Species at Risk https://open.canada.ca/data/en/dataset/db177a8c-5d7d-49eb-8290-31e6a45d786c  
# 

# Load required packages ----
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  sf, terra, dplyr, ggplot2, ggrepel, stringr, 
  here, ggtext, readr, grid, pals, tidyr, fuzzyjoin, 
  patchwork, mapsf, classInt, ggforce, ggspatial, 
  lubridate, stars, scales, RColorBrewer, grafify
)

sf_use_s2(FALSE) # Disable spherical geometry

# Define constants ----
UTM20 <- terra::crs("+init=epsg:32620") # UTM Zone 20
INPUT_DIR <- here::here("inputs/shapes") # Create a consistent data directory reference
OUTPUT_DIR <- here::here("output/figs") # Create a consistent output directory reference

# Define study area boundaries ----
define_study_boundaries <- function() {
  # Full geographic extent
  GEO_BOUND <- st_bbox(c(xmin = -75, ymin = 38, xmax = -40, ymax = 60), crs = st_crs(4326)) %>%
    st_as_sfc() %>% st_sf()
  
  # Full extent in UTM coordinates
  UTM_BOUND <- st_bbox(GEO_BOUND) %>% st_as_sfc() %>% st_sf() %>% st_transform(UTM20)
  
  # Smaller extent for Scotian Shelf focus area
  SS_BOUND <- st_bbox(c(xmin = -70, ymin = 40, xmax = -48, ymax = 48), crs = st_crs(4326)) %>%
    st_as_sfc() %>% st_sf()
  
  SS_BOUND_UTM <- SS_BOUND %>% st_transform(UTM20)
  
  return(list(
    geo_bound = GEO_BOUND,
    utm_bound = UTM_BOUND,
    ss_bound = SS_BOUND,
    ss_bound_utm = SS_BOUND_UTM
  ))
}# Import basemap layers ----
import_regions <- function(shapefile_path, boundaries) {
  regions <- read_sf(shapefile_path) %>%
    st_transform(4326) %>% st_transform(UTM20)
  
  # Extract specific regions
  scotian_shelf <- regions %>% dplyr::filter(DFO_REGION == "MAR")
  gulf <- regions %>% dplyr::filter(DFO_REGION == "GULF")
  quebec <- regions %>% dplyr::filter(DFO_REGION == "QC")
  newfoundland <- regions %>% dplyr::filter(DFO_REGION == "NFLD")
  
  return(list(
    all = regions,
    scotian_shelf = scotian_shelf,
    gulf = gulf,
    quebec = quebec,
    newfoundland = newfoundland
  ))
}

import_land <- function(shapefile_path, boundaries) {
  land <- read_sf(shapefile_path) %>%
    dplyr::filter(CONTINENT == "North America")
  
  land_utm <- land %>% 
    st_transform(UTM20) %>% 
    st_intersection(boundaries$ss_bound_utm)
  
  return(land_utm)
}

import_bathymetry <- function(shapefile_path, boundaries) {
  bathy <- read_sf(shapefile_path) %>%
    st_transform(UTM20) %>% 
    st_intersection(boundaries$ss_bound_utm) %>%
    filter(DEPTH > 150)
  
  return(bathy)
}

# Import conservation areas ----
import_conservation_areas <- function(boundaries) {
  # MPAs
  mpas <- read_sf(here::here(INPUT_DIR, "MPAs/DFO_MPA_MPO_ZPM.shp")) %>%
    st_transform(4326) %>% 
    st_make_valid() %>% 
    st_intersection(boundaries$ss_bound) %>% 
    st_transform(UTM20) %>%
    mutate(NAME = NAME_E)
  
  # Gully MPA
  gully <- read_sf(here::here(INPUT_DIR, "MPAs/Gully_MPA.shp")) %>%
    st_transform(UTM20) %>%
    mutate(NAME = if_else(NAME == "Zone 1", "Zone 1", "Gully MPA"))
  
  gully_zone1 <- gully %>% filter(NAME == "Zone 1")
  
  return(list(
    mpas = mpas,
    gully = gully,
    gully_zone1 = gully_zone1
  ))
}

# Import NBW habitat data ----
import_nbw_data <- function(boundaries) {
  # Critical habitat
  nbw_ch <- read_sf(here::here(INPUT_DIR, "NBW_CH/NorthernBottlenoseWhale_CH.shp")) %>%
    st_transform(UTM20) %>%
    mutate(NAME = DESCRIP)
  
  # Important habitat
  nbw_habitat <- read_sf(here::here(INPUT_DIR, "ImpHabitat/Feyreretal2024/NBW_ImHab_edit.shp")) %>%
    st_transform(4326) %>% 
    st_intersection(boundaries$ss_bound) %>%
    st_transform(UTM20)
  
  # Sightings and detections
  bw_detects <- read_sf(here::here(INPUT_DIR, "Ha/BW.PAM_sf.shp")) %>%
    filter(species == "Ha") %>% 
    st_transform(UTM20)
  
  sightings <- read_sf(here::here(INPUT_DIR, "Ha/Ha_locations_2023.shp")) %>%
    st_transform(UTM20)
  
  return(list(
    critical_habitat = nbw_ch,
    important_habitat = nbw_habitat,
    detections = bw_detects,
    sightings = sightings
  ))
}

# Create NBW map ----
create_nbw_map <- function(land, bathy, conservation_areas, nbw_data, regions) {
  # Compile marine conservation areas
  mcas <- rbind(
    conservation_areas$gully %>% select(NAME), 
    nbw_data$critical_habitat %>% select(NAME)
  ) %>% 
    filter(NAME %in% c(
      "Zone 1", 
      "Haldimand Canyon", 
      "Shortland Canyon",
      "Gully MPA (Marine Protected Area), outer boundary"
    ))
  
  # Define map extent
  x_min <- 712179.56
  x_max <- 1009419.59
  y_min <- 4695205.74
  y_max <- 4993983.86
  
  # Create map
  map <- ggplot() + theme_bw() +
    # Important habitat
    geom_sf(data = nbw_data$important_habitat, 
            aes(col = "Important Habitat"), 
            fill = NA, alpha = .1, linewidth = 1) +
    
    # Land
    geom_sf(data = land, 
            aes(color = "Sable Island"), 
            fill = "#E1BF92", linewidth = .8) +
    
    # Bathymetry
    geom_sf(data = bathy, color = "gray") +
    
    # Gully MPA
    geom_sf(data = conservation_areas$gully %>% 
              filter(NAME == "Gully MPA (Marine Protected Area), outer boundary"), 
            aes(col = "The Gully"), fill = NA, linewidth = .8) +
    
    # Critical habitat
    geom_sf(data = nbw_data$critical_habitat, 
            aes(fill = "Critical Habitat"), 
            col = "black", alpha = .5, linewidth = 1) +
    
    # Sightings and detections
    geom_sf(data = nbw_data$sightings %>% st_intersection(regions$scotian_shelf), 
            colour = "#ff593d", fill = NA, size = 1.2, shape = 18) +
    geom_sf(data = nbw_data$detections %>% st_intersection(regions$scotian_shelf), 
            colour = "#93003a", fill = NA, size = 1, shape = 1) +
    
    # Set map extent
    coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE) +
    
    # Labels and theming
    ylab("") + xlab("") +
    theme(
      plot.margin = margin(.1, .5, .1, .1, "cm"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom", 
      legend.title = element_blank()
    ) +
    
    # Legend styling
    scale_fill_manual(
      values = c("Critical Habitat" = NA),
      name = "Layer Type"
    ) +
    scale_color_manual(
      values = c(
        "The Gully" = "Black", 
        "Sable Island" = "#9D8566", 
        "Important Habitat" = "#FFD300"
      ), 
      name = "Layer Type"
    ) +
    
    # Scale bar
    annotation_scale(
      location = "br", 
      width_hint = 0.25,
      text_cex = 0.75,
      bar_cols = c("grey40", "white")
    ) +
    
    # Canyon labels
    annotate(
      geom = "text", x = 832202.7, y = 4839000, 
      label = "The Gully MPA", fontface = "italic",
      color = "black", size = 3
    ) +
    annotate(
      geom = "text", x = 877340.6, y = 4873735, 
      label = "Shortland", fontface = "italic",
      color = "black", size = 3
    ) +
    annotate(
      geom = "text", x = 910303.4, y = 4887964, 
      label = "Haldimand", fontface = "italic",
      color = "black", size = 3
    )
  
  return(map)
}

# Create template raster for data processing ----
create_template_raster <- function(regions) {
  template <- rast(vect(regions$scotian_shelf), res = 1000)
  template_stars <- st_as_stars(template, as_points = FALSE, merge = FALSE)
  
  return(list(
    raster = template,
    stars = template_stars
  ))
}

  # Set up study area boundaries
  boundaries <- define_study_boundaries()
  
  # Import regional data
  regions <- import_regions(
    here::here(INPUT_DIR, "DFO/DFO_NAFO_EEZ_Land.shp"), 
    boundaries
  )
  
  # Import base layers
  land <- import_land(
    here::here(INPUT_DIR, "worldcountries/ne_50m_admin_0_countries.shp"), 
    boundaries
  )
  
  bathy <- import_bathymetry(
    here::here(INPUT_DIR, "bathymetry_pl_v2/bathymetry_l_v2.shp"), 
    boundaries
  )
  
  # Import conservation areas
  conservation_areas <- import_conservation_areas(boundaries)
  
  # Import NBW data
  nbw_data <- import_nbw_data(boundaries)
  
  # Create map
  nbw_map <- create_nbw_map(land, bathy, conservation_areas, nbw_data, regions)
  
  # Create template raster
  templates <- create_template_raster(regions)
  
 
# Save map to disk but also keep it available for display
gg_Fig2path <- here::here(OUTPUT_DIR, "Fig_1b.png")
ggsave(gg_Fig2path, nbw_map, height = 7, width = 7, units = "in", dpi = 300)

# Also display the map
print(nbw_map)

# Also create the template for later use in the global environment
template <- templates$raster
template_s <- templates$stars