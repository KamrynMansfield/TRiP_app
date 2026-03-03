# load packages
pacman::p_load(tidytransit, sf, dplyr, units, lwgeom)

# load in custom functions
source("scripts/functions.R")

# read in gtfs
gtfs <- read_gtfs("data/Nashville/GTFS/2024-12-16.zip")

# get route and shape id combinations
route_list <- gtfs$trips %>% 
  select(route_id, shape_id) %>% 
  distinct()

# convert gtfs to sf lines with route id
shapes_routes <- shapes_as_sf(gtfs$shapes) %>% 
  left_join(route_list, by = "shape_id")

# group shapes by route id and combine shapes
comb_shapes_routes <- shapes_routes %>% 
  group_by(route_id) %>% 
  summarise(do_union = TRUE)

# visualize combined routes
library(leaflet)
library(viridis)
pal <- colorFactor(viridis(50), domain = comb_shapes_routes$route_id)

route_labels <- comb_shapes_routes %>%
  group_by(route_id) %>%
  summarize(geometry = st_centroid(st_union(geometry)))

library(leaflet)

leaflet(comb_shapes_routes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # Draw routes
  addPolylines(
    color = ~pal(route_id),
    weight = 2,
    opacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 4,
      color = "red",
      bringToFront = TRUE
    )
  ) %>%
  
  # Add always-visible labels
  addLabelOnlyMarkers(
    data = route_labels,
    label = ~route_id,
    labelOptions = labelOptions(
      noHide = TRUE,          # 👈 always show labels
      direction = "auto",
      textOnly = TRUE,
      style = list(
        "color" = "black",
        "font-size" = "12px",
        "font-weight" = "bold",
        "background-color" = "rgba(255,255,255,0.7)",
        "padding" = "2px"
      )
    )
  )


# leaflet(comb_shapes_routes) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolylines(
#     color = ~pal(route_id),
#     weight = 2,
#     opacity = 0.7,
#     label = ~route_id,
#     highlightOptions = highlightOptions(
#       weight = 4,
#       color = "red",
#       bringToFront = TRUE
#     )
#   )

# # There is a function in zoneR package that gets state plane crs if we want
# # devtools::install_github("vibe-lab-gsd/zoneR")
# library(zoneR)
# crs <- zr_get_crs(comb_shapes_routes)

crs <- 32616

# create a 1/4 mile buffer around each route
shapes_buffered <- comb_shapes_routes %>%
  st_transform(crs) %>%  # UTM Zone 16N for Nashville
  st_buffer(dist = set_units(402.336, "m")) %>%
  st_transform(4326)  # Back to lat/lon

# double check that the buffers worked
library(ggplot2)

ggplot() +
  geom_sf(data = shapes_buffered, fill = "lightblue", alpha = 0.4) +
  geom_sf(data = comb_shapes_routes, color = "blue", size = 1) +
  theme_minimal()

# make the buffers valid geometry
shapes_buffered <- st_make_valid(shapes_buffered)


############################################################
#' This section intersects the route buffers that were 
#' created from the 2024 tracts with the validated census
#' tract boundaries from individual years (e.g., 2020, 2021,
#' 2022, 2023). This is done to have one unified route 
#' buffer, but to accommodate different census tract
#' configurations in each set of ACS data. 
############################################################

# load in validated tracts by year (e.g., 2020 or 2021 or 2022 or 2023)
save_folder = "data/census_tract_boundaries_NHGIS/valid_geometry"

years <- 2024

for (year in years) {
  obj_name <- paste0("tract_", year)
  file_path <- file.path(save_folder, paste0(obj_name, ".rds"))
  
  assign(obj_name, readRDS(file_path))
}

# NEED TO MAKE THIS FLEXIBLE BY YEAR!!!!!!!!!!!!!!!!
tracts <- tract_2024 %>% 
  select(GEOID, geometry)

# use planar geometry to avoid errors
sf::sf_use_s2(FALSE)

# Assume your buffered area is called `buffered` and tracts are in `tracts`
# Make sure both are on the same coordinate system
tracts <- st_transform(tracts, crs=4326)
shapes_buffered <- st_transform(shapes_buffered, crs=4326)

# Get indexes of intersecting tracts
intersections <- st_intersects(tracts, shapes_buffered)

# Keep only those that intersect
tracts_touching <- tracts[lengths(intersections) > 0, ]

# check filtered tracts
library(mapview)
mapview(tracts_touching, color = "red", lwd = 1)

# Reproject to a projected CRS (e.g., NAD83 / Conus Albers)
target_crs <- 5070  # EPSG:5070 (USA Contiguous Albers Equal Area)
tracts_touching <- st_transform(tracts_touching, target_crs)
shapes_buffered <- st_transform(shapes_buffered, target_crs)

# work more on this section next
# Perform intersection between tracts and buffered shapes
intersections <- st_intersection(tracts_touching, shapes_buffered)

# Area of each intersected piece (tract + buffer overlap)
intersections$intersect_area <- st_area(intersections)

# Area of each buffer segment
shapes_buffered$buffer_area <- st_area(shapes_buffered)

# Join buffer area by route ID
intersections <- intersections %>%
  left_join(
    st_drop_geometry(shapes_buffered) %>% select(route_id, buffer_area),
    by = c("route_id")
  )

# calculate % of each route's buffer in each tract
intersections$percent_of_buffer <- as.numeric(
  intersections$intersect_area / intersections$buffer_area
)

# check the math: make sure the sum of the percent adds to 1
intersections %>%
  group_by(route_id) %>%
  summarise(total_percent = sum(percent_of_buffer, na.rm = TRUE)) %>%
  arrange(desc(abs(total_percent - 1))) %>%
  mutate(flag = abs(total_percent - 1) > 0.01) %>% 
  print(n = 37)

# save the intersections dataset
#' This dataset contains each route buffer number and all of
#' the 2020 (or specified year) census tracts that intersect 
#' with that route. It also has the percent of the buffer 
#' that a particular census tract represents, which means
#' we can create aggregate census tract information for each
#' route buffer in the acs_tracts_regression.R file.
saveRDS(intersections, "data/Nashville/output/intersecting_tracts_Nashville_2024.rds")


