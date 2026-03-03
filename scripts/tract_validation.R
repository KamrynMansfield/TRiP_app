pacman::p_load(sf, tidyverse)

# load in custom functions
source("scripts/functions.R")

# load census tracts in as sf objects
load_tracts("data/census_tract_boundaries_NHGIS/nhgis0001_shape", years = 2024)

# make the census tracts valid
years <- 2024

for (year in years) {
  obj_name <- paste0("tract_", year)
  tracts <- get(obj_name)
  tracts_valid <- st_make_valid(tracts)
  assign(obj_name, tracts_valid)
}

# Identify where to save valid geometry files
save_folder <- "data/census_tract_boundaries_NHGIS/valid_geometry"

# create save folder if necessary
if (!dir.exists(save_folder)) {
  dir.create(save_folder, recursive = TRUE)
}

# save each "tract_year" file
for (year in years) {
  obj_name <- paste0("tract_", year)
  tracts <- get(obj_name)
  saveRDS(
    tracts, 
    file.path(save_folder, paste0(obj_name, ".rds"))
  )
}

# saveRDS(tract_2017, file.path(save_folder, "tract_2017.rds"))

