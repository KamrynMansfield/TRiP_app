# adds time var and pivots longer by route for input data
timepivot <- function(datasets){
  for(name in datasets) {
    data <- get(name) %>% 
      mutate(time = 1:n()) %>% 
      pivot_longer(
        cols = -c(year, month, time),
        names_to = "route",
        values_to = name
        )
    
    assign(name, data, envir = .GlobalEnv)
  }
}


# pulls the fixed effects coefficients
get_fe <- function(model, fe_name) {
  all_fe <- fixef(model)
  fe <- all_fe[[fe_name]]
  group_names <- names(fe)
  df <- data.frame( # create df with dynamic column names
    group = group_names,
    effect = as.numeric(fe),
    row.names = NULL)
  
  return(df)
  }


# count number of weekdays in a given month/year combo
count_weekdays <- function(df, year_col = "year", month_col = "month") {
  # Ensure necessary columns exist
  if (!all(c(year_col, month_col) %in% names(df))) {
    stop("Both specified year and month columns must exist in the dataframe.")
  }
  
  # Use mapply to compute weekdays for each row
  df$weekdays_in_month <- mapply(function(y, m) {
    # First and last day of the month
    first_day <- as.Date(sprintf("%04d-%02d-01", y, m))
    last_day <- as.Date(format(seq(first_day, length = 2, by = "1 month")[2] - 1, "%Y-%m-%d"))
    
    # Generate all dates in the month
    all_days <- seq(first_day, last_day, by = "day")
    
    # Count weekdays (Monday = 1, ..., Sunday = 7)
    sum(!(weekdays(all_days, abbreviate = FALSE) %in% c("Saturday", "Sunday")))
  }, df[[year_col]], df[[month_col]])
  
  return(df)
}


# load in census data
load_tracts <- function(base_path, years = 2017:2023){
  for(year in years){
    
    message("Loading Census Tracts for ", year, "...")
    
    # create path to shapefile
    shp_path <- file.path(base_path, paste0(
      "/US_tract_", year, "/US_tract_", year, ".shp"))
    
    if (!file.exists(shp_path)) {
      warning("Shapefile for ", year, " not found at ", shp_path)
      next # Skip to next year
    }
    
    # read in tract and transform to WGS 1984
    tract <- st_read(shp_path, quiet=TRUE)
    
    message(year, " shapefile found and loaded")
    
    tract <- tract %>% 
      st_transform(4326)  # In WGS 1984 lat/long
    
    message(year, " shapefile transformed to WGS 1984")
    
    # create full version of tract information
    assign(paste0("tract_", year), tract, envir = .GlobalEnv)
    
    # create simplified version of tract information
    us_tract <- tract %>% 
      select(GEOID, geometry)
    
    assign(paste0("US_tract_", year), us_tract, envir = .GlobalEnv)
    
    message(" ✔ Loaded and assigned: tract_", year, " and US_tract_", year)
  }
  
  message("✅ All years processed.")
}


tracts_by_state <- function(states, lookup_path = "data/fips_code_lookup.csv", years = 2017:2023) {
  # Read in the lookup table
  lookup <- readr::read_csv(lookup_path, show_col_types = FALSE)
  
  # Convert state abbreviations to FIPS codes (or keep as-is if numeric input)
  if (is.character(states)) {
    fips_codes <- lookup %>%
      dplyr::filter(state %in% states) %>%
      dplyr::pull(state_code) 
  } else {
    fips_codes <- stringr::str_pad(as.character(states), 2, pad = "0")
  }
  
  # Loop over each year and filter the tract data
  for (year in years) {
    tract <- get(paste0("tract_", year), envir = .GlobalEnv)
    
    filtered_tract <- tract %>%
      dplyr::filter(stringr::str_sub(GEOID, 1, 2) %in% fips_codes)
    
    assign(paste0("tract_", year), filtered_tract, envir = .GlobalEnv)
  }
  
  message("Filtered tracts for states: ", paste(states, collapse = ", "))
}


# function to load and filter ACS data
load_filter_acs <- function(states, years, variable_map) {
  # Get unique tables from the mapping
  tables <- unique(variable_map$table)
  
  # Initialize flat list for results
  data_list <- list()
  
  for (state in states) {
    for (year in years) {
      for (table in tables) {
        file_path <- file.path(
          "data", "ACS_US", table, as.character(year),
          paste0("acs5_", table, "_tract_", state, "_", year, ".csv")
        )
        
        if (file.exists(file_path)) {
          df <- read.csv(file_path, stringsAsFactors = FALSE)
          
          if ("variable" %in% colnames(df)) {
            # Filter and rename
            filtered <- variable_map %>%
              filter(table == table, variable %in% df$variable)
            
            if (nrow(filtered) > 0) {
              df_filtered <- df %>%
                filter(variable %in% filtered$variable) %>%
                left_join(filtered, by = "variable") %>%
                mutate(
                  variable = new_name,
                  state = state,
                  year = year,
                  table = table
                ) %>%
                select(-new_name)
              
              data_list[[paste(state, year, table, sep = "_")]] <- df_filtered
            }
          }
        } else {
          warning(paste("File not found:", file_path))
        }
      }
    }
  }
  
  return(data_list)
}

