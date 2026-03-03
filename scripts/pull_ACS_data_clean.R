### Run these once
# load packages needed for the code
pacman::p_load(tidycensus, tidyverse, beepr)

# replace the census_api_key with your key in the quotation marks
# set up census API key, just need to run once
census_api_key("e44233cec6b29e917ed37f11e9b7407d73c1f1e6",
               install = TRUE, overwrite=TRUE)

# set output directory for all table folders to live in
# it is a sub-directory of your project directory
base_output_dir <- paste0("data/ACS_US_pull/")

# Function to fetch ACS data for a given state and save to file
# Just need to run the function once to store it
get_s0101_data <- function(state_fips, year) {
  message("Fetching data for: ", state_fips, " (", year, ")")
  
  tryCatch({
    # Define year-specific directory
    year_output_dir <- paste0(base_output_dir, acs_estimate, "/", table_name, "/", year, "/")
    
    # Create year-specific directory if it doesn't exist
    dir.create(year_output_dir, recursive = TRUE, showWarnings = FALSE)
    
    # fetch data
    data <- get_acs(
      geography = geography_name,
      table = table_name,
      year = year,
      survey = acs_estimate,
      state = state_fips,
      key = Sys.getenv("e44233cec6b29e917ed37f11e9b7407d73c1f1e6")
    ) %>%
      mutate(state = state_fips)
    
    # Define output file path
    output_path <- paste0(year_output_dir, acs_estimate, "_", table_name, "_", geography_name, "_", state_fips, "_", year, ".csv")
    
    # Save data to CSV
    write_csv(data, output_path)
    
    message("✅ Saved: ", output_path)
    #beep(sound = 1)
  }, error = function(e) {
    message("❌ Error fetching data for ", state_fips, " (", year, "): ", e$message)
    beep(sound = 11)
  })
}







### This is where you change the data you need
# Define parameters
geography_name <- "tract" # census tract is geography for 5-year estimates
acs_estimate <- "acs5"    # 5-year estimates, do acs1 for 1-year estimates
years <- 2017:2024        # Change to desired year(s)
table_name <- "B08006"    # one table at a time

# List the states you want to pull data for
states <- c("TN")

# Full list of state codes
# "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"

# Loop through all states and save each state's data separately
# highlight lines 62-67 and run together
# after running it for the first time, watch in the console to make sure you are getting green check marks showing the data has been pulled and downloaded for the first few states
grid <- expand_grid(year = years, state = states) 
pwalk(grid, get_s0101_data)

beep(sound = 8) # makes a sound when the code is finished running

message("All states and years have been processed and saved!")










