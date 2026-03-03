pacman::p_load(tidycensus, tidyverse, beepr)

# set up census API key
census_api_key("ENTER CENSUS API KEY HERE",
               install = TRUE)

# Fetch all ACS 1-year variables available
acs_vars <- load_variables(2023, "acs1", cache = TRUE)

# set the time frame to pull the data
nashville_years <- 2021:2023  # Update this to include 2024 when available

# geographies and their FIPS numbers
# Nashville's Davidson County and Surrounding counties
nashville_fips <- "47037"
cheatheam_fips <- "47021"
robertson_fips <- "47147"
rutherford_fips <- "47149"
sumner_fips <- "47165"
williamson_fips <- "47187"
wilson_fips <- "47189"

#Davidson + surrounding counties
counties <- c("47037", "47021", "47147", "47149", "47165", "47187", "47189")

# load in functions
source("documents/ACS_functions.R")

# Loop through years and combine
nashville_acs <- map_df(nashville_years, get_acs_data)

# View data
head(nashville_acs)




# Example: Get data for Williamson County for 2023
williamson_acs <- get_county_acs("Davidson", 2023)

############################################################

acs_vars <- load_variables(2022, "acs1", cache = TRUE)
View(acs_vars)  # Opens a table of available variables

davidson_acs_2022 <- get_acs(
  geography = "county",
  variables = acs_vars$name,  # Pull all available ACS 1-year variables
  year = 2022,
  survey = "acs1",  # 1-year estimates
  state = "47",  # Tennessee state FIPS code
  county = "037",  # Davidson County FIPS code
  key = Sys.getenv("CENSUS_API_KEY")
)

# View first few rows
head(davidson_acs_2022)



############################################################

# Define years and county FIPS
years <- 2021:2023
county_fips <- "037"  # Davidson County, TN
state_fips <- "47"    # Tennessee

# Function to get ACS data for census tracts in Davidson County
get_s0802_data <- function(year) {
  get_acs(
    geography = "tract",
    table = "S0802",
    year = year,
    survey = "acs5",  # ACS 5-Year Estimates
    state = state_fips,
    county = county_fips,
    key = Sys.getenv("CENSUS_API_KEY")
  ) %>%
    mutate(year = year)
}

# Loop through years and combine the results
davidson_s0802 <- map_df(years, get_s0802_data)

# View first few rows
head(davidson_s0802)

# Save to CSV
write_csv(davidson_s0802, "davidson_S0802_tracts_2021_2023.csv")


############################################################

# Define years and county FIPS
years <- 2021:2023
county_fips <- "037"  # Davidson County, TN
state_fips <- "47"    # Tennessee

# Function to get ACS data for census tracts in Davidson County
get_s0802_data <- function(year, county_fips) {
  get_acs(
    geography = "tract",
    table = "S0802",
    year = year,
    survey = "acs5",  # ACS 5-Year Estimates
    state = state_fips,
    county = county_fips,
    key = Sys.getenv("586e805d2349e1bfa31993184dab146a53ba66dd")
  ) %>%
    mutate(year = year)
}

# Loop through years and combine the results
davidson_s0802 <- map_df(years, ~get_s0802_data(.x, ))

# View first few rows
head(davidson_s0802)

# Save to CSV
write_csv(davidson_s0802, "data/ACS/davidson_S0802_tracts_2021_2023.csv")


############################################################
############################################################
############################################################

# get information for the Nashville region
#years <- 2021:2023
#years_range <- "2021_2023"
#state_name <- "nashvillecounties"
#state_fips <- "47"  # Tennessee state FIPS
#counties <- c("037", "021", "147", "149", "165", "187", "189")  # Davidson + surrounding counties
# Davidson, Cheatham, Robertson, Rutherford, Sumner, Williamson, Wilson

# get information for the Atlanta region
years <- 2017:2023
years_range <- "2017_2023"
state_name <- "Atlanta"
state_fips <- "13"
counties <- c("097", "113", "063", "151", "247", "089", "135", "117", "121", "057", "067")
# Douglas, Fayette, Clayton, Henry, Rockdale, DeKalb, Gwinnett, Forsyth, Fulton, Cherokee, and Cobb counties

# Get information for NYC
years <- 2021:2023
years_range <- "2021_2023"
state_name <- "nyccounties"
state_fips <- "36"  
counties <- c("005", "047", "061", "081", "085")  
# Bronx (The Bronx), Kings (Brooklyn), New York (Manhattan), Queens (Queens), Richmond (Staten Island) 

# list of ACS tables to fetch
tables <- c("S0802", "S1903", "S0101", "S1901", "DP02", "DP03", "DP04", "DP05", "S0801", "S1501", "S1701", "S1810", "S2301", "S2801", "S2902", "S2501", "S2503")
#' S0802 - Means of Transportation to Work by Selected Characteristics
#' S1903 - Median Income in the Past 12 Months
#' S0101 - Age and Sex
#' S1901 - Income in the Past 12 Months
#' DP02 - Selected Social Characteristics in the United States
#' DP03 - Selected Economic Characteristics
#' DP04 - Selected Housing Characteristics
#' DP05 - ACS Demographic and Housing Estimates
#' S0801 - Commuting Characteristics by Sex
#' S1501 - Educational Attainment
#' S1701 - Poverty Status in the Past 12 Months
#' S1810 - Disability Characteristics
#' S2301 - Employment Status
#' S2801 - Types of Computers and Internet Subscriptions
#' S2902 - Citizen, Voting-Age Population by Selected Characteristics
#' S2501 - Occupancy Characteristics
#' S2503 - Financial Characteristics

# Function to get ACS data for a given table, county, and year
get_acs_data <- function(table, county_fips, year) {
  get_acs(
    geography = "tract",
    table = table,
    year = year,
    survey = "acs5",  # Use ACS 5-year estimates for tract-level data
    state = state_fips,
    county = county_fips,
    key = Sys.getenv("CENSUS_API_KEY"),
    cache_table = TRUE
  ) %>%
    mutate(year = year, county_fips = county_fips, table = table)
  }


# Loop through each table and save results separately
for (table_name in tables) {
  
  # Fetch ACS data for all years and counties for the current table
  acs_results <- expand_grid(year = years, county = counties) %>%
    pmap_dfr(~ get_acs_data(table = table_name, county_fips = .y, year = .x))
  
  # Define the output file path
  output_path <- paste0("data/ACS/acs_", table_name, "_", state_name, "_countylevel_", years_range, ".csv")
  
  # Save to CSV
  write_csv(acs_results, output_path)
  
  # Print status message
  message("Saved: ", output_path)
  beep(sound = 10)
  }

beep(sound = 2)

message("✅ All ACS tables have been processed and saved!")




############################################################
################## Regional Gas Price Data #################
############################################################
#' Nashville is in PADD 2: Midwest
#' Atlanta is in PADD 1C: Lower Atlantic
#' New York City has its own estimates
 
############################################################
################# Regional Employment Data #################
############################################################
install.packages("blscrapeR")
library(blscrapeR)

############################################################
################## Census Tracts in the US #################
############################################################

# Define parameters
year <- 2022  # Change to desired year
table_name <- "S0101"

# Get a list of all state FIPS codes (excluding territories)
#states <- c(state.abb, "DC")  # Uses state abbreviations + DC
states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC")

# Function to fetch ACS data for a given state
get_s0101_data <- function(state_fips) {
  get_acs(
    geography = "tract",
    table = table_name,
    year = year,
    survey = "acs5",
    state = state_fips,
    key = Sys.getenv("CENSUS_API_KEY")
  ) %>%
    mutate(state = state_fips)
  }

# Loop through all states and combine results
s0101_tracts <- map_df(states, get_s0101_data)

# Save to CSV
write_csv(s0101_tracts, paste0("data/ACS_US/acs_", table_name, "_", state_fips, "_", year, ".csv"))

# Print confirmation
message("✅ Download complete: All U.S. census tracts for ", table_name, " (", year, ") saved.")

############################################################
########### State-wise Census Tracts in the US #############
############################################################



# Install and load necessary libraries
pacman::p_load(tidycensus, tidyverse, beepr)

# Define parameters
years <- 2017:2023  # Change to desired year
table_name <- "GEOINFO"

# Get a list of all state FIPS codes (excluding territories)
#states <- c(state.abb, "DC")  # Uses state abbreviations + DC
states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC")

# "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"

# Create output directory if it doesn't exist
base_output_dir <- paste0("data/ACS_US/")


# Function to fetch ACS data for a given state and save to file
get_s0101_data <- function(state_fips, year) {
  message("Fetching data for: ", state_fips, " (", year, ")")
  
  tryCatch({
    # Define year-specific directory
    year_output_dir <- paste0(base_output_dir, table_name, "/", year, "/")
    
    # Create year-specific directory if it doesn't exist
    dir.create(year_output_dir, recursive = TRUE, showWarnings = FALSE)
    
    # fetch data
    data <- get_acs(
      geography = "tract",
      table = table_name,
      year = year,
      survey = "acs5",
      state = state_fips,
      key = Sys.getenv("CENSUS_API_KEY")
    ) %>%
      mutate(state = state_fips)
    
    # Define output file path
    output_path <- paste0(year_output_dir, "acs_", table_name, "_tracts_", state_fips, "_", year, ".csv")
    
    # Save data to CSV
    write_csv(data, output_path)
    
    message("✅ Saved: ", output_path)
    #beep(sound = 1)
  }, error = function(e) {
    message("❌ Error fetching data for ", state_fips, " (", year, "): ", e$message)
    beep(sound = 11)
  })
}

# Loop through all states and save each state's data separately
grid <- expand_grid(year = years, state = states) 
pwalk(grid, get_s0101_data)

beep(sound = 8)

message("All states and years have been processed and saved!")










