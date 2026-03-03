# functions.R
timepivot() - initial_exploration.R
get_fe() - initial_exploration.R
count_weekdays() - initial_exploration.R
load_tracts() - tract_validation.R
tracts_by_state() - Not Used
load_filter_acs() - census_data.R

# Step 1: tract_validation.R
purpose: This file attempts to make the census tract boundaries valid. The original boundaries often have errors and lines don't always match at vertices perfectly. All 2019 to 2023 census tracts (adjustable) use the st_make_valid() 

## input datasets
All tracts from 2019 to 2023 in "data/census_tract_boundaries_NHGIS/nhgis0001_shape". E.g., "US_tract_2017.shp""

## output datasets
valid tract boundaries into "data/census_tract_boundaries_NHGIS/valid_geometry". E.g., "tract_2017.rds"




# Step 2: spatial.R
purpose: Convert GTFS files into sf objects and add a 1/4 mile buffer around each route. It also loads in the validated tracts from tract_validation.R and intersects the tracts with the 1/4 mile buffers. Last, it calculates the percent of each route's buffer in each tract so it can normalize the percent weight it puts on the data it gathers from each tract. 

## input datasets
"data/Nashville/GTFS/2024-12-16.zip"
Also validated tracts by year from "data/census_tract_boundaries_NHGIS/valid_geometry"

## output datasets
validated census tract data
"data/Nashville/output/intersecting_tracts_Nashville_2020.rds"




# Step 3: census_data.R
purpose: This file combines your variables of interest into one cohesive datafile for 2020-2023. The variable map function is where you specify the table, variable, and name you'd like to assign for each variable of interest. The load_filter_acs function takes the raw TN tract files in the "data/ACS_US" folder and saves just the columns that are specified in the variable_map. Keep track of the table, variables, and new_names because the new_names are just what you'd like to call it. A name could be mixed up with the wrong variable if you're not careful. Then, acs_all is all of the acs variables bound together. final_df takes the "variable" column and pivots it wider, so each variable has a column. the final_df_edit creates percentage workers for the car, taxi, and work from home variables. A percent households with no vehicles is also created. Then the columns used to make the percents are removed and the file is saved.

## input datasets
uses load_filter_acs to load the "acs_tablename_tracts_state_year.csv" files. These are the ACS tables for each state, census tract, and year. 

## output datasets
"data/Nashville/output/ACS_filt_TN_2020_2023.rds"




# Step 4: acs_tracts_regression.R
purpose: This file first joins the yearly ACS data by the tracts that intersect with each route. Then it linearly interpolates the yearly ACS values by route into monthly values, except for percent WFH. It also linearly extrapolates what the values will be in 2024 through April 2025. 

## input datasets
"data/Nashville/output/ACS_filt_TN_2020_2023.rds"
"data/Nashville/output/intersecting_tracts_Nashville_2020.rds"
"data/Nashville/output/intersecting_tracts_Nashville_2021.rds"
"data/Nashville/output/intersecting_tracts_Nashville_2022.rds"
"data/Nashville/output/intersecting_tracts_Nashville_2023.rds"

## output datasets
"data/Nashville/output/ACS_monthly_Nashville_2021_2025.rds"




# Step 5: initial_exploration.R
purpose: takes files from census_data.R to run models

## input datasets
"data/Nashville/cleaneddata/upt_2025_nashville.xlsx"
"data/Nashville/cleaneddata/vrm_2025_nashville.xlsx"
"data/Nashville/GTFS_dummies.csv"
"data/Nashville/output/ACS_monthly_Nashville_2021_2025.rds"
"data/Nashville/gas_prices/Midwest_All_Grades_All_Formulations_Retail_Gasoline_Prices.csv"




### Other Files ###

# pull_ACS_data.R
purpose: Used for pulling Census data for a variety of use cases, but long and not super clean. Refer to pull_ACS_data_clean.R if needing to pull more ACS data.

# pull_ACS_data_clean.R
purpose: Used to pull American Community Survey tables for specific years and states. Streamlined and simple to use. Run lines 1-50 once. Change information in lines 59-62 based on your use case: 1- or 5-year estimates, the geography for your estimates (different ones for 1 and 5 year), the years you want to pull, and the table name. Only pull one table name at a time. Then list the state you'd like the tables for. Each table downloaded is for a state, year, and table combination. These tables are stored in "data/ACS_US_pull" for initial storage. Once you are happy with your selection, you can copy them into the normal "data/ACS_US" folder.

# additionalcode_initial_exploration.R
purpose: These are additional pieces of code that I had in the initial_exploration.R file that didn't make the final cut. The first chunk is code to add in monthly dummies and GTFS dummies. In the regression, I used factor(month), so the month dummies aren't necessary. The GTFS dummies are based on when the GTFS files were updated. But because they couldn't be generalized over multiple years (e.g., Spring update, Fall update), we didn't end up using them. The next chunk removes a few routes and replaces all NA values with 0s, which isn't necessary because we already have an unbalanced panel (i.e. NAs instead of 0s for UPT/VRM when there are no observations). The next chunk creates correlation plots to test the log transformed variables. The next chunk saves the route FE to an excel file. The next chunk was used when I was building the preliminary models for a project in Dr. Khattak's class. I made the training data Jan 2021-Dec 2024 and the testing data Jan 2025-Apr 2025. I used this code to calculate and plot the MAPE and RMSE for each route. The final chunk is a lot of code I had at the bottom of the file and can't remember exactly what I used it for. Some is for testing, some is plotting, some is diagnostic code. I left it in here just in case we need it.












