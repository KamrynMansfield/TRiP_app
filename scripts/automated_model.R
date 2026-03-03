# load packages
pacman::p_load(tidyverse, readxl, writexl, fixest, modelsummary, scales, patchwork, purrr)

# load in custom functions
source("scripts/functions.R")

# load in upt and vrm
upt <- read_excel("data/Nashville/cleaneddata/upt_2025_nashville.xlsx", na = "NA")

vrm <- read_excel("data/Nashville/cleaneddata/vrm_2025_nashville.xlsx", na = "NA")

# pivot datasets longer
data_list <- c("upt", "vrm")
timepivot(data_list)

# left join to combine upt and vrm datasets
df_og <- upt %>% 
  left_join(vrm, by = c("time", "route", "month", "year")) 

# rename route as route_id
df <- df_og %>% 
  rename(route_id = route)

# create column with # of weekdays per month
df <- count_weekdays(df)

# create centered year to later estimate time trend 
df$year_cent <- df$year - 2021

# remove extra datasets
rm(upt, vrm, df_og, data_list)

# load in preprocessed monthly American Community Survey data
acs <- readRDS("data/Nashville/output/ACS_monthly_Nashville_2021_2025.rds")

# load in gas_prices data
gas <- read_csv("data/Nashville/gas_prices/Midwest_All_Grades_All_Formulations_Retail_Gasoline_Prices.csv", 
                skip = 4) %>% 
  rename(date = "Month",
         gas_price = "Midwest All Grades All Formulations Retail Gasoline Prices Dollars per Gallon") %>% 
  mutate(date = my(date),
         month = month(date),
         year = year(date)) %>%
  select(-date) 

# create month and year column for ACS
acs <- acs %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  select(-date)

# combine gas prices and acs with upt and vrm
df_all <- df %>% 
  left_join(gas, by = c("month", "year")) %>% 
  left_join(acs, by = c("month", "year", "route_id")) %>% 
  mutate(month = factor(month))

# make upt a weekday average using number of weekdays in the month
df_all <- df_all %>% 
  mutate(upt_avg = upt/weekdays_in_month)

#' list variables to be log transformed
#' three variables not log transformed: "workers_16_over", "median_earnings", "emp_pop_ratio"
vars_to_log <- c("upt", "upt_avg", "vrm", "gas_price", "perc_car", "perc_taxicab", "perc_wfh", "perc_female", "below_fpl", "fpl_100_150", "perc_renter_occupied", "population", "labor_part_rate", "unemp_rate", "perc_hshlds_noveh")

# log transform vars_to_log variables
df_all_log <- df_all %>%
  mutate(across(
    all_of(vars_to_log),
    ~ if_else(. > 0, log(.), NA_real_),   # safely take log only if positive
    .names = "log_{.col}"          # rename: log_variable
  )) %>%
  select(-all_of(vars_to_log))


candidate_variables <- c("log_gas_price", "log_perc_hshlds_noveh", "log_below_fpl", "log_perc_car", "log_perc_taxicab", "log_perc_wfh", "log_perc_female", "log_fpl_100_150", "log_perc_renter_occupied", "log_labor_part_rate", "log_unemp_rate")

rejected_var <- "placeholder"
max_p <- .9
n_iter <- 0
max_iter <- length(candidate_variables) + 5

while (max_p > 0.05){
  # exclude the previous iteration's high p-value variable
  candidate_variables <- candidate_variables[!candidate_variables %in% rejected_var]
  
  # create the model
  lm_vrm <- feols(as.formula(paste("log_upt_avg~log_vrm+factor(month)+year_cent+I(year_cent^2)+", paste(candidate_variables, collapse = " + "), "| route_id")), data=df_all_log, cluster= ~route_id)
  
  # get vector of p-values
  pvals <- lm_vrm$coeftable[,"Pr(>|t|)"]
  
  # get the max p-value
  max_p <- max(pvals)
  
  # get the name of the variable with the highest p-value
  # this one will get kicked out next iteration
  rejected_var <- names(pvals)[which(pvals == max_p)]
  
  n_iter <- n_iter + 1
  
  if (n_iter > max_iter){
    break
  }
}

coefplot(lm_vrm)
etable(lm_vrm)

# this df will be used later to check the signs 
sign_df <- matrix(c("log_vrm", "+", TRUE,
                    "log_vrm", "+", TRUE,
                    "log_vrm", "+", TRUE,
                    "log_vrm", "+", TRUE,
                    "log_vrm", "+", TRUE,
                    "log_vrm", "+", TRUE,
                    "log_vrm", "+", TRUE,
                    "log_vrm", "+", TRUE,
                    "log_vrm", "+", TRUE,
                    "log_vrm", "+", TRUE),
                  byrow = T, ncol = 3) |>
  as.data.frame()

names(sign_df) <- c("varible", "expected_sign","sign_required")

