# load packages
pacman::p_load(tidyverse)

# load in custom functions
source("scripts/functions.R")

# list states and years
states <- c("TN")
years <- 2020:2024

# do variable mapping for all variables and all tables
variable_map <- tibble::tibble(
  table = c("B08006", "B08006", "B08006",  "B08006", "B08006", "B08006", "B08006", "S0802", "S0802", "S0802", "S0802", "S0802", "S0802","DP05", "S2301", "S2301", "S2301", "B08201", "B08201"),
  variable = c("B08006_001", "B08006_017", "B08006_016", "B08006_002", "B08006_008", "B08006_014", "B08006_015", "S0802_C01_039", "S0802_C01_040", "S0802_C01_037", "S0802_C01_001", "S0802_C01_010", "S0802_C01_093","DP05_0001", "S2301_C02_001", "S2301_C03_001", "S2301_C04_001", "B08201_001", "B08201_002"),
  new_name = c("total_workers", "work_from_home", "taxicab", "car", "public_transit", "bike", "walk", "below_fpl", "fpl_100_150", "median_earnings", "workers_16_over", "perc_female", "perc_renter_occupied", "population", "labor_part_rate", "emp_pop_ratio", "unemp_rate", "total_hshlds", "hshlds_no_veh"))

acs_data <- load_filter_acs(states, years, variable_map)

acs_all <- do.call(rbind, acs_data)

final_df <- acs_all %>%
  select(-moe, -state, -table) %>%     
  pivot_wider(
    names_from = variable,             
    values_from = estimate             
  )

# add new commuter percent columns
final_df_edit <- final_df %>% 
  mutate(perc_car = round((car/total_workers)*100,1),
         perc_taxicab = round((taxicab/total_workers)*100,1),
         perc_wfh = round((work_from_home/total_workers)*100,1),
         perc_hshlds_noveh = round((hshlds_no_veh/total_hshlds)*100,1))

# remove old commute columns
final_df_edit <- final_df_edit %>% 
  select(-c(total_workers, car, public_transit, bike, walk, taxicab, work_from_home, total_hshlds, hshlds_no_veh))


saveRDS(final_df_edit, "data/Nashville/output/ACS_filt_TN_2020_2025.rds")


