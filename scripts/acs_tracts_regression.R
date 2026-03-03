# load packages
pacman::p_load(tidyverse, sf)

# load in custom functions
source("scripts/functions.R")

# load in acs data
acs <- readRDS("data/Nashville/output/ACS_filt_TN_2020_2025.rds")

# load in census tract data
tract_2020 <- readRDS("data/Nashville/output/intersecting_tracts_Nashville_2020.rds")
tract_2021 <- readRDS("data/Nashville/output/intersecting_tracts_Nashville_2021.rds")
tract_2022 <- readRDS("data/Nashville/output/intersecting_tracts_Nashville_2022.rds")
tract_2023 <- readRDS("data/Nashville/output/intersecting_tracts_Nashville_2023.rds")
tract_2024 <- readRDS("data/Nashville/output/intersecting_tracts_Nashville_2024.rds")

# make sure GEOID column is compatible between ACS and tract
acs$GEOID <- as.character(acs$GEOID)

# Filter ACS by year and join to tracts and drop geometry
tract_2020 <- tract_2020 %>%
  st_drop_geometry() %>%
  left_join(acs %>% filter(year == 2020), by = "GEOID")

tract_2021 <- tract_2021 %>%
  st_drop_geometry() %>%
  left_join(acs %>% filter(year == 2021), by = "GEOID")

tract_2022 <- tract_2022 %>%
  st_drop_geometry() %>%
  left_join(acs %>% filter(year == 2022), by = "GEOID")

tract_2023 <- tract_2023 %>%
  st_drop_geometry() %>%
  left_join(acs %>% filter(year == 2023), by = "GEOID")

tract_2024 <- tract_2024 %>%
  st_drop_geometry() %>%
  left_join(acs %>% filter(year == 2024), by = "GEOID")

# NEED TO MAKE THIS FLEXIBLE OVER TIME!!!!
# get adjusted estimates for tracts, then sum for each route
adj_2020 <- tract_2020 %>%
  mutate(across(workers_16_over:perc_hshlds_noveh, ~ .x * percent_of_buffer, .names = "{.col}")) %>%
  group_by(route_id) %>%
  summarise(across(
    workers_16_over:perc_hshlds_noveh,
    ~ sum(.x, na.rm = TRUE)
  ), .groups = "drop")  

adj_2021 <- tract_2021 %>%
  mutate(across(workers_16_over:perc_hshlds_noveh, ~ .x * percent_of_buffer, .names = "{.col}")) %>%
  group_by(route_id) %>%
  summarise(across(
    workers_16_over:perc_hshlds_noveh,
    ~ sum(.x, na.rm = TRUE)
  ), .groups = "drop")  

adj_2022 <- tract_2022 %>%
  mutate(across(workers_16_over:perc_hshlds_noveh, ~ .x * percent_of_buffer, .names = "{.col}")) %>%
  group_by(route_id) %>%
  summarise(across(
    workers_16_over:perc_hshlds_noveh,
    ~ sum(.x, na.rm = TRUE)
  ), .groups = "drop")  

adj_2023 <- tract_2023 %>%
  mutate(across(workers_16_over:perc_hshlds_noveh, ~ .x * percent_of_buffer, .names = "{.col}")) %>%
  group_by(route_id) %>%
  summarise(across(
    workers_16_over:perc_hshlds_noveh,
    ~ sum(.x, na.rm = TRUE)
  ), .groups = "drop")  

adj_2024 <- tract_2024 %>%
  mutate(across(workers_16_over:perc_hshlds_noveh, ~ .x * percent_of_buffer, .names = "{.col}")) %>%
  group_by(route_id) %>%
  summarise(across(
    workers_16_over:perc_hshlds_noveh,
    ~ sum(.x, na.rm = TRUE)
  ), .groups = "drop")  

rm(tract_2020, tract_2021, tract_2022, tract_2023, tract_2024)

############################################################
############# Linear Interpolation of ACS Data #############
############################################################
# load in libraries
pacman::p_load(lubridate, zoo, tidyr, dplyr)

# 1. Add date columns (Assign yearly observations to December of each year)
adj_2020 <- adj_2020 %>% mutate(date = ymd("2020-12-01"))
adj_2021 <- adj_2021 %>% mutate(date = ymd("2021-12-01"))
adj_2022 <- adj_2022 %>% mutate(date = ymd("2022-12-01"))
adj_2023 <- adj_2023 %>% mutate(date = ymd("2023-12-01"))
adj_2024 <- adj_2024 %>% mutate(date = ymd("2024-12-01"))

# 2. Combine all into one long dataframe
adj_all <- bind_rows(adj_2020, adj_2021, adj_2022, adj_2023, adj_2024)

# 3. Pivot longer to make variable column
adj_long <- adj_all %>%
  pivot_longer(cols = workers_16_over:perc_hshlds_noveh, names_to = "variable", values_to = "value")

# 4. Create monthly date sequence
full_dates <- tibble(date = seq(ymd("2020-12-01"), ymd("2025-11-01"), by = "1 month")) # kamryn changed

# 5. Expand to all combinations of route_id, variable, and date
# But for perc_wfh, use yearly values instead of interpolating
adj_filled <- expand_grid(
  route_id = unique(adj_long$route_id),
  variable = unique(adj_long$variable),
  date = full_dates$date
) %>%
  left_join(adj_long, by = c("route_id", "variable", "date")) %>%
  group_by(route_id, variable) %>%
  arrange(date) %>%
  mutate(
    value = if_else(
      variable == "perc_wfh",
      # For perc_wfh → carry last yearly value forward
      zoo::na.locf(value, na.rm = FALSE),
      # For all others → linear interpolation
      zoo::na.approx(value, x = date, na.rm = FALSE)
    )
  ) %>%
  ungroup()

# 6. Extrapolate forward to November 2025 using the trend from Dec 2022 to Dec 2024   (Kamryn changed the dates below)
# For perc_wfh, the values will be linearly extrapolated despite inputting yearly values
extrapolated <- adj_filled %>%
  group_by(route_id, variable) %>%
  arrange(date) %>%
  mutate(
    value = case_when(
      # --- SPECIAL RULE FOR perc_wfh ---
      variable == "perc_wfh" & date <= ymd("2024-12-01") ~ value,
      
      # Hold 2023-12-01 value constant until 2024-11-01
      variable == "perc_wfh" & date > ymd("2024-12-01") & date <= ymd("2025-11-01") ~
        value[date == ymd("2024-12-01")],
      
      # From 2024-12-01 onward, add the full annual increment (2022→2023)
      variable == "perc_wfh" & date >= ymd("2025-12-01") ~ {
        val_2023 <- value[date == ymd("2023-12-01")]
        val_2024 <- value[date == ymd("2024-12-01")]
        annual_increment <- val_2024 - val_2023
        val_2024 + annual_increment
      },
      
      # --- DEFAULT RULE FOR ALL OTHER VARIABLES ---
      date <= ymd("2024-12-01") ~ value,  # keep known & interpolated values
      TRUE ~ {
        # Linear monthly extrapolation using the change from 2022-12-01 → 2023-12-01
        val_2023 <- value[date == ymd("2023-12-01")]
        val_2024 <- value[date == ymd("2024-12-01")]
        
        increment <- (val_2024 - val_2023) / 12
        months_ahead <- interval(ymd("2024-12-01"), date) %/% months(1)
        val_2024 + increment * months_ahead
      }
    )
  ) %>%
  ungroup()


# 7. Filter to just Jan 2021 – Apr 2025
adj_monthly <- extrapolated %>%
  filter(date >= ymd("2021-01-01"), date <= ymd("2025-11-01")) %>%    # kamryn changed this
  pivot_wider(names_from = variable, values_from = value)

saveRDS(adj_monthly, "data/Nashville/output/ACS_monthly_Nashville_2021_2025.rds")
