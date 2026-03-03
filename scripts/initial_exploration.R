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

#' plot any included y variable to see historic values for each route
#' note values after 2023 (time=24) are linearly interpolated
ggplot(df_all_log, aes(x = time, y = log_upt, color = route_id)) +
  geom_point() +
  geom_line()

#' list of regressors except for upt, vrm, month, and time trend
#' for each model, I would run the prior model, then remove the variable(s) with the highest p-value
log_reg_1 <- c("log_gas_price", "log_perc_hshlds_noveh", "log_below_fpl", "log_perc_car", "log_perc_taxicab", "log_perc_wfh", "log_perc_female", "log_fpl_100_150", "log_perc_renter_occupied", "log_labor_part_rate", "log_unemp_rate")

log_reg_2 <- c("log_gas_price", "log_labor_part_rate", "log_perc_wfh", "log_unemp_rate", "log_perc_renter_occupied", "log_perc_female", "log_perc_hshlds_noveh", "log_below_fpl", "log_perc_taxicab")

log_reg_3 <- c("log_gas_price", "log_labor_part_rate", "log_perc_wfh", "log_unemp_rate", "log_perc_renter_occupied", "log_perc_hshlds_noveh", "log_below_fpl", "log_perc_taxicab")

log_reg_4 <- c("log_gas_price", "log_labor_part_rate", "log_unemp_rate", "log_perc_renter_occupied", "log_perc_hshlds_noveh", "log_below_fpl", "log_perc_taxicab")

log_reg_5 <- c("log_gas_price", "log_labor_part_rate", "log_perc_renter_occupied", "log_perc_hshlds_noveh", "log_below_fpl", "log_perc_taxicab")

log_reg_6 <- c("log_gas_price", "log_perc_renter_occupied", "log_perc_hshlds_noveh", "log_below_fpl", "log_perc_taxicab")

log_reg_7 <- c("log_gas_price", "log_perc_hshlds_noveh", "log_below_fpl", "log_perc_taxicab")

log_reg_8 <- c("log_gas_price", "log_perc_hshlds_noveh", "log_below_fpl")

log_reg_9 <- c("log_gas_price")

# log-log fixed effects regressions
lm_vrm_1 <- feols(as.formula(paste("log_upt_avg~log_vrm+factor(month)+year_cent+I(year_cent^2)+", paste(log_reg_1, collapse = " + "), "| route_id")), data=df_all_log, cluster= ~route_id)

lm_vrm_2 <- feols(as.formula(paste("log_upt_avg~log_vrm+factor(month)+year_cent+I(year_cent^2)+", paste(log_reg_2, collapse = " + "), "| route_id")), data=df_all_log, cluster= ~route_id)

lm_vrm_3 <- feols(as.formula(paste("log_upt_avg~log_vrm+factor(month)+year_cent+I(year_cent^2)+", paste(log_reg_3, collapse = " + "), "| route_id")), data=df_all_log, cluster= ~route_id)

lm_vrm_4 <- feols(as.formula(paste("log_upt_avg~log_vrm+factor(month)+year_cent+I(year_cent^2)+", paste(log_reg_4, collapse = " + "), "| route_id")), data=df_all_log, cluster= ~route_id)

lm_vrm_5 <- feols(as.formula(paste("log_upt_avg~log_vrm+factor(month)+year_cent+I(year_cent^2)+", paste(log_reg_5, collapse = " + "), "| route_id")), data=df_all_log, cluster= ~route_id)

lm_vrm_6 <- feols(as.formula(paste("log_upt_avg~log_vrm+factor(month)+year_cent+I(year_cent^2)+", paste(log_reg_6, collapse = " + "), "| route_id")), data=df_all_log, cluster= ~route_id)

lm_vrm_7 <- feols(as.formula(paste("log_upt_avg~log_vrm+factor(month)+year_cent+I(year_cent^2)+", paste(log_reg_7, collapse = " + "), "| route_id")), data=df_all_log, cluster= ~route_id)

lm_vrm_8 <- feols(as.formula(paste("log_upt_avg~log_vrm+factor(month)+year_cent+I(year_cent^2)+", paste(log_reg_8, collapse = " + "), "| route_id")), data=df_all_log, cluster= ~route_id)

lm_vrm_9 <- feols(as.formula(paste("log_upt_avg~log_vrm+factor(month)+year_cent+I(year_cent^2)+", paste(log_reg_9, collapse = " + "), "| route_id")), data=df_all_log, cluster= ~route_id)


# model summary of all nine models - can adjust to only show models of interest
modelsummary(list(lm_vrm_1, lm_vrm_8, lm_vrm_9), # lm_vrm_2, lm_vrm_3, lm_vrm_4, lm_vrm_5, lm_vrm_6, lm_vrm_7, lm_vrm_8, lm_vrm_9), 
             estimate = "{estimate} ({p.value}){stars}", statistic = NULL, fmt = 2, output = "gt", 
             coef_map = c( # this is what determines the order of the variables in the table
               "log_vrm" = "Log VRM",
                "factor(month)2" = "February",
                "factor(month)3" = "March",
                "factor(month)4" = "April",
                "factor(month)5" = "May",
                "factor(month)6" = "June",
                "factor(month)7" = "July",
                "factor(month)8" = "August",
                "factor(month)9" = "September",
                "factor(month)10" = "October",
                "factor(month)11" = "November",
                "factor(month)12" = "December",
                "year_cent" = "Year",
                "I(I(year_cent^2))" = "Year^2",
                "log_gas_price" = "Log Gas Price",
                "log_perc_hshlds_noveh" = "Log % Households No Vehicle Available",
                "log_below_fpl" = "Log % Workers Below FPL",
                "log_perc_car" = "Log % Workers Commuting - Car",
                "log_perc_taxicab" = "Log % Workers Commuting - Ridehailing",
                "log_perc_wfh" = "Log % Workers Commuting - Work from Home",
                "log_perc_female" = "Log % Female Workers",
                "log_fpl_100_150" = "Log % Workers 100%-150% FPL",
                "log_perc_renter_occupied" = "Log % Workers in Renting Households",
                "log_labor_part_rate" = "Log Labor Participation Rate",
                "log_unemp_rate" = "Log Unemployment Rate"))

# extract the route_id fixed effects for a specific model, just to see what the values are
route_fe <- get_fe(lm_vrm_8, "route_id")

#' to uncomment these large chunks, highlight and press CTRL-SHIFT-C
# # save output as excel
# modelsummary(list(lm_vrm_1, lm_vrm_8, lm_vrm_9), estimate = "{estimate} ({p.value}){stars}", statistic = NULL, fmt = 2,
#              coef_map = c(
#                "log_vrm" = "Log VRM",
#                "factor(month)2" = "February",
#                "factor(month)3" = "March",
#                "factor(month)4" = "April",
#                "factor(month)5" = "May",
#                "factor(month)6" = "June",
#                "factor(month)7" = "July",
#                "factor(month)8" = "August",
#                "factor(month)9" = "September",
#                "factor(month)10" = "October",
#                "factor(month)11" = "November",
#                "factor(month)12" = "December",
#                "year_cent" = "Year",
#                "I(year_cent^2)" = "Year^2",
#                "log_gas_price" = "Log Gas Price",
#                "log_perc_hshlds_noveh" = "Log % Households No Vehicle Available",
#                "log_below_fpl" = "Log % Workers Below FPL",
#                "log_fpl_100_150" = "Log % Workers 100%-150% FPL",
#                "log_unemp_rate" = "Log Unemployment Rate",
#                "log_labor_part_rate" = "Log Labor Participation Rate",
#                "log_perc_car" = "Log % Workers Commuting - Car",
#                "log_perc_wfh" = "Log % Workers Commuting - Work from Home",
#                "log_perc_taxicab" = "Log % Workers Commuting - Taxicab",
#                "log_perc_renter_occupied" = "Log % Workers in Renting Households",
#                "log_perc_female" = "Log % Female Workers"),
#              output = "data/Nashville/output/stage1modeloutput.xlsx")

################### Summary Statistics #####################
# summary statistics for df
vars_to_summarise <- c("upt", "upt_avg", "vrm", "gas_price", "perc_hshlds_noveh", "below_fpl", "perc_car", "perc_taxicab", "perc_wfh", "perc_female",  "fpl_100_150", "perc_renter_occupied", "labor_part_rate", "unemp_rate")

pretty_names <- c(
  upt = "Unlinked Passenger Trips",
  upt_avg = "Average Monthly Weekday UPT",
  vrm = "Vehicle Revenue Miles",
  gas_price = "Gas Price",
  perc_hshlds_noveh = "% Households No Vehicle Available",
  below_fpl = "% Workers Below FPL",
  perc_car = "% Workers Commuting - Car",
  perc_taxicab = "% Workers Commuting - Ridehailing",
  perc_wfh = "% Workers Commuting - Work from Home",
  perc_female = "% Female Workers",
  fpl_100_150 = "% Workers 100%-150% FPL",
  perc_renter_occupied = "% Workers in Renting Households",
  labor_part_rate = "Labor Participation Rate",
  unemp_rate = "Unemployment Rate")


# create summary
summary_stats <- map_dfr(vars_to_summarise, function(var) {
  df_all %>%
    summarise(
      variable = pretty_names[[var]],
      mean = round(mean(.data[[var]], na.rm = TRUE),2),
      sd   = round(sd(.data[[var]], na.rm = TRUE),2),
      min  = round(min(.data[[var]], na.rm = TRUE),2),
      max  = round(max(.data[[var]], na.rm = TRUE),2))
})

print(summary_stats)

# write_xlsx(summary_stats, "data/Nashville/output/overall_sum_stats.xlsx")


################# Forecasts Per ChatGPT ####################
## choose model ###
model <- lm_vrm_9
###             ###

# extract coefficients
coefs <- coef(model)

elasticities <- coefs[!grepl("factor\\(month\\)|year_cent", names(coefs))]

month_coefs <- coefs[grepl("factor\\(month\\)", names(coefs))]
names(month_coefs) <- gsub("factor\\(month\\)", "", names(month_coefs))

# add January's month coefficient explicitly as a zero
month_coefs <- c("1" = 0, month_coefs)

# extract linear and quadratic time trend component coefficients
year_lin <- coefs["year_cent"]
year_quad <- coefs["I(I(year_cent^2))"] #changed

# reference UPT for all routes - April 2025
route_reference <- df_all_log %>%
  filter(year == 2025, month == 4) %>% #changed
  group_by(route_id) %>%
  summarise(
    ref_ridership = mean(exp(log_upt_avg), na.rm = TRUE),
    .groups = "drop"
  )

#' scenario definitions: low (status quo), medium (expected VRM growth), high (large VRM)
#' these are annual change rates (e.g., log_vrm=0.5 means a 50% increase in VRM)
scenario_inputs <- list(
  Low = c(
    log_vrm = 0.01,
    log_gas_price = -0.01,
    log_perc_hshlds_noveh = -0.01,
    log_below_fpl = -0.01
  ),
  Medium = c(
    log_vrm = 0.15,     # e.g. 15% annual VRM growth
    log_gas_price = 0.01,
    log_perc_hshlds_noveh = 0.01,
    log_below_fpl = 0.01
  ),
  High = c(
    log_vrm = 0.50,
    log_gas_price = 0.02,
    log_perc_hshlds_noveh = 0.02,
    log_below_fpl = 0.02
  )
)

# make scenarios data a dataframe
scenario_df <- bind_rows(
  lapply(names(scenario_inputs), function(s) {
    tibble(
      scenario = s,
      variable = names(scenario_inputs[[s]]),
      change = scenario_inputs[[s]]
    )
  })
)

# Create forecast grid until December 2026
forecast_grid <- expand_grid(
  route_id = unique(route_reference$route_id),
  year = c(2025, 2026),
  month = 1:12,
  scenario = names(scenario_inputs)) %>%
  filter(!(year == 2025 & month <= 4)) %>% # changed
  left_join(route_reference, by = "route_id")

#' add scenarios to forecast_grid
#' NA values are okay, it happens if the variable in the scenarios wasn't in the regression
#' elasticity_contrib is the elasticity coef multiplied by the scenario change rate
forecast_expanded <- forecast_grid %>%
  left_join(scenario_df, by = "scenario") %>%
  mutate(
    elasticity = elasticities[variable],
    elasticity_contrib = elasticity * change
  )

# sum elasticity_contrib for each route, month, scenario combination
forecast_components <- forecast_expanded %>%
  group_by(route_id, year, month, scenario, ref_ridership) %>%
  summarise(
    scenario_effect = sum(elasticity_contrib, na.rm = TRUE),
    .groups = "drop"
  )

# incorporate year effect, month (seasonal) effect to get the total_log_change and forecasted UPT
forecast_components_full <- forecast_components %>%
  mutate(
    # Year centering
    yc_target = year - 2021,
    yc_ref = 2025 - 2021,
    
    # quadratic time trend coefficient
    year_effect =
      year_lin * (yc_target - yc_ref) +
      year_quad * (yc_target^2 - yc_ref^2),
    
    # monthly seasonality coefficient relative to April (reference month for UPT)
    seasonal_effect = ifelse(
      month == 4,
      0,
      month_coefs[as.character(month)] -
        month_coefs["4"] #changed
    ),
    
    # total log change
    total_log_change =
      scenario_effect +
      seasonal_effect +
      year_effect,
    
    # forecasted ridership
    forecast_upt = ref_ridership * exp(total_log_change)
  )



########### Plot Forecasts Facet Wrapped by Route ##########
# prepare base data
base_plot_df <- df_all_log %>%
  filter(year <= 2025) %>%   # or restrict further if you want
  mutate(
    upt_avg = exp(log_upt_avg),
    source = "Observed",
    month = as.integer(as.character(month)) # convert from factor to int
  ) %>%
  select(route_id, year, month, upt_avg, source)

# prepare Forecast data
forecast_plot_df <- forecast_components_full %>%
  mutate(
    upt_avg = forecast_upt,
    source = scenario   # low / medium / high
  ) %>%
  select(route_id, year, month, upt_avg, source)

### get April 2025 data for each scenario so the lines connect on the ggplot
# extract April 2025 observed ridership per route
april_2025_obs <- df_all_log %>%
  filter(year == 2025, month == 4) %>% #changed
  group_by(route_id) %>%
  summarise(upt_avg = mean(exp(log_upt_avg), na.rm = TRUE), .groups = "drop") %>%
  mutate(
    year = 2025,
    month = 4 #changed
  )

# expand for each scenario
april_2025_forecast_start <- expand_grid(
  route_id = april_2025_obs$route_id,
  scenario = names(scenario_inputs)
) %>%
  left_join(april_2025_obs, by = "route_id") %>%
  rename(source = scenario)

# combine with forecast data
forecast_plot_df_full <- bind_rows(april_2025_forecast_start, forecast_plot_df) %>%
  arrange(route_id, source, year, month)
###

# Combine both datasets
plot_df <- bind_rows(base_plot_df, forecast_plot_df_full) %>%
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-")),
    route_id = factor(route_id,
                       levels = c(3, 4, 6, 7, 8, 9, 14, 17, 18, 19, 22, 
                                  23, 28, 29, 34, 41, 42, 50, 52, 55, 
                                  56, 70, 71, 75, 76, 77, 79))
  )

# Facet wrap by route
allroutes_ridership <- ggplot(plot_df, aes(x = date, y = upt_avg, color = source)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ route_id, scales = "free_y", ncol = 4) +
  labs(
    title = "Preferred Model without ACS - Observed and Forecasted Ridership by Route", # change based on the model name
    x = NULL,
    y = "Average Weekday Ridership",
    color = "Series"
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = extended_breaks(n = 3)) +
  theme(
    axis.text.y = element_text(size = 8),
    axis.ticks.y = element_line(size = 0.2)
  )

allroutes_ridership

ggsave(filename = "../2026 Feb Update/figures/allroutes.png", plot = allroutes_ridership, width = 11, height = 7.5, units = "in")


### Plot for only one Route ###
plot_route <- plot_df %>% 
  filter(route_id == 3)

ggplot(plot_route, aes(x = date, y = upt_avg, color = source)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Observed and Forecasted Ridership for Route 3", # change based on the route
    x = NULL,
    y = "Average Weekday Ridership",
    color = "Series"
  ) +
  theme_minimal()

### Systemwide Plot ###
system_plot_df <- plot_df %>%
  group_by(year, month, source) %>%
  summarise(upt_avg = sum(upt_avg, na.rm = TRUE), .groups = "drop") %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

ggplot(system_plot_df, aes(x = date, y = upt_avg, color = source)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Preferred Model without ACS", # change based on the model name
    x = NULL,
    y = "Average Weekday Ridership") +
  scale_color_discrete(guide = "none") +
  theme_minimal()

#' If you would like a patchwork graph of the two models, run each
#' model through the "Forecasts per ChatGPT" code. Name the 
#' "plot_df" with a "_9" or whichever model you want. Then 
#' name "system_plot_df" with the same suffix. Run the 
#' forecasting code again for the other model, then rename
#' the new "plot_df" and "system_plot_df" so you have individual
#' datasets for each model. Then use the code below
# plot8 <- ggplot(system_plot_df_8, aes(x = date, y = upt_avg, color = source)) +
#   geom_line(linewidth = 0.8) +
#   labs(
#     title = "Preferred Model with ACS",
#     x = NULL,
#     y = "Average Weekday Ridership") +
#   scale_color_discrete(guide = "none") +
#   scale_y_continuous(limits = c(10000, 47000),
#                      breaks = seq(10000, 45000, by = 5000)) +
#   theme_minimal()
# 
# plot9 <- ggplot(system_plot_df_9, aes(x = date, y = upt_avg, color = source)) +
#   geom_line(linewidth = 0.8) +
#   labs(
#     title = "Preferred Model Without ACS",
#     x = NULL,
#     y = NULL,
#     color = "Series"
#   ) +
#   scale_y_continuous(limits = c(10000, 47000),
#                      breaks = seq(10000, 45000, by = 5000)) +
#   theme_minimal() +
#   theme(
#     axis.title.y = element_blank(),
#     axis.text.y  = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.line.y  = element_blank()
#   )
# 
# patchwork_ridership <- plot8+plot9
# 
# patchwork_ridership
# 
# ggsave(filename = "data/Nashville/output/patchwork_ridership.png", plot = patchwork_ridership, width = 8, height = 4, units = "in")




### Kamryn's graphs he added ###



upt_updated <- read_excel("data/Nashville/cleaneddata/upt_2025_nashville.xlsx", na = "NA")
plot_df

plot_df_updated <- upt_updated |>
  pivot_longer(cols = c(names(upt_updated)[3:29]), names_to = "route_id") |>
  mutate(date = as.Date(paste(year, month, "01", sep = "-")),
         source = "Observed") |>
  select(route_id, year, month, upt = value, source, date) |>
  count_weekdays(year_col = "year",month_col = "month") |>
  mutate(upt_avg = upt / weekdays_in_month) |>
  select(route_id, year, month, upt_avg, source, date)

df_all <- df_all %>% 
  mutate(upt_avg = upt/weekdays_in_month)

plot_df_updated <- bind_rows(plot_df, plot_df_updated)

plot_route <- plot_df_updated %>% 
  filter(route_id == 3)

# routes that were lower than prediction: 41, 70, 75, 79, 9

plot_df_updated |>
  # filter(year %in% c(2024,2025,2026,2027)) |>
  # filter(route_id == 9) |>
  ggplot(aes(x = date, y = upt_avg, color = source)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "", # change based on the route
    x = NULL,
    y = "Average Weekday Ridership",
    color = "Series"
  ) +
  scale_color_manual(values = c("#4B4B4B","#4B4B4B","#4B4B4B","#FF8200")) +
  facet_wrap(~route_id, scales = "free_y") +
  theme_bw() # +
  # theme(axis.text = element_blank())

ggsave("../2026 Feb Update/figures/all_routes_validate.png", width = 12, height = 6)

# making overall ridership prediction table
plot_df_updated |>
  filter(source != "Observed") |>
  group_by(year, month, source) |>
  summarize(upt_avg = sum(upt_avg, na.rm = T)) |>
  mutate(date = my(paste0(month,"-",year))) |>
  # filter(year >= 2025) |>
  ggplot(aes(x = date, y = upt_avg, color = source)) +
  geom_line(linewidth = 0.8) +
  labs(y = "Overall Average UPT",
       x = "Date",
       color = "Prediction Level") +
  theme_bw()

ggsave("../2026 Feb Update/figures/overall_ridership_fig.png")

total_riders <- plot_df_updated |>
  filter(source != "Observed") |>
  group_by(year, month, source) |>
  summarize(upt_avg = sum(upt_avg, na.rm = T)) |>
  mutate(date = my(paste0(month,"-",year))) |>
  ungroup() |>
  pivot_wider(values_from = upt_avg, names_from = source) |>
  select(date, High, Low, Medium)





### Input plot

make_input_variable_figures <- function(log_variables, save_folder){
  
  for (variable in log_variables){
    # make the log figure
    save_file <- paste0(save_folder,"/",variable,".png")
    
    df_all_log |>
      # filter(route_id %in% c(14,52)) |>
      filter(route_id %in% unique(df_all_log$route_id)) |>
      mutate(date = my(paste(month, year, sep = " - "))) |>
      ggplot(aes(x = date, y = .data[[variable]], color = route_id)) +
      geom_point() +
      geom_line() +
      labs(x = "Date",
           color = "Route") +
      theme_bw()
    
    ggsave(save_file)
    
    # make the non-log figure
    variable_no_log <- str_remove(variable, "^log_")
    save_file <- paste0(save_folder,"/",variable_no_log,".png")
    
    df_all |>
      # filter(route_id %in% c(14,52)) |>
      filter(route_id %in% unique(df_all$route_id)) |>
      mutate(date = my(paste(month, year, sep = " - "))) |>
      ggplot(aes(x = date, y = .data[[variable_no_log]], color = route_id)) +
      geom_point() +
      geom_line() +
      labs(x = "Date",
           color = "Route") +
      theme_bw()
    
    ggsave(save_file)
  }
  
}

# make_input_variable_figures("log_vrm", "../2026 Feb Update/input_data_plots/")

  
  
  
  