# coefs <- final_coefs
# data_xlsx <- "data/data_example.xlsx"
# acs_data <- acs
# gas_csv <- "data/Midwest_All_Grades_All_Formulations_Retail_Gasoline_Prices.csv"
# scenario_inputs_df <- elast_table
# start_year <- NULL
# start_month <- NULL

forecast_ridership <- function(coefs,
                              data_xlsx,
                              acs_data,
                              gas_csv,
                              scenario_inputs_df,
                              start_year = NULL,
                              start_month = NULL){
  
  df_all_log <- make_model_data_frame(data_xlsx, acs_data, gas_csv)
  
  min_year <- min(df_all_log$year)
  
  elasticities <- coefs[!grepl("factor\\(month\\)|year_cent", names(coefs))]
  
  month_coefs <- coefs[grepl("factor\\(month\\)", names(coefs))]
  names(month_coefs) <- gsub("factor\\(month\\)", "", names(month_coefs))
  
  # add January's month coefficient explicitly as a zero
  month_coefs <- c("1" = 0, month_coefs)
  
  # extract linear and quadratic time trend component coefficients
  year_lin <- ifelse("year_cent" %in% coefs,coefs["year_cent"], 0)
  year_quad <- ifelse("I(year_cent^2)" %in% coefs,coefs["I(year_cent^2)"],0)
  
  # get the year that the forecast will start from
  if (is.null(start_year)){
    ref_year <- max(df_all_log$year)
  } else{
    ref_year <- start_year
  }
  
  # get the month that the forecast will start from
  if (is.null(start_month)){
    ref_month <- df_all_log |>
      filter(year == ref_year) |>
      select(month) |>
      unlist() |>
      as.numeric() |>
      max()
  } else{
    ref_month <- start_month
  }
  
  # reference UPT for all routes in the given year and month
  route_reference <- df_all_log %>%
    filter(year == ref_year, month == ref_month) %>%
    group_by(route_id) %>%
    summarise(
      ref_ridership = mean(exp(log_upt_avg), na.rm = TRUE),
      .groups = "drop"
    )
  
  # prepare the scenario data frame for next step
  scenario_df <- organize_scenario_df(scenario_inputs_df)
  
  
  # Create forecast grid until December 2026
  forecast_grid <- expand_grid(
    route_id = unique(route_reference$route_id),
    year = c(ref_year, ref_year + 1),
    month = 1:12,
    scenario = c("Low","Medium","High")) %>%
    filter(!(year == ref_year & month <= ref_month)) %>%
    left_join(route_reference, by = "route_id")
  
  #' add scenarios to forecast_grid
  #' NA values are okay, it happens if the variable in the scenarios wasn't in the regression
  #' elasticity_contrib is the elasticity coef multiplied by the scenario change rate
  forecast_expanded <- forecast_grid %>%
    left_join(scenario_df, by = "scenario", relationship = "many-to-many") %>%
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
      yc_target = year - min_year,
      yc_ref = ref_year - min_year,
      
      # quadratic time trend coefficient
      year_effect =
        year_lin * (yc_target - yc_ref) +
        year_quad * (yc_target^2 - yc_ref^2),
      
      # monthly seasonality coefficient relative to April (reference month for UPT)
      seasonal_effect = ifelse(
        month == ref_month,
        0,
        month_coefs[as.character(month)] -
          month_coefs[as.character(ref_month)] 
      ),
      
      # total log change
      total_log_change =
        scenario_effect +
        seasonal_effect +
        year_effect,
      
      # forecasted ridership
      avg_daily_upt = ref_ridership * exp(total_log_change),
      
      # a variable to say it is forecasted
      forecast = TRUE
    )
  
  
  
  
  # combine forecasts and observed upt into one big df 
  final_df <- df_all_log |>
    mutate(avg_daily_upt = exp(log_upt_avg),
           scenario = "Observed",
           forecast = FALSE,
           month = as.integer(month)) |>
    select(route_id, year, month, scenario, forecast, avg_daily_upt) |>
    bind_rows(forecast_components_full)
  
  
  # create a new route that combines the upt of all the routes
  new_route_df <- final_df |>
    group_by(year, month, scenario, forecast) |>
    summarize(route_id = "all_routes",
              avg_daily_upt = sum(avg_daily_upt, na.rm = T),
              .groups = "drop_last")
  
  final_df <- bind_rows(final_df, new_route_df)
  
  # make a data frame with the reference month upt for all the scenarios
  # just to make the lines connect when we graph it
  ref_month_upt <- final_df |>
    filter(month == ref_month,
           year == ref_year) |>
    select(route_id, avg_daily_upt) |>
    mutate(forecast = TRUE)
  
  ref_upt_df <- expand_grid(route_id = unique(final_df$route_id),
                            year = ref_year,
                            month = ref_month,
                            scenario = c("Low","Medium","High")) |>
    left_join(ref_month_upt, by = "route_id")
  
  
  new_final_df <- final_df |>
    bind_rows(ref_upt_df) |>
    count_weekdays() |>
    mutate(tot_weekday_upt = avg_daily_upt * weekdays_in_month)|>
    mutate(date = ym(paste(year, month,sep = "/")))  |>
    select(route_id, year, month, avg_daily_upt,tot_weekday_upt, forecast, scenario, ref_ridership, scenario_effect, year_effect, seasonal_effect, total_log_change, date)
  
  return(new_final_df)
}
