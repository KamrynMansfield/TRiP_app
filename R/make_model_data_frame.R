# fare_df <- data.frame(change_date = c("04-14-2022","06-01-2024"), prev_fare = c(2.25, 2.50), new_fare = c(2.50, 3.00))
# brt_df <- data.frame(change_date = c("05-30-2023","05-30-2023","05-30-2024"), route_changed = c(14, 17, 19))


make_model_data_frame <- function(data_xlsx,
                                    acs_data,
                                    gas_csv,
                                  fare_df = NULL,
                                  brt_df = NULL){
  # load in upt and vrm data
  data <- read_excel(data_xlsx, na = "NA")

  min_year <- min(data$year)

  # create a data frame with a time column showing the months from the first month in dataset
  time_key <- data |>
    group_by(year, month) |>
    summarise(.groups = "drop_last") |>
    ungroup() |>
    arrange(month) |>
    arrange(year)

  time_key$time <- 1:nrow(time_key)

  df <- data |>
    left_join(time_key, by = c("year","month"))

  # create column with # of weekdays per month
  df <- count_weekdays(df)

  # create centered year to later estimate time trend
  df$year_cent <- df$year - min_year

  # load in preprocessed monthly American Community Survey data
  acs <- acs_data

  # load in gas_prices data
  gas <- read_csv(gas_csv,
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

  df$route_id <- as.character(df$route_id)
  acs$route_id <- as.character(acs$route_id)

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

  # add any variables from the vrm_data
  extra_vars <- names(data)[!names(data) %in% c("route_id","month","year","upt","vrm")]
  vars_to_log <- c(vars_to_log, extra_vars)

  # log transform vars_to_log variables
  df_all_log <- df_all %>%
    mutate(across(
      all_of(vars_to_log),
      ~ if_else(. > 0, log(.), NA_real_),   # safely take log only if positive
      .names = "log_{.col}"          # rename: log_variable
    )) %>%
    select(-all_of(vars_to_log))

  # add fares column

  if (!is.null(fare_df)){
    oiriginal_fare <- fare_df |>
      filter(change_date == min(change_date)) |>
      select(prev_fare) |>
      unlist()

    df_all_log$fare <- oiriginal_fare
    df_all_log$month_numeric <- as.numeric(df_all_log$month)

    for (row_id in 1:nrow(fare_df)){
      date_used <- as.character(ymd(fare_df$change_date[[row_id]]))
      month_used <- as.numeric(month(date_used))
      year_used <- as.numeric(year(date_used))
      new_fare <- as.numeric(fare_df$new_fare[[row_id]])

      df_all_log <- df_all_log |>
        mutate(fare = case_when(
          month_numeric >= month_used & year >= year_used ~ new_fare,
          TRUE ~ fare
        ))
    }
    df_all_log$month_numeric <- NULL
  } else{
    df_all_log$fare <- 1
  }

  df_all_log$log_fare <- if_else(df_all_log$fare > 0, log(df_all_log$fare), NA_real_)

  # add brt column
  if (!is.null(brt_df)){
    df_all_log$brt <- FALSE
    df_all_log$month_numeric <- as.numeric(df_all_log$month)

    for (row_id in 1:nrow(brt_df)){
      date_used <- as.character(ymd(brt_df$change_date_brt[[row_id]]))
      month_used <- as.numeric(month(date_used))
      year_used <- as.numeric(year(date_used))
      brt_route <- as.numeric(brt_df$routes_brt[[row_id]])

      df_all_log <- df_all_log |>
        mutate(brt = case_when(
          month_numeric >= month_used & year >= year_used & route_id == brt_route ~ TRUE,
          TRUE ~ brt
        ))
    }
    df_all_log$month_numeric <- NULL
  } else{
    df_all_log$brt <- 0
  }

  return(df_all_log)
}
