# data_xlsx <- "../data/MARTA Data/marta_input_extra_variables.xlsx"
# acs_data <- acs_data
# gas_csv <- "data/Midwest_All_Grades_All_Formulations_Retail_Gasoline_Prices.csv"
# variables <- c("[VRM]" = "log_vrm",
#               "[Month]" = "factor(month)",
#               "[Year]" = "year_cent",
#               "[Year Squared]" = "year_cent^2",
#               "[Gas Price]" = "log_gas_price",
#               "[% No Vehicle Households]" = "log_perc_hshlds_noveh",
#               "[% Workers Below Federal Poverty Line]" = "log_below_fpl",
#               "[% Commuting by Car]" = "log_perc_car",
#               "[% Commuting by Taxi]" = "log_perc_taxicab",
#               "[% Work From Home]" = "log_perc_wfh",
#               "[% Female Workers]" = "log_perc_female",
#               "[% Workers Between 100-150% of Federal Povery Level]" = "log_fpl_100_150",
#               "[% Workers in Renter Occupied Housing Units]" = "log_perc_renter_occupied",
#               "[Labor Participation Rate]" = "log_labor_part_rate",
#               "[Unemployment Rate]" = "log_unemp_rate",
#               "[Bus Rapid Transit]" = "brt",
#               "[Adult Base Fare]" = "log_fare",
#               addnl_vars)
# fare_df <- NULL
# brt_df <- NULL
#
# df_all_log[,12:14]

create_regression_model <- function(data_xlsx,
                                    acs_data,
                                    gas_csv,
                                    variables,
                                    fare_df = NULL,
                                    brt_df = NULL){

  df_all_log <- make_model_data_frame(data_xlsx, acs_data, gas_csv, fare_df, brt_df)

  candidate_variables <- variables

  rejected_var <- "placeholder"
  max_p <- .9
  n_iter <- 0
  max_iter <- length(candidate_variables) + 5

  while (max_p > 0.1){
    # exclude the previous iteration's high p-value variable
    candidate_variables <- candidate_variables[!candidate_variables %in% rejected_var]

    if (length(candidate_variables) == 0) stop("No candidate variables left to fit.")

    # create the model
    lm_vrm <- feols(as.formula(paste("log_upt_avg~", paste(candidate_variables, collapse = " + "), "| route_id")), data=df_all_log, cluster= ~route_id)

    # get vector of p-values
    pvals <- lm_vrm$coeftable[,"Pr(>|t|)"]

    # get the max p-value
    max_p <- max(pvals, na.rm = TRUE)
    if (!is.finite(max_p)) break

    # get the name of the variable with the highest p-value
    # this one will get kicked out next iteration
    rejected_var <- names(pvals)[which.max(pvals)]

    if (rejected_var == "I(year_cent^2)"){
      rejected_var <- "year_cent^2"
    } else if (grepl("month",rejected_var, ignore.case = TRUE)){
      rejected_var <- "factor(month)"
    }

    n_iter <- n_iter + 1

    if (n_iter > max_iter){
      break
    }
  }

  return(lm_vrm)
}



create_regression_model_forced <- function(data_xlsx,
                                           acs_data,
                                           gas_csv,
                                           variables,
                                           fare_df = NULL,
                                           brt_df = NULL){

  df_all_log <- make_model_data_frame(data_xlsx, acs_data, gas_csv, fare_df, brt_df)

  candidate_variables <- variables

  lm_vrm <- feols(as.formula(paste("log_upt_avg~", paste(candidate_variables, collapse = " + "), "| route_id")), data=df_all_log, cluster= ~route_id)

  return(lm_vrm)
}

# marta_data <- read_excel("../data/MARTA Data/Bus Data.xlsx", sheet = 4)
#
# lm_vrm <- feols(log_UPT ~ factor(Month) + log_VRM + log_Gas_price + log_num_0_veh + log_Mean_Vacant_Percent | Route, data=marta_data, cluster= ~Route)
# lm_vrm_2 <- feols(log_UPT ~ factor(Month) + log_VRM + log_Gas_price + log_num_0_veh + log_Mean_Vacant_Percent | Route, data=marta_data)
#
#
#
# coefs <- coef(lm_vrm)
# data.frame(variable = names(coefs),
#            coef = coefs) |>
#   gt()



