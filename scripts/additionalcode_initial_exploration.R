################ GTFS and Monthy Dummies ##########
# load in GTFS dummies
gtfs <- read_csv("data/Nashville/GTFS_dummies.csv") %>% 
  pivot_longer(cols = F20_F21:S25_W26,
               names_to = "gtfs",
               values_to = "value") %>% 
  filter(value == 1) %>% 
  select(-value)

# left_join GTFS dummies in replacement of monthly dummies
df <- df %>% 
  left_join(gtfs, by = c("month", "year"))

# make monthly dummy cols
df <- df  %>%
  mutate(
    month_2  = as.integer(month == 2),
    month_3  = as.integer(month == 3),
    month_4  = as.integer(month == 4),
    month_5  = as.integer(month == 5),
    month_6  = as.integer(month == 6),
    month_7  = as.integer(month == 7),
    month_8  = as.integer(month == 8),
    month_9  = as.integer(month == 9),
    month_10 = as.integer(month == 10),
    month_11 = as.integer(month == 11),
    month_12 = as.integer(month == 12))

############### Pre-model Adjustments #############
# remove route 64 and 79 because of missing first 3 months
df <- df %>%
 filter(!route %in% c(79, 64))

# replace all NA values with 0
df <- df %>%
  replace(is.na(.), 0)

############ Correlation Plots ###################

## Correlation plots

# library(corrplot)
train_numeric <- train_df %>%
  select("log_upt_avg", "log_vrm","log_gas_price","log_population",
         "log_labor_part_rate","log_perc_car",
         "log_perc_wfh","log_unemp_rate",
         "log_perc_renter_occupied","log_perc_female",
         "log_fpl_100_150","log_perc_hshlds_noveh",
         "log_below_fpl","log_perc_taxicab")
train_cor <- cor(train_numeric, use = "pairwise.complete.obs")
corrplot(train_cor, tl.cex = 0.6)

# Look at fitted and residual information
#Step 1: Get residuals of log_upt_avg after controlling for everything except log_population
res_y <- resid(feols(as.formula(
  paste("log_upt_avg ~", paste(setdiff(log_reg_1, "log_population"), collapse = " + "), "+ log_vrm | route_id + month + year")
), data = train_df))

# Step 2: Get residuals of log_population after controlling for everything else
res_x <- resid(feols(as.formula(
  paste("log_population ~", paste(setdiff(log_reg_1, "log_population"), collapse = " + "), "+ log_vrm | route_id + month + year")
), data = train_df))

# Create a data frame with residuals
partial_df <- data.frame(
  res_x = res_x,
  res_y = res_y
)

# Plot
ggplot(partial_df, aes(x = res_x, y = res_y)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "blue") +
  labs(
    x = "log_population (residualized)",
    y = "log_upt_avg (residualized)",
    title = "Partial relationship between log_population and log_upt_avg"
  ) +
  theme_minimal()


############### Save route FE to excel ############
library(writexl)
write_xlsx(route_fe, "data/Nashville/output/route_fixed_effects.xlsx")

# save output as picture
#library(gt)
#gtsave(gt_table, filename = "data/Nashville/output/khattakmodeloutput.png")


################ RMSE and MAPE ##########################
# Forecast 2025 Data
test_df$predicted_log_upt <- predict(lm_vrm_7, newdata = test_df)
test_df$predicted_upt <- exp(test_df$predicted_log_upt)
test_df$upt_avg <- exp(test_df$log_upt_avg)

# compare to actual
ggplot(test_df, aes(x = upt_avg, y = predicted_upt, color = factor(route_id))) +
  geom_point(size=2, alpha = 1) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x = "Actual Monthly Average Weekday UPT",
    y = "Predicted Monthly Average Weekday UPT",
    color = "Route ID") +
  theme_classic()

# Compute RMSE
rmse <- sqrt(mean((test_df$upt_avg - test_df$predicted_upt)^2, na.rm = TRUE))
print(rmse)

# Compute RMSE by route with ridership
# First compute average monthly weekday ridership per route
avg_ridership <- test_df %>%
  group_by(route_id) %>%
  summarise(
    avg_monthly_upt = mean(upt_avg, na.rm = TRUE),
    .groups = "drop"
  )

# Compute RMSE by route
rmse_by_route <- test_df %>%
  group_by(route_id) %>%
  summarise(
    RMSE = sqrt(mean((upt_avg - predicted_upt)^2, na.rm = TRUE)),
    .groups = "drop"
  )

# Compute MAPE by route
mape_by_route <- test_df %>%
  mutate(abs_pct_error = abs((upt_avg - predicted_upt) / upt_avg)) %>%
  group_by(route_id) %>%
  summarise(
    MAPE = mean(abs_pct_error, na.rm = TRUE),
    avg_monthly_upt = mean(upt_avg, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_monthly_upt))  # Optional: sort by average ridership


# Join the two
rmse_mape_ridership <- rmse_by_route %>%
  left_join(mape_by_route, by = "route_id") %>%
  arrange(desc(avg_monthly_upt))  # Sort by ridership

write_xlsx(rmse_mape_ridership, "data/Nashville/output/mape_rmse_by_route.xlsx")

# View MAPE v. RMSE by UPT
ggplot(rmse_mape_ridership, aes(x = RMSE, y = MAPE, color = avg_monthly_upt)) +
  geom_point(size = 5, alpha = 1) +
  scale_color_viridis_c(name = "Monthly Avg Weekday UPT", option = "turbo") +
  labs(
    x = "RMSE",
    y = "MAPE"
  ) +
  theme_classic()

# Make interactive
library(plotly)
p <- ggplot(rmse_mape_ridership, aes(
  x = RMSE,
  y = MAPE,
  color = avg_monthly_upt,
  text = paste0("Route: ", route_id, 
                "<br>RMSE: ", round(RMSE, 1), 
                "<br>MAPE: ", round(MAPE, 1), "%",
                "<br>Avg Monthly UPT: ", round(avg_monthly_upt))
)) +
  geom_point(size = 5, alpha = 1) +
  scale_color_viridis_c(name = "Monthly Avg Weekday UPT", option = "turbo") +
  labs(
    x = "RMSE",
    y = "MAPE",
    title = "Prediction Error by Route"
  ) +
  theme_classic()

ggplotly(p, tooltip = "text")


###########################################################
###########################################################
###########################################################

# Extract the logical vector of rows used in the estimation
obs_used <- lm_vrm_1$obs_selection[[1]]

# Now safely assign predicted values
train_df$predicted <- NA
train_df$predicted[obs_used] <- predict(lm_vrm_1)

library(ggplot2)
library(plotly)

p <- ggplot(train_df, aes(
  x = predicted,
  y = log_upt_avg,
  color = factor(route_id),
  text = paste0("Route ID: ", route_id, "<br>Month: ", month, "<br>Predicted: ", round(predicted, 2), "<br>Actual: ", round(log_upt_avg, 2))
)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "gray40", linetype = "dashed") +
  scale_color_viridis_d(option = "turbo") +
  labs(
    title = "Predicted vs Actual (log ridership)",
    x = "Predicted log ridership",
    y = "Actual log ridership",
    color = "Route ID"
  ) +
  theme_minimal()

# Convert to interactive plot
ggplotly(p, tooltip = "text")

# non-interactive plot
ggplot(train_df, aes(
  x = predicted,
  y = log_upt_avg,
  color = factor(route_id)
)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "gray40", linetype = "dashed") +
  scale_color_viridis_d(option = "turbo") +
  labs(
    title = "Predicted vs Actual (log ridership)",
    x = "Predicted log ridership",
    y = "Actual log ridership",
    color = "Route ID"
  ) +
  theme_classic()







library(rlang)

plot_by_route <- function(df, y_var, y_label, plot_title) {
  ggplot(df, aes(
    x = time,
    y = !!sym(y_var),
    color = factor(route_id)
  )) +
    geom_line(alpha = 0.6) +
    geom_point(alpha = 0.6, size = 2) +
    labs(
      title = plot_title,
      x = "Time Period",
      y = y_label,
      color = "Route ID") +
    scale_x_continuous(
      breaks = seq(from = 0, to = 52, by = 12),  # major breaks every 12
      minor_breaks = seq(from = 0, to = 52, by = 6)) +
    theme_minimal()
}

library(patchwork)

p1 <- plot_by_route(df_all, "perc_wfh", "Percent Work from Home", "Interpolated Percent Work from Home Data by Route")
plot_by_route(train_df, "log_perc_wfh", "Log Percent Work from Home", "Interpolated Log Percent Work from Home Data by Route")

p2 <- plot_by_route(df_all, "upt_avg", "Average Monthly Weekday UPT", "Average Monthly Weekday UPT by Route")
plot_by_route(df_all, "vrm", "VRM", "VRM by Route")
plot_by_route(df_all, "vrh", "VRH", "VRH by Route")

plot_by_route(df_all, "perc_renter_occupied", "Gas Price ($)", "Gas Price over Time")


p1 + p2 + plot_layout(guides = "collect")










# diagnostics
qqnorm(residuals(lm_vrm_8), ylab = 'Residuals')
qqline(residuals(lm_vrm_8))

library(tidyverse)
autoplot(lm_vrm_3)

# no visible collinearity
collinearity(lm_vrm_1, verbose = TRUE)

resids <- resid(lm_vrm_8)
# Histogram of residuals
hist(resids, breaks = 50, main = "Histogram of Residuals")

# QQ plot
qqnorm(resids)
qqline(resids, col = "red")

#Residuals v. Fitted - heteroskedasticity
plot(fitted(lm_vrm_8), resid(lm_vrm_8),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

plot(lm_vrm_1, which = "qq") # alternative 

# wooldridge test
library(plm)
pdata <- pdata.frame(train_df, index = c("route_id"))
pwartest(log_upt_avg~log_vrm+factor(month)+time+log_gas_price+log_perc_car+log_perc_taxicab+log_perc_wfh+log_perc_female+log_below_fpl+log_fpl_100_150+log_perc_renter_occupied+log_perc_no_veh+log_population+log_labor_part_rate+log_unemp_rate, data = train_df)


### scale location plot
resids <- resid(lm_vrm_1)
fitted_vals <- fitted(lm_vrm_1)

# Standardize residuals (mean 0, sd 1)
std_resids <- scale(resids)

# Compute square root of absolute standardized residuals
sqrt_std_resids <- sqrt(abs(std_resids))

# Plot
plot(fitted_vals, sqrt_std_resids,
     xlab = "Fitted values",
     ylab = expression(sqrt("|Standardized residuals|")),
     main = "Scale-Location Plot",
     pch = 20, col = "darkblue")
abline(h = mean(sqrt_std_resids), col = "red", lty = 2)




vrm_1 <- feols(upt_avg~vrm+factor(month)+time+gas_price+perc_car+perc_taxicab+perc_wfh+perc_female+below_fpl+fpl_100_150+perc_renter_occupied+perc_no_veh+population+labor_part_rate+unemp_rate | route_id, data=df_all, cluster= ~route_id)









# Extract fixed effects
route_fe <- get_fe(lm_vrm_1, "route_id")

month_fe <- get_fe(lm_vrm_6, "month")

ggplot(train_df, aes(x = log_median_earnings, y = log_upt_avg, color = factor(route_id))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Relationship between Log UPT Avg and Log Median Earnings",
    x = "Log Median Earnings",
    y = "Log UPT Avg",
    color = "Route"
  ) +
  theme_minimal()

ggplot(train_df, aes(x = log_vrm, y = log_median_earnings, color = factor(route_id))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Relationship between Log Median Earnings and Log VRM",
    x = "Log VRM",
    y = "Log Median Earnings",
    color = "Route"
  ) +
  theme_minimal()

# Use this model and graph
df_route <- train_df %>% 
  filter(route_id == 3)

library(plm)
plm(
  log_upt_avg ~ log_population,
  data = train_df,
  index = "route_id",
  model = "between"
)

library(ggrepel)

route_labels <- train_df %>%
  group_by(route_id) %>%
  filter(log_population == max(log_population)) %>%
  ungroup()

ggplot(train_df, aes(x = log_population, y = log_upt_avg, color = factor(route_id))) +
  geom_point(alpha = 0.6) +
  geom_line(aes(colour = route_id), linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_text_repel(
    data = route_labels,
    aes(label = route_id),
    nudge_x = 0.06,
    direction = "y",
    size = 5,
    segment.color = "grey40",     # connector line color
    segment.size = 0.1,           # connector thickness
    segment.alpha = 0.2,          # connector transparency
    force = 0.3,                  # repel strength
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  labs(
    title = "Relationship between Log UPT Avg and Log Population",
    x = "Log Population",
    y = "Log UPT Avg",
    color = "Route"
  ) +
  theme_minimal()

ggplot(train_df, aes(x=time, y = log_population, color=log_upt_avg)) +
  geom_point() +
  scale_color_gradient(low = "black", high = "red")

ggplot(df_og, aes(x = vrm, y = upt, color = factor(route))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Relationship between VRM and UPT",
    x = "VRM",
    y = "UPT",
    color = "Route"
  ) +
  theme_minimal()

df_og_demean <- df_og %>%
  group_by(route) %>%
  mutate(
    vrm_demeaned = vrm - mean(vrm),
    upt_demeaned = upt - mean(upt)
  )

ggplot(df_og_demean, aes(x = vrm_demeaned, y = upt_demeaned)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Within-Route Relationship Between VRM and UPT",
    x = "VRM (demeaned)",
    y = "UPT (demeaned)"
  ) +
  theme_minimal()

df_log_demean <- train_df %>%
  group_by(route_id) %>%
  mutate(
    vrm_demeaned = log_vrm - mean(log_vrm),
    upt_demeaned = log_upt_avg - mean(log_upt_avg),
    earnings_demeaned = log_median_earnings - mean(log_median_earnings),
    wfh_demeaned = log_perc_wfh - mean(log_perc_wfh)
  )

ggplot(df_log_demean, aes(x = wfh_demeaned, y = vrm_demeaned, color = factor(year))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Within-Route Relationship Between Log Work from Home and Log VRM",
    x = "Log Work from Home (demeaned)",
    y = "Log VRM (demeaned)"
  ) +
  theme_minimal()

# run correlation matrix
library(corrplot)
df_log_corr <- train_df %>%
  select(time, log_upt:log_unemp_rate) 

cor_matrix <- cor(df_log_corr, use = "complete.obs", method = "pearson")
print(cor_matrix)

png("data/Nashville/output/corr_plot.png", width = 1200, height = 1000)

corrplot(cor_matrix, 
         method = "color",       # Color-coded squares
         type = "upper",         # Show upper triangle only
         tl.col = "black",       # Text label color
         tl.cex = 1,           # Text size
         addCoef.col = "black",  # Add correlation coefficients
         number.cex = 1,       # Size of numbers
         diag = FALSE)           # Hide diagonal

dev.off()

cor_df <- as.data.frame(as.table(cor_matrix)) %>%
  filter(Var1 != Var2) %>%                      # remove self-correlations
  filter(abs(Freq) > 0.8) %>%                   # filter for high correlation (adjust threshold)
  arrange(desc(abs(Freq)))

# Step 3: Remove duplicate pairs (A-B and B-A)
cor_df_unique <- cor_df %>%
  mutate(pair = pmap_chr(list(Var1, Var2), ~ paste(sort(c(..1, ..2)), collapse = "_"))) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(Var1, Var2, Correlation = Freq)

print(cor_df_unique)








model_logvrh <- feols(
  logupt~logvrh+month | route+year, 
  data=df_log, 
  cluster= ~route)

summary(model_logvrm)

summary(model_logvrh)











### Tests
#--------------Fixed effects paneldata model
fixed <- plm(upt ~ vrm + time + month, data = pdata, model = "within")
summary(fixed)

#--------------Random effects paneldata model
random <- plm(upt ~ vrm + time + month, data = pdata, model="random")
summary(random)

#--------------Diagnostics
phtest(fixed, random) # Fixed-effects model better than random-effects model
plmtest(fixed, c("time"), type=("bp")) # Need of time-fixed effects
pcdtest(fixed, test = c("lm")) # Test indicates cross-sectional dependance
pcdtest(fixed, test = c("cd")) # Test indicates cross-sectional dependance
pbgtest(fixed) # Test indicates serial correlation

