############################################################

# Title: Regression - Pay Equity
# Author: Emma Broadnax
# Last Updated: 
# Notes: Build Regression Model
# Warnings:
############################################################

####################### Regression: Including Sales #######################

set.seed(123)



# use hourly rate; for us bonuses are a % of salary, so there is minimal discretion for targets for most roles.
# build initial model to get R2 threshold metrics for group on entire dataset
lm <- lm(log(hourly_rate) ~ 
           gender +
           people_mgr + 
           yrs_of_svc + 
           is_actuarial +
           pay_band_grade,
         data = df_ledger_master)

# model metrics
summary(lm)
# check variable inflation factor
vif(lm)

# report gender coefficient
(exp(coef(lm)["genderM"]) - 1) * 100


# train & run and evaluate for overfitting
# create a training/test split for the data
split_data <- initial_split(df_ledger_master ,prop = 0.75)

train_data <- training(split_data)
test_data <- testing(split_data) 

lm_model <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

# fit model to training data
lm_fit <- lm_model %>% 
  fit(log(hourly_rate) ~ 
        gender +
        people_mgr + 
        yrs_of_svc + 
        is_actuarial +
        pay_band_grade,
      data = train_data) 

summary(lm_fit$fit)
#plot(lm_fit$fit)

# evaluate large outliers 
zproc <- predict(lm_fit, new_data = train_data) %>% 
  bind_cols( train_data)%>% 
  mutate(hourly_pred = exp(.pred)) %>% 
  mutate(residual = hourly_rate - hourly_pred) %>% 
  bind_rows(train_data) %>% 
  select(name, residual, hourly_pred, hourly_rate, gender) 

# evaluate test set accuracy
df_test_results <- predict(lm_fit, new_data = test_data) %>% 
  bind_cols(test_data) %>% 
  mutate(log_hourly_rate = log(hourly_rate))

# RMSE on test set
rmse(df_test_results, 
     truth = log_hourly_rate,
     estimate = .pred)

# R2 on test set
rsq(df_test_results,
    truth = log_hourly_rate,
    estimate = .pred)

ggplot(data = data_test_results,
       mapping = aes(x = .pred, y = log_hourly_rate)) +
  geom_point(color = '#006EA1') +
  geom_abline(intercept = 0, slope = 1, color = 'orange') +
  labs(title = 'Linear Regression Results - Test Set',
       x = 'Predicted Hourly Rate',
       y = 'Actual Hourly Rate')

# Gender coefficient
(exp(coef(lm_fit$fit)["genderM"]) - 1) * 100

####################### Create Outliers #######################

# create outlier set for output
azl_outliers <- predict(lm, df_ledger_master, interval = "prediction") %>% 
  bind_cols(df_ledger_master) %>% 
  select(name,  hourly_rate, fit, lwr, upr) %>% 
  mutate(hourly_fit = exp(fit),
         hourly_lower = exp(lwr),
         hourly_upper = exp(upr),
         residual = hourly_rate - hourly_fit) %>% 
  select(-fit, -lwr, -upr)

# calculate outliers
z_q1 <- quantile(azl_outliers$hourly_fit, .25)
z_q2 <- quantile(azl_outliers$hourly_fit, .5)
z_q3 <- quantile(azl_outliers$hourly_fit, .75)

# calculate iqr 
z_iqr <- z_q3 - z_q1

# calculate thresholds 
z_outlier_upper <- z_q3 + z_iqr
z_outlier_lower <- z_q1 - z_iqr

azl_outliers <- azl_outliers %>% 
  mutate(above_below = case_when(hourly_fit < z_outlier_lower ~ "Below",
                                 hourly_fit > z_outlier_upper ~ "Above", 
                                 TRUE ~ "N/A"))

# write azl outliers
write.xlsx(azl_outliers, paste0("azl_outliers_", report_date, ".xlsx"))
