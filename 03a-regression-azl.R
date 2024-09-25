############################################################

# Title: Regression - Pay Equity
# Author: Emma Broadnax
# Last Updated: 
# Notes: Cleans Pay Equity Dataframes
# Warnings:
############################################################

####################### Regression: Including Sales #######################

set.seed(456)

# use hourly rate; for us bonuses are a % of salary, so there is minimal discretion for targets for most roles.
lm <- lm(log(hourly_rate) ~ 
           gender +
           people_mgr + 
           #   DR_Count +
           yrs_of_svc + 
           #   Location_Code + 
           pay_band_grade +
           # Job_Group +
           #   cln_global_grade + 
           #   cln_business_unit +
           #  calibration_category + 
           age_group,
         data = df_master_azl)

summary(lm)
# check variable inflation factor
vif(lm)

# report gender coefficient
(exp(coef(lm)["genderM"]) - 1) * 100

# create a training/test split for the data
split_data <- initial_split(df_master_azl ,prop = 0.75)

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
           #   DR_Count +
           yrs_of_svc + 
           #   Location_Code + 
           pay_band_grade +
           # Job_Group +
           #   cln_global_grade + 
           #   cln_business_unit +
           #  calibration_category + 
           age_group,
         data = df_master_azl)
#  step_dummy(all_nominal_predictors()) %>% 
#  step_zv(all_predictors()) %>% 
#  step_normalize(all_predictors())

summary(lm_fit$fit)
plot(lm_fit$fit)

# variable importance
vip(lm_fit)

# evaluate large outliers 
zproc <- predict(lm_fit, new_data = train_data) %>% 
  bind_cols( train_data)%>% 
  mutate(residual = hourly_rate - .pred) %>% 
  select(residual, .pred, hourly_rate,)

# evaluate test set accuracy
data_test_results <- predict(lm_fit, new_data = test_data) %>% 
  bind_cols(test_data) %>% 
  mutate(log_hourly_rate = log(hourly_rate))

# RMSE on test set
rmse(data_test_results, 
     truth = log_hourly_rate,
     estimate = .pred)

# R2 on test set
rsq(data_test_results,
    truth = log_hourly_rate,
    estimate = .pred)

ggplot(data = data_test_results,
       mapping = aes(x = .pred, y = log_hourly_rate)) +
  geom_point(color = '#006EA1') +
  geom_abline(intercept = 0, slope = 1, color = 'orange') +
  labs(title = 'Linear Regression Results - Advertising Test Set',
       x = 'Predicted Hourly Rate',
       y = 'Actual Hourly Rate')

# Gender coefficient
(exp(coef(lm_fit$fit)["genderM"]) - 1) * 100
