############################################################

# Title: Regression - Pay Equity
# Author: Emma Broadnax
# Last Updated: 
# Notes: Cleans Pay Equity Dataframes
# Warnings:
############################################################

####################### Explore Data #######################

df_master_azra %>% 
  ggplot(aes(x = yrs_of_svc, y = hourly_rate, group = gender, color = gender)) +
  geom_point() +
  geom_smooth(method ="lm")

# could a sampling technique help the model performance?
# distributions are consistent except at 15, where there are only 19 observations?
df_master_azra %>% 
  ggplot(aes(x = gender, y = hourly_rate)) +
  geom_boxplot() +
  facet_wrap(~aip_target_percent)

# look at this - not a significant coefficient in the model but should be (is this because of parity among F?)
df_master_azra %>% 
  ggplot(aes(x = gender, y = hourly_rate)) +
  geom_boxplot() +
  facet_wrap(~is_in_ca)

df_master_azra %>% 
 # filter(gender == "F") %>% 
  group_by(gender, is_in_ca) %>% 
  summarise(avg_pay = mean(hourly_rate),
            med_pay = median(hourly_rate),
            sd_pay = sd(hourly_rate)) %>% 
  pivot_wider(names_from = is_in_ca, values_from = c("avg_pay", "med_pay", "sd_pay"))

df_master_azra %>% 
  ggplot(aes(x = gender, y = hourly_rate)) +
  geom_boxplot() +
  facet_wrap(~age_group)

df_master_azra %>% 
  group_by(gender) %>% 
  summarise(avg_pay = mean(hourly_rate),
            std_dv = sd(hourly_rate),
            med_pay = median(hourly_rate),
            count = n())

# set.seed(2024)
# 
# lm <- lm(log_hourly_rate ~ 
#            gender +
#            #people_mgr + 
#            calibration_category +
#            is_workers_comp +
#            is_finance +
#            #is_in_ca +
#            is_in_ma +
#            #is_in_ga +
#            #is_constr + 
#            is_asbestos + 
#            #is_under_45 + 
#            cln_aip_target_percent,
#          data = df_master_azra)
# 
# summary(lm)

####################### Logistic Linear Regression #######################

# set seed
set.seed(2024)

# create a training/test split for the data
split_data <- initial_split(df_master_azra ,prop = 0.75)

train_data <- training(split_data) 
test_data <- testing(split_data) 

# define model
lm_model <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

# define formula to predict log hourly rate
formula <- log_hourly_rate  ~ 
  gender + 
  calibration_category +
  is_in_ma +
  is_in_ca +
  is_in_ga +
  is_workers_comp +
  is_asbestos + 
  #is_under_45 +
  #is_finance +
  cln_aip_target_percent

# conduct pre-processing needed
lm_recipe <- recipe(formula, data = train_data) %>%
  step_relevel(gender, ref_level = "M") %>% 
  step_relevel(cln_aip_target_percent, ref_level = "999") %>%
  step_relevel(calibration_category, ref_level = "At =") %>% 
  step_relevel(is_in_ma, ref_level = "0") %>% 
  step_relevel(is_in_ca, ref_level = "0") %>% 
  step_relevel(is_asbestos, ref_level = "0") %>% 
  step_relevel(is_workers_comp, ref_level = "0") %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_zv(all_numeric_predictors())

lm_wf <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(lm_recipe)

wf_fit_train <- lm_wf %>% 
  fit(train_data)

wf_fit_test <- lm_wf %>% 
  fit(test_data)

wf_fit_all <- lm_wf %>% 
  fit(df_master_azra)

# rsquare on training
glance(wf_fit_train)
glance(wf_fit_test)
glance(wf_fit_all)

# evaluate VIF
pay_model <- wf_fit_train %>% 
  extract_fit_engine()



test_fit <- predict(wf_fit_train, test_data)

pay_preds <- 
  augment(wf_fit_train, test_data)

pay_preds <- pay_preds %>%
  dplyr::select(job_title, cln_aip_target_percent, log_hourly_rate, .pred, .resid)


# plot residuals
ggplot(data = pay_preds,
       mapping = aes(x = .pred, y = log_hourly_rate)) +
  geom_point(color = '#006EA1') +
  geom_abline(intercept = 0, slope = 1, color = 'orange') +
  labs(title = 'Linear Regression Results - Advertising Test Set',
       x = 'Predicted Hourly Rate',
       y = 'Actual Hourly Rate')

####################### Logistic Linear Regression #######################

# set seed
set.seed(2024)

# create a training/test split for the data
split_data <- initial_split(df_master_azra ,prop = 0.75)

train_data <- training(split_data) 
test_data <- testing(split_data) 

# define model
lm_model <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

# define formula to predict log hourly rate
formula <- log_hourly_rate  ~ 
  gender + 
  calibration_category +
  is_workers_comp +
  is_asbestos + 
  #is_finance +
  cln_aip_target_percent

# conduct pre-processing needed
lm_recipe <- recipe(formula, data = train_data) %>%
  step_relevel(gender, ref_level = "M") %>% 
  step_relevel(cln_aip_target_percent, ref_level = "999") %>%
  step_relevel(calibration_category, ref_level = "At =") %>% 
  step_relevel(is_in_ma, ref_level = "0") %>% 
  step_relevel(is_in_ca, ref_level = "0") %>% 
  step_relevel(is_asbestos, ref_level = "0") %>% 
  step_relevel(is_workers_comp, ref_level = "0") %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_zv(all_numeric_predictors())

lm_wf <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(lm_recipe)

wf_fit_train <- lm_wf %>% 
  fit(train_data)

wf_fit_test <- lm_wf %>% 
  fit(test_data)

wf_fit_all <- lm_wf %>% 
  fit(df_master_azra)

# rsquare on training
glance(wf_fit_train)
glance(wf_fit_test)
glance(wf_fit_all)

# evaluate VIF
pay_model <- wf_fit_train %>% 
  extract_fit_engine()

test_fit <- predict(wf_fit_train, test_data)

pay_preds <- 
  augment(wf_fit_train, test_data)

pay_preds <- pay_preds %>%
  dplyr::select(job_title, cln_aip_target_percent, log_hourly_rate, .pred, .resid)


# plot residuals
ggplot(data = pay_preds,
       mapping = aes(x = .pred, y = log_hourly_rate)) +
  geom_point(color = '#006EA1') +
  geom_abline(intercept = 0, slope = 1, color = 'orange') +
  labs(title = 'Linear Regression Results - Advertising Test Set',
       x = 'Predicted Hourly Rate',
       y = 'Actual Hourly Rate')


####################### Outliers: AZRA #######################


# compute outlier dataset
predictions <- predict(lm, new_data = azra_data, interval = "prediction", level = 0.9)

outliers_azra <- df_master_azra %>% 
  mutate(log_hrly_rate = log(hourly_rate)) %>% # Create a variable calculating log of Hrly_Rate
  cbind(pred_int = predictions) %>%  # Join in prediction interval
  mutate(pred_fit_amt = exp(pred_int.fit), # Convert prediction to dollar amount
         pred_lower_amt = exp(pred_int.lwr), # Convert lower bound of prediction interval to dollar amount
         pred_upper_amt = exp(pred_int.upr), # Convert upper bound of prediction interval to dollar amount
         # Create indicator for whether actual is under or over the bounds of the prediction interval
         anom_ind = as.character(ifelse(log_hrly_rate < pred_int.lwr, "Under", ifelse(log_hrly_rate > pred_int.upr, "Over", 0)))) %>% 
  filter(anom_ind == "Under" | anom_ind == "Over") %>% # Filter by employees that are under or over (outliers)
  select(-c("pred_int.fit", "pred_int.upr", "pred_int.lwr", "log_hrly_rate"))

df_master_azra %>% 
  filter(job_title == "Claims Adjuster III") %>% 
  ggplot(aes(x = log(yrs_of_svc), y = (annual_rt), color = gender)) +
  geom_point() +
  geom_smooth( method = "lm", se = FALSE)
