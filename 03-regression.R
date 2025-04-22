############################################################

# Title: Regression - Pay Equity
# Author: Emma Broadnax
# Last Updated: 
# Notes: Cleans Pay Equity Dataframes
# Warnings:

############################################################

####################### Regression - new job architecture #######################

set.seed(456)

# use hourly rate; for us bonuses are a % of salary, so there is minimal discretion for targets for most roles.
lm <- lm(log_hourly_rate ~ 
           #is_people_mgr + 
           #   DR_Count +
           gender + 
           yrs_of_svc + 
           location_code + 
           pay_band + 
           career_level +
           sub_family +
          # cln_global_grade + 
           cln_business_unit  +
          # calibration_category,
           grp_age,
         data = data)

summary(lm)

vif(lm)

# Gender coefficient
(exp(coef(lm)["GenderM"]) - 1) * 100

# create a training/test split for the data
split_data <- initial_split(data ,prop = 0.75)

train_data <- training(split_data)
test_data <- testing(split_data) 

lm_model <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

# fit model to training data
lm_fit <- lm_model %>% 
  fit(Hrly_Rate ~ 
        is_people_mgr + 
        # DR_Count +
        Gender + 
        Yrs_of_Svc + 
        #Location_Code + 
        Pay_Band + 
        career_level +
        family +
        # cln_global_grade + 
        #cln_business_unit  +
        #calibration_category +
        grp_age, data = data) 
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
  mutate(residual = Hrly_Rate - .pred) %>% 
  select(residual, .pred, Hrly_Rate, family, sub_family, career_level, current_pay, everything())

# evaluate test set accuracy
data_test_results <- predict(lm_fit, new_data = test_data) %>% 
  bind_cols(test_data) 

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
(exp(coef(lm_fit$fit)["GenderM"]) - 1) * 100



####################### Regression - including Sales #######################

set.seed(456)

# use hourly rate; for us bonuses are a % of salary, so there is minimal discretion for targets for most roles.
lm <- lm(log(Hrly_Rate) ~ 
            is_people_mgr + 
        #   DR_Count +
           Gender + 
           Yrs_of_Svc + 
        #   Location_Code + 
            Pay_Band +
           # Job_Group +
        #   cln_global_grade + 
        #   cln_business_unit +
         #  calibration_category + 
           grp_age,
        data = data)

summary(lm)

vif(lm)

# Gender coefficient
(exp(coef(lm)["GenderM"]) - 1) * 100

####################### Regression - excluding Sales #######################
# Gender remains a non-significant factor in this mode, even among non-sales population.
# Cannot use pay band & business unit - they explain the same variation
lm_nosales <- lm(log(Hrly_Rate) ~ 
                   DR_Count +
                   Gender + 
                   Yrs_of_Svc +
                   is_vir_loc + 
                   #   Pay_Band + 
                   Global_Grade +
                   cln_business_unit +
                   is_over_65, 
                 data = data %>% 
                   filter(Empl_Class != 'S'))

summary(lm_nosales)

vif(lm_nosales)

# Gender coefficient
(exp(coef(lm_nosales)["GenderM"]) - 1) * 100


seniority_gap <- data %>% 
  group_by(Gender) %>% 
  summarise(count = n(),
            med_pay = median(Hrly_Rate),
            mean_pay = mean(Hrly_Rate)) 

# calculate the % difference between male and female median pay, company-wide
#(seniority_gap$med_pay[seniority_gap$Gender == "F"]-seniority_gap$med_pay[seniority_gap$Gender == "M"])/seniority_gap$med_pay[seniority_gap$Gender == "M"]
(seniority_gap$mean_pay[seniority_gap$Gender == "F"]-seniority_gap$mean_pay[seniority_gap$Gender == "M"])/seniority_gap$mean_pay[seniority_gap$Gender == "M"]


seniority_gap$med_pay[seniority_gap$Gender == "F"]
seniority_gap$med_pay[seniority_gap$Gender == "M"]
####################### Regression - individual ELG #######################

# Create a regression for each ELG area

# Define a dataframe with regression objects for ELG areas with no other business units
regression_df1 <- data %>% 
  filter(ELG %in% list_single_bu) %>%  
  group_by(ELG) %>% 
  do(model = lm(log(hourly_ttdc) ~ 
                  is_people_mgr + 
                  Gender + 
                  Yrs_of_Svc +
                  Location_Code + 
                  cln_global_grade +
                  calibration_category +
                  grp_age, data = .))

# Define a dataframe with regression objects for ELG areas with business units
regression_df2 <- data %>% 
  filter(ELG %in% list_mult_bu) %>%  
  group_by(ELG) %>% 
  do(model = lm(log(hourly_ttdc) ~ 
                  is_people_mgr + 
                  Gender + 
                  Yrs_of_Svc + 
                  Location_Code + 
                  cln_global_grade + 
                  Business_Unit +
                  calibration_category +
                  grp_age, data = .))

# Combine above dataframes to create dataframe with all ELG areas
regression_df <- regression_df1 %>% 
  rbind(regression_df2) 

# Store list of ELG Areas for use in formatting results
ELGList <- as.character(regression_df %>% pull(ELG))

# Display results
for(elg in as.character(regression_df$ELG)) {
  summary(regression_df[regression_df$ELG == elg,])
}

# Extract GenderM coefficients (converted to percents) and p-values
coeffPctList <- lapply(regression_df$model, get_pct)
coeffPctList <- unlist(coeffPctList)

# most of the ELG specific models aren't significant along gender roles
pvalueList <- lapply(regression_df$model, get_pvalue)
pvalueList <- unlist(pvalueList)

# Initialize row for whole company 
whole_company <- data.frame(ELG = c("Whole Company", "Whole Company (No Sales)"),
                            model = c(NA, NA),
                            gender_pct = c(get_pct(lm), get_pct(lm_nosales)),
                            pvalue = c(get_pvalue(lm), get_pvalue(lm_nosales)))

# Construct the regression dataframe with gender coefficients (converted to percents) and p-values
regression_df <- regression_df %>% 
  cbind(gender_pct = coeffPctList, pvalue = pvalueList) %>% # Join in coefficients and p-values for ELG areas
  rbind(whole_company) %>% # Join in values whole company 
  mutate(ss_ind = ifelse(pvalue < 0.05, 1, 0)) # Create statistically significant indicator

# Generate table
knitr::kable(regression_df %>%
               select(-model)) 


# Graph of GenderM coefficients for each ELG area including whole company
regression_df %>% 
  ggplot(aes(x = gender_pct, y = reorder(ELG, gender_pct))) + #fill = as.factor(ss_ind))) +
  geom_bar(stat = "identity", fill = "deepskyblue2") +
  theme_minimal() +
  labs(x = "", 
       y = "ELG Area", 
       title = str_wrap("Percent Higher in Male Employee Hourly Rate While Accounting for Other Factors", 60)) + 
  geom_text(aes(label=scales::percent(gender_pct, accuracy = 0.1L)), 
            position=position_dodge(width=0.9), size = 3, hjust = 0) +
  expand_limits(x = 0.11) +
  theme(axis.text.x=element_blank())

####################### Regression - individual job families #######################

# create a list of job families
list.family <- data %>% 
  group_by(family) %>% 
  summarise(count = n()) %>% 
  filter(count >= 50) %>% 
  pull(family) 
  

# Define a dataframe with regression objects for ELG areas with no other business units
lm_family <- data %>% 
  filter(family %in% list.family) %>%  
  group_by(family) %>% 
  do(model = lm(Hrly_Rate ~ 
                  #is_people_mgr + 
                  #   DR_Count +
                  Gender + 
                  Yrs_of_Svc + 
                  Location_Code + 
                  Pay_Band + 
                  career_level +
                  sub_family +
                  # cln_global_grade + 
                  #cln_business_unit  +
                  # calibration_category,
                  grp_age,
                data = .))

df_lm_family <- rbind(lm_family)


# Display results
for(family in as.character(df_lm_family$family)) {
  summary(df_lm_family[df_lm_family$family == family,])
}


# Extract GenderM coefficients (converted to percents) and p-values
coeffPctList <- lapply(df_lm_family$model, get_pct)
coeffPctList <- unlist(coeffPctList)

# most of the ELG specific models aren't significant along gender roles
pvalueList <- lapply(df_lm_family$model, get_pvalue)
pvalueList <- unlist(pvalueList)

# Initialize row for whole company 
whole_company <- data.frame(ELG = c("Whole Company", "Whole Company (No Sales)"),
                            model = c(NA, NA),
                            gender_pct = c(get_pct(lm), get_pct(lm_nosales)),
                            pvalue = c(get_pvalue(lm), get_pvalue(lm_nosales)))

# Construct the regression dataframe with gender coefficients (converted to percents) and p-values
regression_df <- df_lm_family %>% 
  cbind(gender_pct = coeffPctList, pvalue = pvalueList) %>% # Join in coefficients and p-values for ELG areas
 # rbind(whole_company) %>% # Join in values whole company 
  mutate(ss_ind = ifelse(pvalue < 0.05, 1, 0)) # Create statistically significant indicator

# Generate table
knitr::kable(regression_df %>%
               select(-model)) 


# Graph of GenderM coefficients for each ELG area including whole company
regression_df %>% 
  ggplot(aes(x = gender_pct, y = reorder(ELG, gender_pct))) + #fill = as.factor(ss_ind))) +
  geom_bar(stat = "identity", fill = "deepskyblue2") +
  theme_minimal() +
  labs(x = "", 
       y = "ELG Area", 
       title = str_wrap("Percent Higher in Male Employee Hourly Rate While Accounting for Other Factors", 60)) + 
  geom_text(aes(label=scales::percent(gender_pct, accuracy = 0.1L)), 
            position=position_dodge(width=0.9), size = 3, hjust = 0) +
  expand_limits(x = 0.11) +
  theme(axis.text.x=element_blank())

####################### Multi-collinearity Testing #######################

list_model <- regression_df$model[(regression_df$ELG != "Whole Company") & (regression_df$ELG != "Whole Company (No Sales)")]

# Check if any variables in each regression model are perfectly correlated
aliasList <- lapply(list_model, function(x) alias(x))
aliasList

# Calculate the variation inflation factors of all predictors in each regression model
vifList <- lapply(list_model, function(x) vif(x))
vifList


####################### Graphs #######################

data_all %>% 
  ggplot(aes(x = hourly_ttdc, y = Yrs_of_Svc, color = Gender)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x)

# bigger discrepancy among non-people mgrs
data_all %>% 
  ggplot(aes(x = hourly_ttdc, y = Gender)) +
  geom_boxplot() +
  facet_wrap(~People_Mgr)

data_all %>% 
  ggplot(aes(x = hourly_ttdc, y = Age, color =)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x)


# visualize data - Actuarial
data_actuarial %>% 
  ggplot(aes(x = Hrly_Rate, y = Gender)) +
  geom_boxplot() +
  geom_smooth(method='lm', formula= y~x) +
  #facet_wrap(~is_people_mgr)
  facet_wrap(~Pay_Band)

data_actuarial %>% 
  ggplot(aes(x = Hrly_Rate, y = Gender)) +
  geom_violin() +
  geom_smooth(method='lm', formula= y~x) +
  #facet_wrap(~is_people_mgr)
  facet_wrap(~Pay_Band)

data_actuarial %>% 
  group_by(Gender, Pay_Band) %>% 
  summarise(count = n(),
            avg_pay = mean(Hrly_Rate),
            med_pay = median(Hrly_Rate)) %>% 
  arrange(Pay_Band, Gender)

data_actuarial <- data %>% 
  filter(ELG == "Actuarial")

data_actuarial %>% 
  count(Gender)

lm_act <- lm(Hrly_Rate ~ Pay_Band + Gender, data = data_actuarial)
summary(lm_act)
