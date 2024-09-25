############################################################

# Title: Clean AZRA - Pay Equity
# Author: Emma Broadnax
# Last Updated: 09/17/2024
# Notes: Cleans Pay Equity Dataframes
# Warnings: null global grade is currently corrected manually

############################################################

####################### Clean Data #######################

# filter ledger to AZL employees
df_ledger_master_azra <- df_ledger_master %>% 
  filter(co_descr == "Allianz Reinsurance America Inc")

df_exec_rev_azra <- in_exec_rev_azra %>% 
  filter(!is.na(employee_id)) %>% 
  mutate(employee_id = as.character(employee_id)) %>% 
  dplyr::select(employee_id, calibration_category)


####################### Join: Executive Review #######################

df_master_azra <- df_ledger_master_azra %>%
  filter(aip_target_percent < 20) %>%  # above 20 is AVP
  left_join(df_exec_rev_azra, by = c("id" = "employee_id")) %>% 
  mutate(aip_target_percent = as.factor(aip_target_percent),
         calibration_category = as.factor(calibration_category),
         is_in_ca = as.factor(if_else(home_state == "CA", 1, 0)),
         is_in_ga = as.factor(if_else(home_state == "GA", 1, 0)),
         is_in_ma = as.factor(if_else(home_state == "MA", 1, 0)),
         #is_under_45 = as.factor(if_else(age < 45, 1, 0)),
         is_workers_comp = as.factor(if_else(department_description == "AZRA Workers Compensation", 1, 0)),
         is_asbestos = as.factor(if_else(department_description == "AZRA Asbestos &amp; Pollution", 1, 0)),
         is_finance = as.factor(if_else(department_description == "AZRA Finance", 1, 0)),
         is_constr = as.factor(if_else(department_description == "AZRA Construction Defect", 1, 0))) %>% 
  mutate(cln_aip_target_percent = case_when(aip_target_percent == '5' ~ '999',
                                            aip_target_percent == '7.5' ~ '999',
                                            TRUE ~ aip_target_percent)) 

####################### Test Files #######################

stopifnot(nrow(data) > 0)
