############################################################

# Title: Clean - Pay Equity
# Author: Emma Broadnax
# Last Updated: 
# Notes: Cleans Pay Equity Dataframes
# Warnings: null global grade is currently corrected manually

# change from work location to state - AZRA employees are spread
# look at geozone
# try adding talent segments
# try including AZRA department for grouping (break apart the claims people in particular)

############################################################

####################### Clean Ledger Input #######################

df_ledger <- in_ledger %>% 
  # remove temporary employees
  # remove officers
  # remove inpat/expats
  # remove interns
  filter(reg_temp == "R" &
           officer == "N" &
           pay_band_grade != "Z" &
           co_descr %in% company.list) %>% 
  filter(empl_class %in% c("M", "S")) %>% 
  # adjust pay bands for geographic differentials
  mutate(dr_count = replace_na(dr_count, 0),
         aip_target_percent = replace_na(aip_target_percent, 0),
         age_group = as.factor(cut(age, breaks = age_groups, include.lowest = TRUE)),
         target_aip = aip_target_percent * annual_rt,
         hourly_rate = annual_rt/(std_hrs_wk*52),
         log_hourly_rate = log(hourly_rate),
         hourly_ttdc = (annual_rt + target_aip)/(std_hrs_wk * 52)) %>%
  # group low  grade counts
  mutate(pay_band_grade = case_when(pay_band_grade == "A8A" ~ "A8",
                                    pay_band_grade == "A9A" ~ "A9",
                                    TRUE ~ pay_band_grade)) %>% 
  # convert column types
  mutate(job_code = as.factor(job_code),
         empl_class = as.factor(empl_class)) %>% 
  dplyr::select(id,
                name,
                job_code,
                dr_count,
                people_mgr,
                empl_class,
                pay_band_grade,
                fte,
                yrs_of_svc,
                age,
                age_group,
                annual_rt,
                target_aip,
                hourly_rate,
                log_hourly_rate,
                hourly_ttdc,
                aip_target_percent,
                x2nd_lvl_mgr,
                gender,
                location_code,
                #work_state, # defaults to MN in most cases
                home_state,
                business_unit,
                department_description,
                co_descr)

####################### Clean Job Codes #######################

df_jobs <- in_jobs %>% 
  mutate(cln_global_grade = str_remove(global_grade_picklist_label, "AGS ")) %>%
  rename(job_title = local_job_title_label,
         global_function = global_function_picklist_label) %>% 
  # remove duplicate codes (unclear why there are duplicates on this report, but aren't all temporary job codes)
  distinct(external_code, job_title, cln_global_grade, global_function)

df_jobs <- df_jobs %>% 
# add dummy variables for jobs based on talent segments we would expect to see in a premium band 
  # only actuarial is priced differently; other differences should come out through pay band
  # differences for sales, AIM accounted for in pay band
  mutate(is_actuarial = if_else(global_function == "Actuarial", 1, 0))

####################### Join: Job Code #######################

df_ledger_master <- df_ledger %>% 
  left_join(df_jobs, by = c("job_code" = "external_code")) %>% 
  filter(!is.na(cln_global_grade)) %>% 
  #filter(cln_global_grade <= 13) %>% # remove officers
  filter(cln_global_grade != "Not Applicable")
