############################################################

# Title: Setup - Pay Equity
# Author: Emma Broadnax
# Last Updated: 09/17/2024
# Notes: Set up environment
# Warnings: Data locations are hard-coded

############################################################

####################### Load Libraries #######################

library(openxlsx)
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(car)
library(scales)
library(kableExtra)
library(janitor)

####################### Set Constants #######################

na_list = c("NA", "na")

# currently only running for full AZL umbrella based on group guidance
# any company with under 250 employees does not need this analysis done
# explored breaking out this analysis and submitting for AZRA, but determined not needed
company.list = c("AZOA Services Corporation", 
                 "Allianz Life Insurance Company",
                 "Allianz Reinsurance America Inc")

# set file paths
#p.executive.review <- "C:\\Users\\azl698z\\Documents\\Code\\pay-equity\\All-Company Executive Review 02.20.2024-upd.xls"
p.job.code.list <- "C:\\Users\\azl698z\\Documents\\job_code_list\\JobCodeReport-04.17.2025.xlsx"
p.ledger <- "C:\\Users\\azl698z\\Documents\\people_ledger\\PeopleLedger Confidential Month End 2025-04-01.xlsx"
#p.mapping <- "Mapping.xlsx"

# global grade groupings

# azl officers
thir_plus <- c("AGS 13", "AGS 14", "AGS 15", "AGS 16", "AGS 17", "AGS 18", "AGS 19", "AGS 20")

age_groups <- c(0, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 100)

####################### Read Files #######################

in_jobs <- read.xlsx(p.job.code.list,
                     detectDates = TRUE,
                     startRow = 3) %>% 
  clean_names()

# people ledger (for AZRA)
in_ledger <- read.xlsx(p.ledger, 
                       na.strings = na_list, 
                       detectDates = TRUE) %>% 
  clean_names() 

# elg member mappings
# in_mapping <- read.xlsx(p.mapping) %>% 
#   clean_names()


####################### Define Functions #######################


# Get GenderM coefficient function
# Parameters
# ----------
# lm_obj : A model object of the pay equity regression. This must contain Gender as a variable with F as the reference.
# 
# Returns
# -------
# Numeric value of the GenderM coefficient in the model object.

get_pct <- function(lm_obj){
  as.numeric((exp(coef(lm_obj)[["GenderM"]]) - 1))
}

# Get GenderM p-value function
# Parameters
# ----------
# lm_obj : A model object of the pay equity regression. This must contain Gender as a variable with F as the reference.
# 
# Returns
# -------
# Numeric value of the GenderM p-value in the model object.

get_pvalue <- function(lm_obj){
  as.numeric(summary(lm_obj)$coefficients[,4]["GenderM"])
}

