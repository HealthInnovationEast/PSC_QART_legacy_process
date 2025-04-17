library(here)
library(Microsoft365R)
library(stringr)
library(tidyverse)
library(readxl)
library(janitor)
library(glue)
library(httr)
library(jsonlite)
library(lubridate)
library(openxlsx2)
library(quarto)

source('config_sharepoint_location.R')

# ----------------- these variables will **change** every quarter

# make these vars below from a table, so only one input which will be a date
reporting_quarter <- "2024/25 Q4" # will change very quarter
previous_quarter <- "2024/25 Q3"
#previous_quarter_end_date <- as.Date("2024-09-30") # ??? is this still used 

# below will be used after 2024/25 Q3 to append Q4 data 
previous_submissions_file_name <- 'mat_neo_qart_all_data_upto_2024_25_Q3.csv' # will change every quarter

# process control
#org_legacy_status_check <- T # always T, do we need to keep then?

# either "prepare_templates" or "process_submissions"
stage_of_process <- "prepare_templates" 
 
# ----------------- code below stays **fixed** 

master_files_folder <- "1. Master files" # where data files are in SharePoint
slides_folder <- "2. Slides" # where presentation will be saved in SharePoint
template_file_name <- "preferred_template.xlsx"

# if running the process in 2024/25 Q4, we can source directly create_empty_templates.R 
# because the look up of psc-icb-trusts applicable to this quarter was produced via retroactive_org_changes 
# (i.e., psc_icb_trust_lookup.csv)
if (stage_of_process == "prepare_templates" & reporting_quarter == '2024/25 Q4') {
  source('create_empty_templates.R')
}

# for all quarters after 2024/25 Q4, we want to account for new organisation changes 
if (stage_of_process == "prepare_templates" & reporting_quarter != '2024/25 Q4') {
  source('active_organisation_check.R') # will produce a new version of psc_icb_trust_lookup.csv to account for org changes (if there have been any)
  source('create_empty_templates.R')
} 

# this step will only apply when reporting_quarter == 2024/25 Q3 or after 
if (stage_of_process == "process_submissions") {
  source('read_qart_submissions.R')
  source('append_data.R')
  source('run_quarto.R') # which runs qart_slides.qmd
}


