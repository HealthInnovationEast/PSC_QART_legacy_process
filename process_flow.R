library(here)
source('config_sharepoint_location.R')

# parameters 
reporting_quarter <- "2024/25 Q3" # will change very quarter
master_files_folder <- "1. Master files"
template_file_name <- "preferred_template.xlsx"
previous_submissions_file_name <- 'mat_neo_qart_cleansed_upto_2425_Q2.csv' # will change very quarter

# process control

# needs running once only 
set_up_folders <- F 
retroactive_fixes <- T 
# will consistently need running
org_name_checks <- T
prepare_templates <- F #T
process_submissions <- T

# process execution 
if (set_up_folders){
  source('setup_site_folders.R')
}         

if (retroactive_fixes & org_name_checks) {
  # below checks matneo submissions up from 2021/22 Q1 to 2014/25 Q2
  # sources 'psc_name_update.R' (which applies HIN name updates)
  # and produces psc_lookup.csv and discrepancies_lookup.csv
  source('active_organisation_check.R')
  # below uses discrepancies_lookup.csv
  # and uploads cleansed data to SharePoint
  source('data_cleanse.R')
} else if(!retroactive_fixes & org_name_checks) {
  #source('active_organisation_check.R') #but the version that looks at cleansed data
  print('one script')
 }

if (prepare_templates) {
  # below uses psc_lookup.csv
  source('create_empty_template.R')
}

if (process_submissions) {
  source('read_qart_submissions.R')
  source('append_data.R')
  source('run_quarto.R') # which runs qart_slides.qmd
}
