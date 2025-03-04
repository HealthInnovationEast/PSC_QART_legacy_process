library(here)
source('config_sharepoint_location.R')

# parameters 
reporting_quarter <- "2024/25 Q3" #read_quart_submisisons.R
current_quarter_year <- "2425 Q3" #create_empty_template 
#This must be changed each quarter. 
# Note that quarter format has changes and we probably need to think about harmonising
# across files
master_files_folder <- "1. Master files"
template_file_name <- "preferred_template.xlsx"

# process control

set_up_folders <- F # this only needs running once 
org_name_checks <- T
retroactive_fixes <- T
prepare_templates <- T
process_submissions <- T

# process execution 
if (set_up_folders){
  source('setup_site_folders.R')
}         

if (org_name_checks & retroactive_fixes) {
  # below checks matneo submissions up from 2021/22 Q1 to 2014/25 Q2
  # sources 'psc_name_update.R' (which applies HIN name updates)
  # and produces psc_lookup.csv and discrepancies_lookup.csv
  source('active_organisation_check.R') 
  # below uses discrepancies_lookup.csv 
  # and uploads cleansed data to SharePoint
  source('data_cleanse.R')
} #(name_checks & !retroactive_fixes) 
 #{source('active_organisation_check.R') #but the version that looks at cleansed data
 #}

if (prepare_templates) {
  # below uses psc_lookup.csv
  source('create_empty_template.R')
}

if (process_submissions) {
  source('read_qart_submissions.R')
  #source('append_data.R)
  #source('qart_slides.qmd')
}