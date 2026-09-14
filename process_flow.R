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

# this is the stage of the task we want execute
# either "prepare_templates" or "process_submissions"
stage_of_process <- "prepare_templates" 

# set dates by setting end of reporting quarter (mind that latest data will be lagged by 1 quarter)
reporting_quarter_end_date <- ymd('2025-03-31') 
reporting_quarter_string <- quarter(reporting_quarter_end_date, 
                                    fiscal_start = 4,
                                    type = 'year_start/end')
previous_quarter_end_date <- reporting_quarter_end_date %m-% months(3)
previous_quarter_string <- quarter(previous_quarter_end_date, 
                                   fiscal_start = 4,
                                   type = 'year_start/end')

# file below will be used to append new submissions data from PSC
previous_submissions_file_name <- 'mat_neo_qart_cleansed_upto_2024_25_Q3.csv' 

# ----------------- code below stays **fixed** 

# validation rule for dates
quarter_check <- str_replace_all(previous_quarter_string, '/| ', '_')

if (str_detect(previous_submissions_file_name, quarter_check) == FALSE) {
  stop('Previous_quarter_string was not set correctly. Check value for reporting_quarter_end_date')
} else {
  message('Dates set correctly')
}

master_files_folder <- "1. Master files" # where data files are in SharePoint
slides_folder <- "2. Slides" # where presentation will be saved in SharePoint
template_file_name <- "preferred_template.xlsx"

if (stage_of_process == "prepare_templates") {
  # will produce a new version of psc_icb_trust_lookup.csv to account for org changes (if there have been any)
  source('active_organisation_check.R') 
  # IMPORTANT: make sure you have updated the reporting quarter in the intro tab
  source('create_empty_templates.R')
} 

if (stage_of_process == "process_submissions") {
  # goes into sharepoint and collates submitted data for every PSC
  source('read_qart_submissions.R')
  # appends colalted data to previous submissions
  source('append_data.R')
  # uses appended data to produce power point slide
  source('run_quarto.R') # runs qart_slides.qmd
}


