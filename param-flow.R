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

sharepoint_local_access <- TRUE

source('config-sharepoint-location.R')

# ----------------- these variables will **change** every quarter

# this is the stage of the task we want executed
# either "prepare_templates" or "process_submissions"
stage_of_process <- "prepare_templates" 

# set dates by setting end of reporting quarter 
# (mind that latest data will be lagged by 1 quarter)
reporting_quarter_end_date <- ymd('2026-09-30')


# ----------------- code below stays **fixed**
reporting_quarter_string <- quarter(reporting_quarter_end_date, 
                                    fiscal_start = 4,
                                    type = 'year_start/end')
reporting_quarter_file_string <- reporting_quarter_string |>
  str_replace_all("/| ", "_")

previous_quarter_end_date <- reporting_quarter_end_date %m-% months(3)
previous_quarter_string <- quarter(previous_quarter_end_date, 
                                   fiscal_start = 4,
                                   type = 'year_start/end')
previous_quarter_file_string <- previous_quarter_string |>
  str_replace_all("/| ", "_")

# file below will be used to append new submissions data from PSCs
previous_mat_neo_submissions_file_name <- paste0("mat_neo_qart_all_data_upto_",
                                                 previous_quarter_file_string, ".csv") 

# adults and paeds cumulative data, since 2526 Q1
previous_marthas_submissions_adults_paeds <- paste0("marthas_qart_all_data_upto_",
                                                    previous_quarter_file_string, ".csv")

# mat neo and ED data, from 2627 Q1
previous_marthas_submissions_matneo <- paste0("marthas_qart_matneo_data_upto_",
                                              previous_quarter_file_string, ".csv")

previous_marthas_submissions_ed <- paste0("marthas_qart_ed_data_upto_",
                                          previous_quarter_file_string, ".csv")

# validation rule for dates
quarter_check <- str_replace_all(previous_quarter_string, '/| ', '_')

if (str_detect(previous_mat_neo_submissions_file_name, quarter_check) == FALSE) {
  stop('Previous_quarter_string was not set correctly. Check value for reporting_quarter_end_date')
} else {
  message('Dates set correctly')
}

master_files_folder <- "1. Master files" # where data files are in SharePoint
slides_folder <- "2. Output" # where html will be saved in SharePoint
template_file_name <- "qart_template_2627.xlsx"
site_lookup_file_name <- 'MR_All_Phases_Master.xlsx'
ICBs_by_HIN_file_name <- "ICBs_by_HIN.csv"

if (stage_of_process == "prepare_templates") {
  # will produce a new version of psc_icb_trust_lookup.csv to account for org changes (if there have been any)
  source('process/active-organisation-check.R') 
  # below accommodates adding martha's rule trust sites from 2526
  # eventually this could be incorporated in active_organisation_check.R
  source('process/trust-site-codes.R')
  # IMPORTANT: make sure you have updated the reporting quarter in the intro tab
  source('process/create-empty-templates.R')
} 

if (stage_of_process == "process_submissions") {
  # goes into sharepoint and collates submitted data for every PSC
  source('process/read-qart-submissions.R')
  # appends collated data to previous submissions
  source('process/append-data.R')
  # uses appended data to produce power point slide
  source('run-quarto.R') # runs qart_slides.qmd AND progression_slides.qmd
}
