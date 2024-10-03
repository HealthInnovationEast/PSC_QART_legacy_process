library(openxlsx2)
library(tidyverse)
library(Microsoft365R)
library(readxl)

site_url <- "https://nhsengland.sharepoint.com/sites/MED/ps2/psi"
site <- get_sharepoint_site(site_url = site_url, tenant="nhsengland")
openlib <- site$get_drive("Open Library")
base_url <- "03 Programme Management/04-PSCs/02-Assurance/13 - QART automation demo/"


current_quarter_year <- "Q1.2324"
current_quarter_number <- 1
last_quarter_number<- if_else(current_quarter_number- 1 == 0, 4, current_quarter_number-1)


location_lookup <- read.csv("psc_lookup.csv")
psc<- unique(location_lookup$PSC)

dir.create(str_glue("data/completed_files/{current_quarter_year}/"))

#loop through locations
for (i in psc){
  
  openlib$download_file(src  = str_glue("{base_url}/{i}/{current_quarter_year}/QART.xlsx"), 
                        dest=str_glue("data/completed_files/{current_quarter_year}/{i}.xlsx"))
  
}

