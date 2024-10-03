# This file will be run once once settled on location

library(tidyverse)
library(Microsoft365R)

location_lookup <- read.csv("psc_lookup.csv")
psc<- unique(location_lookup$PSC)

site_url <- "https://nhsengland.sharepoint.com/sites/MED/ps2/psi"

site <- get_sharepoint_site(site_url = site_url, tenant="nhsengland")
openlib <- site$get_drive("Open Library")
base_url <- "03 Programme Management/04-PSCs/02-Assurance/13 - QART automation demo/"


for (psc_name in psc){
  openlib$upload_file(dest  = str_glue("{base_url}/{psc_name}/readme.txt"), src="readme.txt" )
}
