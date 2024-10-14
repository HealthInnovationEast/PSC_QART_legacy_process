# This file will be run once once settled on location

library(tidyverse)
library(Microsoft365R)
source("config_sharepoint_location.R")

location_lookup <- read.csv("psc_lookup.csv")
psc<- unique(location_lookup$PSC)

for (psc_name in psc){
  chosenlib$upload_file(dest  = str_glue("{base_url}/{psc_name}/readme.txt"), src="template_files/readme.txt" )
}
