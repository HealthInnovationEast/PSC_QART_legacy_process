# This file will be run once once settled on location

library(tidyverse)
library(Microsoft365R)
source("config_sharepoint_location.R")

#upload a readme file to a folder with the name of each psc. 
#To create a folder, a file must be uploaded to the folder- which creates the folder.

for (psc_name in psc){
  chosenlib$upload_file(dest  = str_glue("{base_url}/{psc_name}/readme.txt"), src="template_files/readme.txt" )
}
