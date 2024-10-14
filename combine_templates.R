library(openxlsx2)
library(tidyverse)
library(Microsoft365R)
library(readxl)

source("config_sharepoint_location.R")
source("config_quarter_info.R")

#create location to store the files copied from sharepoint
dir.create(str_glue("data/completed_files/{current_quarter_year}/"))

#loop through locations
for (i in psc){
  # download file from sharepoint
  chosenlib$download_file(src  = str_glue("{base_url}/{i}/{current_quarter_year}/QART.xlsx"), 
                        dest=str_glue("data/completed_files/{current_quarter_year}/{i}.xlsx"))
  
  # TODO - read in the file and aggregate data from all locations.
}

