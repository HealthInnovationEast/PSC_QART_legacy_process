library(openxlsx2)
library(tidyverse)
library(Microsoft365R)
library(readxl)

source("config_sharepoint_location.R")
source("config_quarter_info.R")

dir.create(str_glue("data/completed_files/{current_quarter_year}/"))

#loop through locations
for (i in psc){
  
  chosenlib$download_file(src  = str_glue("{base_url}/{i}/{current_quarter_year}/QART.xlsx"), 
                        dest=str_glue("data/completed_files/{current_quarter_year}/{i}.xlsx"))
  
}

