library(openxlsx2)
library(tidyverse)
library(Microsoft365R)

source("config_sharepoint_location.R")
source("config_quarter_info.R")

#loop through locations
for (i in psc){
  
  #dataset with ics and trust
  location_icb<-locations %>% 
    filter(ICB=="East Midlands") %>% #don't have lookup table- use this as demo
    select(ICS, TRUST)
  
  #dataset with trusts
  location_trust <- location_icb %>% select(TRUST)
  
  #dataset just ics
  location_ics <- location_icb %>% select(ICS) %>% distinct()
  
  
  location_icb_2_quarters<- bind_rows(location_icb %>% mutate(Quarter=current_quarter_number),
                                      location_icb %>% mutate(Quarter = last_quarter_number))
  #load template excel file
  wb <- openxlsx2::wb_load("template_files/preferred_template.xlsx")
  
  
  wb<- wb_add_data(wb, 
              sheet= "MatNeo optimisation", 
              x = location_icb_2_quarters, 
              start_col = 1, 
              start_row = 2,
              col_names = FALSE)
  
  wb<- wb_add_data(wb, 
                   sheet= "MatNeo early warning", 
                   x = location_icb_2_quarters,
                   start_col = 1, 
                   start_row = 2,
                   col_names = FALSE)
  
  wb<- wb_add_data(wb, 
                   sheet= "MatNeo lead engagement", 
                   x = location_trust,
                   start_col = 1, 
                   start_row = 2,
                   col_names = FALSE)
  
  wb<- wb_add_data(wb, 
                   sheet= "MatNeo PAS", 
                   x = location_ics,
                   start_col = 1, 
                   start_row = 2,
                   col_names = FALSE)
  
  wb_save(wb, 
          file = "data/empty_template.xlsx")
  
  chosenlib$upload_file(dest  = str_glue("{base_url}/{i}/{current_quarter_year}/QART.xlsx"), src="data/empty_template.xlsx" )
 
  file.remove( "data/empty_template.xlsx")

}

