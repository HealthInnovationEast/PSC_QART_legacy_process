library(openxlsx2)
library(tidyverse)
#this just contains nottingham info but could make bigger or use better source
locations<- read.csv("lookup.csv")
current_quarter <- 1
last_quarter<- if_else(current_quarter- 1 == 0, 4, current_quarter-1)
#loop through locations
for (i in unique(locations$ICB)){

  #dataset with ics and trust
location_icb<-locations %>% filter(ICB==i) %>%
  select(ICS, TRUST)
#dataset with trusts
location_trust <- location_icb %>% select(TRUST)
#dataset just ics
location_ics <- location_icb %>% select(ICS) %>% distinct()


location_icb_2_quarters<- bind_rows(location_icb %>% mutate(Quarter=current_quarter),
                                    location_icb %>% mutate(Quarter = last_quarter))
#load template excel file
wb <- openxlsx2::wb_load("preferred_template.xlsx")


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

wb_save(wb,file=str_glue("empty_templates/{i}.xlsx"), overwrite = T)



}

