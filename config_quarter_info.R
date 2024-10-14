# Config file for quarter information and lookup tables

current_quarter_year <- "Q2.2324" #This must be changed each quarter. format is Q1.2324
current_quarter_number <- as.numeric(str_sub(current_quarter_year, 2,2)) # pull out the second character
last_quarter_number<- if_else(current_quarter_number- 1 == 0, 4, current_quarter_number-1) #get the previous quarter number
