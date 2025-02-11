# Config file for quarter information and lookup tables

# This is probably better placed in a params script
current_quarter_year <- "2425 Q3" #This must be changed each quarter. format is Q1.2324

# These two below have become obsolete after changes in create_empty_template
# current_quarter_number <- as.numeric(str_sub(current_quarter_year, 2,2)) # pull out the second character
# last_quarter_number<- if_else(current_quarter_number- 1 == 0, 4, current_quarter_number-1) #get the previous quarter number
