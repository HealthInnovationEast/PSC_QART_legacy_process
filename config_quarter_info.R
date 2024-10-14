current_quarter_year <- "Q2.2324"
current_quarter_number <- 2
last_quarter_number<- if_else(current_quarter_number- 1 == 0, 4, current_quarter_number-1)

location_lookup <- read.csv("psc_lookup.csv")
psc<- unique(location_lookup$PSC)
locations<- read.csv("lookup.csv")
