library(here)
library(Microsoft365R)
site_url <- Sys.getenv("sharepoint_url")
site <- get_sharepoint_site(site_url = site_url, tenant="nhs")
chosenlib <- site$get_drive("Restricted Library")
base_url <- "Measurement/QART"
dr <- chosenlib$get_item(str_glue({base_url}))