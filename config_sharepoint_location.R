library(Microsoft365R)
site_url <- "https://nhs.sharepoint.com/sites/MED/ps2/it/mit"
site <- get_sharepoint_site(site_url = site_url, tenant="nhs")
chosenlib <- site$get_drive("Restricted Library")
base_url <- "Measurement/QART"


location_lookup <- read.csv("psc_lookup.csv")
psc<- unique(location_lookup$latest_psc_name)
