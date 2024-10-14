library(Microsoft365R)
site_url <- "https://nhsengland.sharepoint.com/sites/MED/ps2/psi"
site <- get_sharepoint_site(site_url = site_url, tenant="nhsengland")
chosenlib <- site$get_drive("Open Library")
base_url <- "03 Programme Management/04-PSCs/02-Assurance/13 - QART automation demo/"
