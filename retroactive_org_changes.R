library(here)
library(Microsoft365R)
library(stringr)
library(tidyverse)
library(readxl)
library(janitor)
library(glue)
library(httr)
library(jsonlite)
library(lubridate)

source('config_sharepoint_location.R')

# script to retroactively correct errors in data up to 2024/15 Q2
master_files_dr <- chosenlib$get_item(glue::glue("{base_url}/1. Master files"))

# download hin names look up
hin_names_file <- master_files_dr$get_item('hin_names.csv')

hin_names_file$download(dest = here("lookups", "hin_names.csv"), 
                        overwrite = T)

# download raw data file from share point (data up to 2024/25 Q2)
raw_data_file <- master_files_dr$get_item('MatNeoSIP.xlsx')

raw_data_file$download(dest = here("data", "MatNeoSIP.xlsx"), 
                       overwrite = T)

# read latest psc (hin) names
hin_names <- read.csv(here("lookups", "hin_names.csv")) |>
  arrange(hin_folders)

# read previous submission data (i.e., up to 2024/25 Q2)
previous_submissions_data <- read_excel(here("data", "MatNeoSIP.xlsx"), sheet = "Data") |>
  clean_names() |>
  rename(
    "stage_7_all" = "stage_7",
    "quarter" = "date"
  ) |>
  rename_with(~ str_replace(., "x", "stage_"), starts_with("x"))

previous_submissions_old_psc_name <- previous_submissions_data |>
  mutate(psc = case_when(psc == "Health Innovation Network" ~ "South London HIN",
                         psc == "Health Innovation Manchester" ~ "Manchester HIN",
                         .default = paste0(psc)
  ))

# replace old psc names with their appropriate HIN name
psc_old_names <- previous_submissions_old_psc_name |> 
  distinct(psc) |>
  arrange(psc)

psc_name_look_up <- data.frame(hin_names, psc_old_names) |>
  rename('latest_psc_name' = hin_folders,
         'former_psc_name' = psc)

previous_submissions_psc_name_updated <- previous_submissions_old_psc_name |> 
  left_join(psc_name_look_up, by = c('psc' = 'former_psc_name')) |>
  relocate(latest_psc_name, 
           .after = psc) |> 
  select(-psc)

# extract organisation list
organisations <- previous_submissions_psc_name_updated |>
  select(1:4) |>
  # we know of submissions under trust sites instead of trusts names
  filter(!organisation %in% c(
    "Eastbourne & Conquest Hospital",
    "Royal Sussex County (RSCH) (UHSX)",
    "Princess Royal (PRH) (UHSX)",
    "Worthing (UHSX)"
  )) |>
  distinct(latest_psc_name, organisation) 

# Name fixes
# ideally, the hard coded replacements below would be replaced with a file or API call
# The name changes for the concerned organisations below reflect one of 3 events:
# 1. Incorrect use of a name that's slightly different from legal name
# (e.g., omitting 'teaching' in 'teaching hospitals)
# 2. An aesthetic name change (picture a re-branding)
# 3. A name change following a statutory legal changes (e.g, a merger). For example,
# org A acquired org B, then org A (as a merged org) changes name to Org C
# this means org B is succeeded by org C and org A had a re-brand (which is what
# is represented in the hard coded name change below)
# consult https://www.england.nhs.uk/publication/<old-organisation-name> for more details

org_names_previous_submissions <- organisations |>
  mutate(organisation_tidy = case_when(
    organisation == "UNITED LINCOLNSHIRE HOSPITALS NHS TRUST" ~
      "UNITED LINCOLNSHIRE TEACHING HOSPITALS NHS TRUST", # old templates systematically omitted 'teaching'
    organisation == "WEST HERTFORDSHIRE HOSPITALS NHS TRUST" ~
      "WEST HERTFORDSHIRE TEACHING HOSPITALS NHS TRUST", # ditto
    organisation == "MID YORKSHIRE HOSPITALS NHS TRUST" ~
      "MID YORKSHIRE TEACHING NHS TRUST", # name change effective from May 2023
    organisation == "KINGSTON HOSPITAL NHS FOUNDATION TRUST" ~
      "KINGSTON AND RICHMOND NHS FOUNDATION TRUST", # name change after acquisition in Nov 2024
    organisation == "ST HELENS AND KNOWSLEY TEACHING HOSPITALS NHS TRUST" ~
      "MERSEY AND WEST LANCASHIRE TEACHING HOSPITALS NHS TRUST", # name change after merger in July 2023
    organisation == "WESTERN SUSSEX HOSPITALS NHS FOUNDATION TRUST" ~
      "UNIVERSITY HOSPITALS SUSSEX NHS FOUNDATION TRUST", # name change after acquisition in April 2021
    organisation == "ROYAL DEVON AND EXETER NHS FOUNDATION TRUST" ~
      "ROYAL DEVON UNIVERSITY HEALTHCARE NHS FOUNDATION TRUST", # name change after acquisition in April 2022
    organisation == "HOMERTON UNIVERSITY HOSPITAL NHS FOUNDATION TRUST" ~
      "HOMERTON HEALTHCARE NHS FOUNDATION TRUST", # name change effective from April 2022
    organisation == "PENNINE ACUTE HOSPITALS NHS TRUST" ~
      "NORTHERN CARE ALLIANCE NHS FOUNDATION TRUST", # name change after dissolution in October 2021
    organisation == "YORK TEACHING HOSPITAL NHS FOUNDATION TRUST" ~
      "YORK AND SCARBOROUGH TEACHING HOSPITALS NHS FOUNDATION TRUST", # name change effective from FY21/22
    .default = paste0(organisation)
  )) 

# API replacement
trusts <- org_names_previous_submissions |>
  # URL links don't do white spaces nor apostrophes, so we encode them instead
  mutate(
    organisation_shorter = str_remove_all(organisation_tidy, "(?i) (nhs|nhs foundation) trust"),
    url_end = str_replace_all(
      organisation_shorter,
      "'", "%27"
    ),
    url_end = str_replace_all(url_end, " ", "%20")
  )

distinct_trusts <- trusts |>
  distinct(organisation_tidy, url_end)

# API call flow:
# 1. look for names, extract api link
# 2. use links to determine legacy status and extract succession history
# 3. use org codes of active trusts to extract ICB codes
# 4. extract all names using ICB codes

# API search 1
# Function below makes an API call using a name string
# and restricting results by organisations categorised as an NHS Trust
# if there are multiple hits that match the string and role criteria,
# the function adds and additional legal status criteria to determine the relevant hit
# once the relevant linked is inferred,
# we extract a hyperlink that can be used as a subsequent API call to consult more details

call_by_name <- function(url_end) {
  search_trust <- url_end |>
    str_replace_all("%20", " ") |>
    str_replace_all("%27", "'")
  
  print(glue::glue("** Now looking for {search_trust} **"))
  
  trust <- content(GET(paste0(
    "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/?PrimaryRoleId=RO197&Name=",
    url_end
  )))
  
  # there might be multiple results from query, but we're only after NHS Trusts
  hits <- as.numeric(length(trust$Organisations))
  print(glue::glue("Call has retrieved {hits} hit(s)"))
  
  api_n_hits <- hits
  
  if (hits == 0) {
    api_org_role <- NA
    api_org_code <- NA
    api_org_name <- NA
    # api_org_status = NA
    api_org_link <- NA
    api_hit <- NA
  } else if (hits == 1) {
    # assume the 1 result is correct
    api_org_role <- trust$Organisations[[1]]$PrimaryRoleDescription
    api_org_code <- trust$Organisations[[1]]$OrgId
    api_org_name <- trust$Organisations[[1]]$Name
    api_org_link <- trust$Organisations[[1]]$OrgLink
    api_hit <- hits
  } else {
    # there are multiple hits produced in call and we want to determine which is relevant
    # setup counter for how many hits were potentially correct
    n_status <- 0
    hit_id <- NA
    for (hit in 1:hits) {
      name_retrieved <- trust$Organisations[[hit]]$Name
      status <- trust$Organisations[[hit]]$Status
      # the name retrieved has got to match the beginning of the string
      # and it has to correspond to an organisation that's operationally active
      hit_active <- str_detect(name_retrieved, paste0("^", search_trust)) & status == "Active"
      if (hit_active) {
        hit_id <- hit
        n_status <- n_status + 1
      }
    }
    
    print(str_glue("There were {n_status} hits matching our criteria"))
    if (n_status == 1) {
      api_org_role <- trust$Organisations[[hit_id]]$PrimaryRoleDescription
      api_org_code <- trust$Organisations[[hit_id]]$OrgId
      api_org_name <- trust$Organisations[[hit_id]]$Name
      api_org_link <- trust$Organisations[[hit_id]]$OrgLink
      api_hit <- hit_id
      
      print(glue::glue("Final result was retrieved from hit {hit_id}"))
    } else {
      print(str_glue("Setting values to NA as either 0 or more than 1 hits met our criteria"))
      api_org_role <- NA
      api_org_code <- NA
      api_org_name <- NA
      # api_org_status = NA
      api_org_link <- NA
      api_hit <- NA
    }
  }
  
  # put results together
  tibble(
    url_end,
    api_n_hits,
    api_hit,
    api_org_role,
    api_org_code,
    api_org_name,
    api_org_link
  )
}

call_org_links <- apply(distinct_trusts[c('url_end')], 1, call_by_name) |>
  bind_rows()

# QA do we have hits with no names
qa_no_hits <- call_org_links |>
  filter(api_n_hits == 0)  

empty_qa_no_hits <- nrow(qa_no_hits) == 0

if (empty_qa_no_hits == F) {
  stop("Check qa_no_hits as there have been calls with no return by name")
}

# QA check results for calls where there was >1 hit
qa_multiple_hits <- call_org_links |>
  filter(api_n_hits > 1)

# query end dates of defunct orgs in another API call
org_calls <- call_org_links |>
  distinct(api_org_link)

# API search 2
# Function below uses the links extracted in the first API call to determine
# legacy status and succession history (if applicable) by checking the date types
# stored against an organisation
# we extract the date the organisation became active.
# If deemed a legacy organisation, we also extract end date and organisation code of successor
# the extraction logic below is backed up by API documentation:
# https://www.odsdatasearchandexport.nhs.uk/referenceDataCatalogue/ODS-Date-Concepts_620601026.html#:~:text=%2Dmm%2Ddd.-,Type%20%2D%20Legal%20vs%20Operational,need%20of%20systems%20and%20services.
# https://www.odsdatasearchandexport.nhs.uk/referenceDataCatalogue/Succession_587381243.html

call_by_org_link <- function(api_org_link) {
  trust_info <- content(GET(api_org_link))
  api_org_code <- trust_info$Organisation$OrgId$extension
  api_org_name <- trust_info$Organisation$Name
  api_org_date <- trust_info$Organisation$Date
  
  print(glue::glue("** Getting mapping for {api_org_name} **"))
  
  date_elements <- as.numeric(length(api_org_date))
  
  print(glue::glue("Found {date_elements} date type(s)"))
  
  # if there's only one date type, it is always operational
  # (legal dates are only provided when they differ from operational)
  # if there is no end date, the organisation is not legacy
  # (end dates are only provided when there's succession history and they're always of legal type)
  
  if (date_elements == 1) {
    api_date_type <- api_org_date[1][[1]]$Type
    api_date_start <- api_org_date[1][[1]]$Start
    # this should be NULL
    api_date_end <- api_org_date[1][[1]]$End
    api_succ_code <- NA
    print(glue::glue("Retrieved {api_date_type} Date Info"))
  } else if (date_elements == 2) {
    # some orgs might have both operational AND Legal date types
    # but this doesn't automatically equate to the org being legacy
    # check the legal type element to determine whether there is succession history
    date_type <- api_org_date[2][[1]]$Type
    api_date_info <- names(api_org_date[[2]])
    
    if (date_type == "Legal" & "End" %in% api_date_info) {
      # if the organisation has a legal end date, it is a legacy organisation
      # for which we want to retrieve end date and successor information
      api_date_type <- date_type
      api_date_start <- api_org_date[2][[1]]$Start
      # this should be ALWAYS have a date
      api_date_end <- api_org_date[2][[1]]$End
      
      # retrieve the successor history
      succ_info <- trust_info$Organisation$Succs$Succ
      succ_orgs <- as.numeric(length(succ_info))
      succ_org_types <- sapply(succ_info, `[[`, "Type")
      succ_n_orgs <- length(which(succ_org_types == "Successor"))
      
      if (succ_n_orgs > 1) {
        print(glue::glue("** Warning: {api_org_code} **"))
        print(glue::glue("There are {succ_n_orgs} succesor organisations recorded in this record"))
        print("Investigate succession history further")
        api_succ_code <- "Multiple"
      } else if (succ_n_orgs == 1) {
        print(glue::glue("Successor/predecessor history:"))
        
        for (succ_org in 1:succ_orgs) {
          succ_type <- trust_info$Organisation$Succs$Succ[[succ_org]]$Type
          print(succ_type)
          
          if (succ_type == "Successor") {
            succ_org_n <- succ_org
            api_succ_code <- trust_info$Organisation$Succs$Succ[[succ_org_n]]$Target$OrgId$extension
          }
        }
      }
    } else {
      # some orgs will have both date types but no end dates
      # meaning the organisation is current but had a different start dates operationally and legally
      # we fetch operational info as that was the approach used when there was only 1 date type assigned to org
      api_date_type <- api_org_date[1][[1]]$Type
      
      if (api_date_type == "Operational") {
        api_date_start <- api_org_date[1][[1]]$Start
        # This should be NULL because it won't exist
        api_date_end <- api_org_date[1][[1]]$End
        api_succ_code <- NA
      }
    }
    print(glue::glue("Retrieved {api_date_type} Date Info"))
  }
  
  
  tibble(
    api_org_link,
    api_org_code,
    api_org_name,
    date_elements,
    api_date_type,
    api_date_start,
    api_date_end,
    api_succ_code
  )
}

call_org_end_dates <- apply(org_calls, 1, call_by_org_link) |>
  bind_rows() |>
  relocate(api_date_end, .after = api_date_start)

qa_multiple_date_types <- call_org_end_dates |>
  filter(date_elements > 1)

# This table should be EMPTY
# If it isn't, the main successor needs to be worked out. The link below is super helpful
# https://www.england.nhs.uk/publication/<predecessor-name>
qa_multiple_succ <- qa_multiple_date_types |>
  filter(api_succ_code == "Multiple")

empty_qa_multiple_succ <- nrow(qa_multiple_succ) == 0

if (empty_qa_multiple_succ == F) {
  warning("Check qa_multiple_succ for multiple successors")
}

# list of quarters (formatted )
qart_quarters <- tibble(
  q_date = seq(
    # from a year before data collection (this will make it clearer to see UNIVERSITY HOSPITALS SUSSEX merger)
    from = as.Date("2020-04-01"),
    # up until end of 2024/25 Q2 (i.e., last quarter of previous submisisons)
    to = as.Date("2024-09-30"),
    by = "quarter"
  )
) |>
  mutate(quarter = lubridate::quarter(q_date, type = "year.quarter", fiscal_start = 4)) |>
  mutate(
    quarter_fy_start = round(quarter - 1),
    # extract last 4 characters from 'quarter' string (e.g., extracts 21.1 from 2021.1 )
    quarter_fy_end = str_extract((quarter), '(.{4})$'),
    nhs_quarter = paste(quarter_fy_start, quarter_fy_end, sep = "/"),
    nhs_quarter = str_replace(
      nhs_quarter,
      fixed("."),
      " Q"
    )
  )

# determine after which quarter the organisation becomes legacy
call_org_end_dates_quarter_info <- call_org_end_dates |>
  mutate(active_until_quarter = lubridate::quarter(api_date_end,
                                                   type = "year.quarter",
                                                   fiscal_start = 4
  )) |>
  left_join(qart_quarters |> select(quarter, nhs_quarter),
            by = c(active_until_quarter = "quarter")
  )

# bring successor info for legacy orgs
successor_organisation_details <- call_org_end_dates_quarter_info |>
  filter(!is.na(nhs_quarter)) |>
  select(api_date_end, api_succ_code) |>
  left_join(call_org_end_dates_quarter_info |> select(api_org_code, api_org_name),
            by = c("api_succ_code" = "api_org_code")
  ) |>
  rename("api_succ_name" = api_org_name)

# determine organisation status (legacy/active) up to 2024/25 Q2
call_orgs_active_status_quarter <- call_org_end_dates_quarter_info |>
  left_join(successor_organisation_details, by = c("api_date_end", "api_succ_code")) |>
  mutate(
    api_current_code_quarter = case_when(is.na(nhs_quarter) ~ api_org_code,
                                         .default = api_succ_code
    ),
    api_current_org_name_quarter = case_when(is.na(nhs_quarter) ~ api_org_name,
                                             .default = api_succ_name
    )
  )

# create a map that includes organisations as recorded in raw data plus information from API calls 
# below is a map of all organisation present in data
map_trusts_legacy_status <- trusts |>
  left_join(call_org_links, by = "url_end") |>
  left_join(call_orgs_active_status_quarter, by = c(
    "api_org_link",
    "api_org_code",
    "api_org_name"
  ))

# API search 3 - ICB mapping
# Function below uses trust codes for organisations deemed as active for present quarter
# A call by trust code is generated and relationships of the Trust
# with other organisations are evaluated
# we extract the ICB (code) under which the Trusts falls into (RE5 = IS LOCATED IN THE GROGRAPHY OF)
# Info on relationships:
# https://www.odsdatasearchandexport.nhs.uk/referenceDataCatalogue/Relationships_571324965.html

current_trust_codes <- map_trusts_legacy_status |>
  distinct(api_current_code_quarter)

call_icb_code <- function(api_current_code_quarter) {
  trust_info <- content(GET(paste0(
    "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/",
    api_current_code_quarter
  )))
  
  api_current_org_name_quarter <- trust_info$Organisation$Name
  
  print(glue::glue("** Getting ICB code for {api_current_org_name_quarter} **"))
  
  relationships <- length(trust_info$Organisation$Rels$Rel)
  
  # setup counter for how many hits were potentially correct
  n_status <- 0
  rel_n <- NA
  
  for (relationship in 1:relationships) {
    rel_id <- trust_info$Organisation$Rels$Rel[[relationship]]$id
    rel_status <- trust_info$Organisation$Rels$Rel[[relationship]]$Status
    if (rel_id == "RE5" & rel_status == "Active") { # RE5 is the ICB relationship
      n_status <- n_status + 1
      rel_n <- relationship # extract relationship number
      print(glue::glue("Index of relationship extracted: {rel_n}"))
    }
  }
  
  if (n_status == 1) {
    api_icb_code <- trust_info$Organisation$Rels$Rel[[rel_n]]$Target$OrgId$extension
  } else {
    print(str_glue("Setting ICB code to NA as either 0 or more than 1 hits met our criteria"))
    api_icb_code <- NA
  }
  
  
  tibble(
    api_current_code_quarter,
    api_current_org_name_quarter,
    api_icb_code
  )
}

call_icb_org_codes <- apply(current_trust_codes, 1, call_icb_code) |>
  bind_rows()

# API search 4 - all ICB names
# below we make a single call that extracts all organisations whose name contains ICB
# and whose role is R0261 (STRATEGIC PARTNERSHIP)
# roles list available here: https://www.odsdatasearchandexport.nhs.uk/referenceDataCatalogue/Roles_571324911.html

link <- paste0(
  "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations",
  "?PrimaryRoleId=RO261",
  "&Name=integrated%20care%20board",
  "&Limit=1000"
)

all_icbs <- content(GET(link))

call_icb_names <- data.frame()

for (icb in 1:length(all_icbs$Organisations)) {
  api_icb_code <- all_icbs$Organisations[[icb]]$OrgId
  api_icb_name <- all_icbs$Organisations[[icb]]$Name
  
  call_icb_names[icb, "api_icb_code"] <- api_icb_code
  call_icb_names[icb, "api_icb_name"] <- api_icb_name
}

# add ICB codes to get ICB names form previous call so trusts have all ICB details needed
map_trusts_legacy_status_icb_details <- map_trusts_legacy_status |>
  left_join(call_icb_org_codes, c(
    "api_current_code_quarter",
    "api_current_org_name_quarter"
  )) |>
  left_join(call_icb_names, c("api_icb_code"))

# output 1: how organisations names will appear in templates
# this is a map of active orgs from 2024/25 Q2 onwards 
map_psc_trust_icb_active_orgs <- map_trusts_legacy_status_icb_details |>
  distinct(
    latest_psc_name, api_icb_code, api_icb_name,
    api_current_code_quarter, api_current_org_name_quarter
  ) |> 
  arrange(latest_psc_name, api_icb_name, api_current_org_name_quarter)

write.csv(map_psc_trust_icb_active_orgs, here("lookups", "psc_icb_trust_lookup.csv"), row.names = F)

# save file on SharePoint 
chosenlib$upload_file(
  dest = str_glue("{base_url}/1. Master files/psc_icb_trust_lookup.csv"),
  src = "lookups/psc_icb_trust_lookup.csv"
)

# print a message of where organisation changes have occurred 
map_legacy <- map_trusts_legacy_status_icb_details |> 
  filter(!is.na(nhs_quarter)) |>
  select(latest_psc_name, api_org_code, api_org_name, api_date_end, api_succ_code, api_succ_name)

message('The following organisation changes are applicable to the list of trusts up to 2024/25 Q2')
print(t(map_legacy))

# output 2: to be used for retroactive data cleansing
map_discrepancies <- map_trusts_legacy_status_icb_details |>
  select(
    latest_psc_name, organisation, organisation_tidy, api_org_code, api_date_end,
    nhs_quarter, api_current_code_quarter, api_current_org_name_quarter
  ) |>
  filter(organisation != organisation_tidy |
           organisation != api_current_org_name_quarter) |>
  arrange(latest_psc_name, api_current_org_name_quarter) 

# save locally
write.csv(map_discrepancies, here("lookups", "discrepancies_lookup.csv"), row.names = F)

# proceed to data cleanse procedure
source('retroactive_data_cleanse.R')

