# script to check legacy status of trusts in PSC's 

# here we evaluate the organisation names for quarters after retroactive changes have been applied 
# i.e., to account for changes in orgs applicable from 2024/25 Q4 and after


# download previously submitted matneo data from SharePoint
master_files_dr <- chosenlib$get_item(glue::glue("{base_url}/{master_files_folder}"))
previous_data_file <- master_files_dr$get_item(str_glue({previous_mat_neo_submissions_file_name}))
previous_data_file$download(dest = here("output", str_glue({previous_mat_neo_submissions_file_name})), 
                       overwrite = T)

# upload data 
org_names_previous_submissions <- read.csv(
  here(str_glue('output/{previous_mat_neo_submissions_file_name}'))
  ) |>
  # some of the encoding went off so we fix here 
  mutate_if(
    is.character,
    function(row) iconv(row, to = "UTF-8", sub = "")
  ) |>
  mutate_if(is.character, ~ gsub("[^ -~]", " ", .)) |>
  # we know of submissions under trust sites instead of trusts names
  filter(quarter == previous_quarter_string) |>
  distinct(updated_psc_name, api_icb_code, api_icb_name, api_org_code, organisation_name_verified) |>
  rename('previous_quarter_icb_name' = api_icb_name,
         'previous_quarter_org_name' = organisation_name_verified,
         'previous_quarter_org_code' = api_org_code,
         'previous_quarter_icb_code' = api_icb_code
          )

# API call flow:
# 1. use trust codes to determine legacy status and extract succession history
# 2. use org codes of active trusts to extract ICB codes
# 3. extract all names using ICB codes


# API search 1
# Function below uses trust codes to determine legacy status 
# and succession history (if applicable) by checking the date types stored against an organisation
# we extract the date the organisation became inactive.
# If deemed a legacy organisation, we also extract end date and organisation code of successor
# the extraction logic below is backed up by API documentation:
# https://www.odsdatasearchandexport.nhs.uk/referenceDataCatalogue/ODS-Date-Concepts_620601026.html#:~:text=%2Dmm%2Ddd.-,Type%20%2D%20Legal%20vs%20Operational,need%20of%20systems%20and%20services.
# https://www.odsdatasearchandexport.nhs.uk/referenceDataCatalogue/Succession_587381243.html

call_by_org_code <- function(api_org_code) {
  trust_info <- content(GET(paste0(
    "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/",
    api_org_code
    )))
  api_org_name <- trust_info$Organisation$Name
  api_org_date <- trust_info$Organisation$Date
  
  print(glue::glue("** Getting mapping for {api_org_code} - {api_org_name} **"))
  
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
  
  # this is useful for quarters where theyere have not been any mergers 
  # so code doesn't break when in call_org_end_dates
  if (is.null(api_date_end)){
    api_date_end <- NA
  } 
  
  tibble(
    api_org_code,
    api_org_name,
    date_elements,
    api_date_type,
    api_date_start,
    api_date_end,
    api_succ_code
  )
}

call_org_end_dates <- apply(org_names_previous_submissions |> select(previous_quarter_org_code), 1, call_by_org_code) |>
  bind_rows() |>
  relocate(api_date_end, .after = api_date_start) |>
  mutate(api_date_start = ymd(api_date_start),
         api_date_end = ymd(api_date_end))

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

# determine after which quarter the organisation becomes legacy
call_org_end_dates_quarter_info <- call_org_end_dates |>
  mutate(
    # calculate inactivity time point
    inactive_from_date = ymd(api_date_end) + days(1),
    
    # if a change took place after the end of the reporting quarter, 
    # we ignore it because that change doesn't affect the data yet
    api_succ_code = case_when(inactive_from_date > reporting_quarter_end_date ~ NA,
                              .default = api_succ_code),
    # make sure type is character so left join in successor_organisation_details won't break
    api_succ_code = as.character(api_succ_code),
    api_date_end = case_when(inactive_from_date > reporting_quarter_end_date ~ NA,
                             .default = api_date_end),
    inactive_from_date = case_when(inactive_from_date > reporting_quarter_end_date ~ NA,
                                   .default = inactive_from_date),
    nhs_inactive_from_quarter = case_when(!is.na(inactive_from_date) ~ quarter(inactive_from_date, 
                                                                              fiscal_start = 4,
                                                                              type = 'year_start/end')
                                          )) |>
  relocate(inactive_from_date, nhs_inactive_from_quarter,
           .before = api_succ_code) 


# bring successor info for legacy orgs
# note this object will be empty when no org changes have occurred between 2 quarters
successor_organisation_details <- call_org_end_dates_quarter_info |>
  filter(!is.na(nhs_inactive_from_quarter)) |>
  select(api_date_end, api_succ_code) |>
  left_join(call_org_end_dates_quarter_info |> select(api_org_code, api_org_name),
            by = c("api_succ_code" = "api_org_code")
  ) |>
  rename("api_succ_name" = api_org_name)

# determine organisation status (legacy/active) up to reporting_quarter_end_date
call_orgs_legacy_status_quarter <- call_org_end_dates_quarter_info |>
  left_join(successor_organisation_details, by = c("api_date_end", "api_succ_code")) |>
  mutate(
    api_current_code = case_when(is.na(nhs_inactive_from_quarter) ~ api_org_code,
                                 .default = api_succ_code
    ),
    api_current_org_name = case_when(is.na(nhs_inactive_from_quarter) ~ api_org_name,
                                     .default = api_succ_name
    )
  )

# API search 2 - ICB mapping
# Function below uses trust codes for organisations deemed as active for present quarter
# A call by trust code is generated and relationships of the Trust
# with other organisations are evaluated
# we extract the ICB (code) under which the Trusts falls into (RE5 = IS LOCATED IN THE GROGRAPHY OF)
# Info on relationships:
# https://www.odsdatasearchandexport.nhs.uk/referenceDataCatalogue/Relationships_571324965.html

current_trust_codes <- call_orgs_legacy_status_quarter |>
  distinct(api_current_code)

call_icb_code <- function(api_current_code) {
  trust_info <- content(GET(paste0(
    "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/",
    api_current_code
  )))
  
  api_current_org_name <- trust_info$Organisation$Name
  
  print(glue::glue("** Getting ICB code for {api_current_org_name} **"))
  
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
    api_current_code,
    api_current_org_name,
    api_icb_code
  )
}

call_icb_org_codes <- apply(current_trust_codes, 1, call_icb_code) |>
  bind_rows()

# API search 3 - all ICB names
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
  api_current_icb_code <- all_icbs$Organisations[[icb]]$OrgId
  api_current_icb_name <- all_icbs$Organisations[[icb]]$Name
  
  call_icb_names[icb, "api_current_icb_code"] <- api_current_icb_code
  call_icb_names[icb, "api_current_icb_name"] <- api_current_icb_name
}

# add ICB codes to get ICB names form previous call so trusts have all ICB details needed
map_active_trusts_icb_details <- call_icb_names |>
  left_join(call_icb_org_codes, by = c("api_current_icb_code" = "api_icb_code"))

# output 1: how organisations names will appear in templates
# this is a map of active orgs for value stored in reporting_quarter_string 
map_psc_trust_icb_active_orgs <- map_active_trusts_icb_details |>
  left_join(org_names_previous_submissions,
            by = c('api_current_code' = 'previous_quarter_org_code')
            ) |>
  select(updated_psc_name, 
         api_current_icb_code, previous_quarter_icb_name, api_current_icb_name,
         api_current_code, previous_quarter_org_name, api_current_org_name
  ) |> 
  # check for name changes 
  mutate(icb_name_change = previous_quarter_icb_name != api_current_icb_name,
         trust_name_change = previous_quarter_org_name != api_current_org_name,
         ) |>
  relocate(icb_name_change, .after = api_current_icb_name)

# QA 
qa_name_changes <- map_psc_trust_icb_active_orgs |>
  filter(icb_name_change == TRUE | trust_name_change == TRUE)

empty_qa_name_changes <- nrow(qa_name_changes) == 0

if (empty_qa_name_changes == F) {
  warning("There have been either ICB or Trust name changes")
  t(qa_name_changes |> 
      select(previous_quarter_icb_name, api_current_icb_name,
             previous_quarter_org_name , api_current_org_name)
    )
} else {
  message("There have been no organisation changes between now an the previous quarter")
}

# final output
map_psc_trust_icb_active_orgs_final <- map_psc_trust_icb_active_orgs |>
  select(updated_psc_name, 
         api_current_icb_code, api_current_icb_name,
         api_current_code, api_current_org_name) 

write.csv(map_psc_trust_icb_active_orgs_final, here("lookups", "psc_icb_trust_lookup.csv"), row.names = F)

# save file on SharePoint 
chosenlib$upload_file(
  dest = str_glue("{base_url}/1. Master files/psc_icb_trust_lookup.csv"),
  src = "lookups/psc_icb_trust_lookup.csv"
)

# print a message of where organisation changes have occurred 
map_legacy <- call_orgs_legacy_status_quarter |> 
  filter(!is.na(nhs_inactive_from_quarter)) |>
  left_join(org_names_previous_submissions |> 
              select(updated_psc_name, previous_quarter_org_code,
                     previous_quarter_org_name), 
            by = c('api_org_code' = 'previous_quarter_org_code')) |>
  select(updated_psc_name, api_org_code, api_org_name, 
         api_date_end, api_succ_code, api_succ_name) 

if (nrow(map_legacy) > 0){
  message(str_glue('The following organisation changes are applicable up to {reporting_quarter_string}'))
  print(t(map_legacy))
} else {
  message("There have been no organisation changes between now an the previous quarter")
}

