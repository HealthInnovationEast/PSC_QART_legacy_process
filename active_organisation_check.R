library(here)
library(readxl)
library(tidyverse)
library(janitor)
library(glue)
library(httr)
library(jsonlite)
library(lubridate)

# find latest submission data
submissions_data <- read_excel(here("data", "MatNeoSIP.xlsx"), sheet = "Data") |>
  clean_names() |>
  rename(
    "stage_7_all" = "stage_7",
    "quarter" = "date"
  ) |>
  rename_with(~ str_replace(., "x", "stage_"), starts_with("x"))

# extract organisation list
organisations <- submissions_data |>
  select(1:4) |>
  # we already know of submissions under trust sites instead of trusts names
  filter(!organisation %in% c(
    "Eastbourne & Conquest Hospital",
    "Royal Sussex County (RSCH) (UHSX)",
    "Princess Royal (PRH) (UHSX)",
    "Worthing (UHSX)"
  ))

# API replacement
# some of the manual replacements below are temporary, need to find files or API that has old names
trusts <- organisations |>
  distinct(psc, organisation) |>
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
  )) |>
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

# query by looking at name
call_by_name <- function(url_end) {
  search_trust <- url_end |>
    str_replace_all("%20", " ") |>
    str_replace_all("%27", "'")

  print(glue::glue("**Now looking for {search_trust}**"))
  # trust <- content(GET(paste0('https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/?Name=', url_end)))
  trust <- content(GET(paste0("https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/?PrimaryRoleId=RO197&Name=", url_end)))

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
    for (hit in 1:hits) {
      name_retrieved <- trust$Organisations[[hit]]$Name
      status <- trust$Organisations[[hit]]$Status

      # the name retrieved has got to match the beginning of the string
      # and it has to correspond to an organisation that's operationally active
      if (str_detect(name_retrieved, paste0("^", search_trust)) & status == "Active") {
        hit_id <- hit

        api_org_role <- trust$Organisations[[hit_id]]$PrimaryRoleDescription
        api_org_code <- trust$Organisations[[hit_id]]$OrgId
        api_org_name <- trust$Organisations[[hit_id]]$Name
        api_org_link <- trust$Organisations[[hit_id]]$OrgLink
        api_hit <- hit_id

        print(glue::glue("Final result was retrieved from hit {hit_id}"))
      }
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

call_org_links <- apply(distinct_trusts[, 2], 1, call_by_name) |>
  bind_rows()

# QA check results for calls where there was >1 hit
qa_multiple_hits <- call_org_links |>
  filter(api_n_hits > 1)

# QA check compare org name used historically in templates vs. official name (from api)
# these will be the trusts to change in the templates
qa_name_discrepancies <- trusts |>
  left_join(call_org_links, by = "url_end") |>
  filter(organisation != api_org_name) |>
  select(
    psc,
    url_end,
    api_n_hits,
    api_hit,
    api_org_code,
    api_org_name,
    organisation,
    organisation_tidy,
    organisation_shorter
  ) |>
  arrange(psc)

# query end dates of defunct orgs in another API call

org_calls <- call_org_links |>
  distinct(api_org_link)

call_by_org_link <- function(api_org_link) {
  trust_info <- content(GET(api_org_link))

  api_org_code <- trust_info$Organisation$OrgId$extension
  api_org_name <- trust_info$Organisation$Name
  api_org_date <- trust_info$Organisation$Date

  print(glue::glue("** Getting mapping for {api_org_name} **"))

  date_elements <- as.numeric(length(api_org_date))

  print(glue::glue("Found {date_elements} date type(s)"))

  # if there's only one date type, it is operational
  # and operational date start would be the same as legal date start and no end date recorded
  # https://www.odsdatasearchandexport.nhs.uk/?search=generalorg&query=RJR
  # compare above result from ODS ODS portal vs API call below
  # https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/RJR

  if (date_elements == 1) {
    date_type <- api_org_date[1][[1]]$Type

    if (date_type == "Operational") {
      api_date_type <- date_type
      api_date_start <- api_org_date[1][[1]]$Start
      # this should be NULL
      api_date_end <- api_org_date[1][[1]]$End
      api_succ_code <- NA
      print(glue::glue("Retrieved {api_date_type} Date Info"))
    }
  } else if (date_elements == 2) {
    # some orgs might have both operational AND Legal date types
    # check the legal type element to determine whether it is relevant
    date_type <- api_org_date[2][[1]]$Type
    api_date_info <- names(api_org_date[[2]])

    if (date_type == "Legal" & "End" %in% api_date_info) {
      # if the organisation has a legal end date, it is a legacy organisation
      # for which we want to retrieve end date and successor information
      api_date_type <- date_type
      api_date_start <- api_org_date[2][[1]]$Start
      # this should be ALWAYS have a date
      api_date_end <- api_org_date[2][[1]]$End

      # retrieve the successor
      succ_info <- trust_info$Organisation$Succs$Succ
      succ_orgs <- as.numeric(length(succ_info))

      print("Successor/predecessor history:")

      for (succ_org in 1:succ_orgs) {
        succ_type <- trust_info$Organisation$Succs$Succ[[succ_org]]$Type
        print(succ_type)

        if (succ_type == "Successor") {
          succ_org_n <- succ_org
          # api_succ_date
          api_succ_code <- trust_info$Organisation$Succs$Succ[[succ_org_n]]$Target$OrgId$extension
        }
      }
    } else {
      # some orgs will have both date types but no end dates
      # meaning the organisation is current but had a different start date operationally and legally
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

# do we already have info for legacy orgs?
legacy_details <- call_org_end_dates |>
  filter(!is.na(api_date_end)) |>
  select(api_date_end, api_succ_code) |>
  left_join(call_org_end_dates |> select(api_org_code, api_org_name),
    by = c("api_succ_code" = "api_org_code")
  ) |>
  rename("api_succ_name" = api_org_name)

trusts_api_info <- call_org_end_dates |>
  left_join(legacy_details, by = c("api_date_end", "api_succ_code")) |>
  mutate(
    api_current_code = case_when(is.na(api_succ_code) ~ paste0(api_org_code),
      .default = paste0(api_succ_code)
    ),
    api_current_org_name = case_when(is.na(api_succ_name) ~ paste0(api_org_name),
      .default = paste0(api_succ_name)
    )
  )

# organisations to remove from Q3 24/25 templates
qart_quarters <- tibble(
  q_date = seq(
    from = as.Date("2020-04-01"),
    to = as.Date("2024-09-30"), # TO DO: make object to store value of end date of previous reporting quarter
    by = "quarter"
  )
) |>
  mutate(quarter = lubridate::quarter(q_date, type = "year.quarter", fiscal_start = 4)) |>
  # rename("quarter_end" = quarters) |>
  mutate(
    quarter_fy_start = round(quarter - 1),
    nhs_quarter = paste(quarter_fy_start, quarter, sep = "/"),
    nhs_quarter = str_replace(
      nhs_quarter,
      fixed("."),
      " Q"
    )
  )

# left_join results from both calls
current_trust_map <- trusts |>
  left_join(call_org_links, by = "url_end") |>
  left_join(current_trust_map, by = c(
    "api_org_link",
    "api_org_code",
    "api_org_name"
  ))

# TO DO: retrieve ICB mapping
# output has to be mapping of active orgs for a quarter

#potential logic for removals
qa_duplicate_and_legacy_trusts <- current_trust_map |>
  group_by(psc, api_current_code, api_current_org_name) |>
  filter(n() > 1) |>
  select(
    psc, organisation, organisation_tidy, api_org_code,
    api_date_end, api_succ_code,
    api_current_code, api_current_org_name
  ) |>
  filter(api_date_end <= as.Date("2024-09-30") | is.na(api_date_end)) |>
  mutate(valid_until_quarter = lubridate::quarter(api_date_end,
                                                  type = "year.quarter",
                                                  fiscal_start = 4
  )) |>
  left_join(qart_quarters, by = c("valid_until_quarter" = "quarter")) |>
  mutate(removal = if_else(!is.na(api_date_end), 1, 0)) |>
  arrange(api_current_code, valid_until_quarter)
