library(here)
library(readxl)
library(tidyverse)
library(janitor)
library(glue)
library(httr)
library(jsonlite)
library(lubridate)

# find latest submission data
files <- list.files(
  path = here::here("data"),
  full.names = T
)

file_info <- file.info(files)

most_recent <- row.names(file_info)[which.max(file_info$mtime)]

submissions_data <- read_excel(most_recent, sheet = "Data") |>
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
    organisation == "YORK TEACHING HOSPITAL NHS FOUNDATION TRUST" ~
      "YORK AND SCARBOROUGH TEACHING HOSPITALS NHS FOUNDATION TRUST", # name change effective from FY21/22
    .default = paste0(organisation)
  )) |>
  # URL links don't do white spaces nor apostrophes, so we encode them instead
  mutate(
    organisation_shorter = case_when(
      # do not remove nhsft from isle of wight, because there's an org called isle of wight healthcare
      str_detect(organisation_tidy, "^ISLE OF WIGHT") ~ paste0(organisation_tidy),
      # otherwise, remove all instances of nhs trust and nhs foundation trust
      # to make results more succinct
      .default = str_remove_all(organisation_tidy, "(?i)(nhs|nhs foundation) trust")
    ),
    organisation_shorter = str_trim(organisation_shorter), # can't figure out why str_remove_all introduces trailing white space
    url_end = str_replace_all(
      organisation_shorter,
      "'", "%27"
    ),
    url_end = str_replace_all(url_end, " ", "%20")
  )

distinct_trusts <- trusts |>
  distinct(organisation_tidy, url_end)

# query by looking at name
call_org_links <- apply(distinct_trusts[, 2], 1, function(url_end) {
  search_trust <- url_end |>
    str_replace_all("%20", " ") |>
    str_replace_all("%27", "'")

  glue::glue("Now looking for {search_trust}") # doesn't print :(
  # trust <- content(GET(paste0('https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/?Name=', url_end)))
  trust <- content(GET(paste0("https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/?PrimaryRoleId=RO197&Name=", url_end)))

  # there might be multiple results from query, but we're only after NHS Trusts
  hits <- as.numeric(length(trust$Organisations))
  glue::glue("Search has retrieved {hits} hit(s)") # won't print :(

  if (hits == 0) {
    api_org_role <- NA
    api_org_code <- NA
    api_org_name <- NA
    # api_org_status = NA
    api_org_link <- NA
  } else if (hits == 1) {
    # assume the 1 result is correct
    api_org_role <- trust$Organisations[[1]]$PrimaryRoleDescription
    api_org_code <- trust$Organisations[[1]]$OrgId
    api_org_name <- trust$Organisations[[1]]$Name
    api_org_link <- trust$Organisations[[1]]$OrgLink
  } else {
    # there are multiple hits produced in call and we want to determine which is relevant
    for (hit in 1:hits) {
      name_retrieved <- trust$Organisations[[hit]]$Name

      # the name retrieved has got to match the beginning of the string
      if (str_detect(name_retrieved, paste0("^", search_trust))) {
        hit_id <- hit

        api_org_role <- trust$Organisations[[hit_id]]$PrimaryRoleDescription
        api_org_code <- trust$Organisations[[hit_id]]$OrgId
        api_org_name <- trust$Organisations[[hit_id]]$Name
        api_org_link <- trust$Organisations[[hit_id]]$OrgLink
      }
    }
  }

  # put results together
  tibble(
    api_org_role,
    api_org_code,
    api_org_name,
    api_org_link,
    url_end
  )
}) |>
  bind_rows() |>
  as.data.frame()


# query end dates of defunct orgs in another API call

org_calls <- call_org_links |>
  distinct(api_org_link)

call_org_end_dates <- apply(org_calls, 1, function(api_org_link) {
  trust_info <- content(GET(api_org_link))

  api_org_code <- trust_info$Organisation$OrgId$extension
  api_org_date <- trust_info$Organisation$Date

  date_elements <- as.numeric(length(api_org_date))

  if (date_elements > 1) {
    for (element in 1:date_elements) {
      date_type <- api_org_date[element][[1]]$Type

      if (date_type == "Legal") {
        api_date_type <- date_type
        api_date_start <- api_org_date[element][[1]]$Start
        api_date_end <- api_org_date[element][[1]]$End
      }
    }
  } else {
    date_type <- api_org_date[1][[1]]$Type
    api_date_type <- date_type
    api_date_start <- api_org_date[1][[1]]$Start
    api_date_end <- api_org_date[1][[1]]$End
  }

  tibble(
    api_org_link,
    api_org_code,
    api_date_type,
    api_date_start,
    api_date_end
  )
}) |>
  bind_rows() |>
  as.data.frame()

# left_join results from both calls
trusts_api_info <- trusts |>
  left_join(call_org_links, by = "url_end") |>
  left_join(call_org_end_dates, by = c("api_org_link", "api_org_code"))

# organisations to remove from Q3 24/25 templates
date_series <- seq(
  from = as.Date("2021-04-01"),
  to = as.Date("2024-09-30"),
  by = "day"
)

quarters <- lubridate::quarter(date_series, type = "year.quarter", fiscal_start = 4) |>
  unique()

qart_quarters <- data.frame(quarters) |>
  rename("quarter_end" = quarters) |>
  mutate(
    quarter_start = round(quarters - 1),
    nhs_quarter = paste(quarter_start, quarter_end, sep = "/"),
    nhs_quarter = str_replace(
      nhs_quarter,
      fixed("."),
      " Q"
    )
  )

trusts_api_info |>
  filter(
    !is.na(api_date_end),
    api_date_end <= as.Date("2024-09-30")
  ) |>
  select(psc, organisation, api_org_name, api_date_end) |>
  mutate(valid_until_quarter = lubridate::quarter(api_date_end,
    type = "year.quarter",
    fiscal_start = 4
  )) |>
  left_join(qart_quarters, by = c("valid_until_quarter" = "quarter_end")) |>
  arrange(valid_until_quarter) #|>
# View()

# organisations that need updating
trusts_api_info |>
  filter(organisation != api_org_name) |>
  select(psc, organisation, api_org_name) |>
  View()

# TO DO: retrieve ICB mapping
# output has to be mapping of active orgs for a quarter
