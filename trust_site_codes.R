message('Adding trust site codes...')

# bring psc-icb-trust lookup created via active_Organisation_check.R  
psc_icb_trust_lookup <- read.csv(here("lookups", "psc_icb_trust_lookup.csv")) 
pscs <- unique(psc_icb_trust_lookup$updated_psc_name)

# download look up provided by improvement team
site_lookup_file_name <- '20250710 Site list for QART template.xlsx'
site_lookup_file <- master_files_dr$get_item(str_glue({site_lookup_file_name}))
site_lookup_file$download(dest = here("lookups", str_glue({site_lookup_file_name})), 
                            overwrite = T)

# upload look up
sites_phase_1 <- read_excel(here("lookups", str_glue({site_lookup_file_name})),
                            sheet = 'PHASE 1 SITES') |>
  clean_names() |>
  mutate(phase = 1)

sites_phase_2 <- read_excel(here("lookups", str_glue({site_lookup_file_name})),
                          sheet = 'PHASE 2 SITES') |>
  clean_names() |>
  mutate(phase = 2)

# combine phase 1 and 2 
site_lookup <- bind_rows(sites_phase_1, sites_phase_2)

# wrangle to allow left join to psc_icb_trust_lookup
site_lookup_parsed <- site_lookup |>
  select(-c(psc), # not useful as using abbreviated names
         name_of_trust,
         name_of_site,
         trust_site_code = ods_code, 
         to_grey_out = starts_with('will_be')
         ) 

# use site codes in an API call
call_by_site_code <- function(trust_site_code) {
  site_info <- content(GET(paste0(
    "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/",
    trust_site_code
  )))
  
  api_site_name <- site_info$Organisation$Name
  
  print(glue::glue("** Getting parent code for {trust_site_code} - {api_site_name}  **"))
  
  relationships <- length(site_info$Organisation$Rels$Rel)
  
  # setup counter for how many hits were potentially correct
  n_status <- 0
  rel_n <- NA
  
  for (relationship in 1:relationships) {
    rel_id <- site_info$Organisation$Rels$Rel[[relationship]]$id
    rel_status <- site_info$Organisation$Rels$Rel[[relationship]]$Status
    if (rel_id == "RE6" & rel_status == "Active") { # RE5 is the parent trust relationship
      n_status <- n_status + 1
      rel_n <- relationship # extract relationship number
      print(glue::glue("Index of relationship extracted: {rel_n}"))
    }
  }
  
  if (n_status == 1) {
    api_parent_code <- site_info$Organisation$Rels$Rel[[rel_n]]$Target$OrgId$extension
  } else {
    print(str_glue("Setting parent code to NA as either 0 or more than 1 hits met our criteria"))
    api_parent_code <- NA
  }
  
  tibble(
    trust_site_code,
    api_site_name,
    api_parent_code
  )
}

call_parent_codes <- apply(site_lookup_parsed |> select(trust_site_code), 1, call_by_site_code) |>
  bind_rows() 

# are all the nhs trusts for these sites included in qart already ? 
no_psc_returns <- call_parent_codes |>
  left_join(psc_icb_trust_lookup, by = c('api_parent_code' = 'api_current_code')) |>
  filter(is.na(updated_psc_name)) 

# get trust names, start, and end dates using function developed in active_orgsanisation_check.R 
call_org_names_and_dates <- apply(no_psc_returns |> distinct(api_parent_code), 1, call_by_org_code) |>
  bind_rows() |>
  relocate(api_date_end, .after = api_date_start) |>
  mutate(api_date_start = ymd(api_date_start),
         api_date_end = ymd(api_date_end))

new_trusts_succession_history <- call_org_names_and_dates |>
  # put trust names back
  left_join(no_psc_returns, by = c('api_org_code' = 'api_parent_code')) |>
  # remove unnecessary vars
  select(-c(updated_psc_name, api_icb_code, api_icb_name, api_current_org_name))|>
  # work out trust code and name after org changes
  mutate(api_org_current_code = if_else(!is.na(api_succ_code), api_succ_code, api_org_code),
         api_org_current_name = if_else(is.na(api_succ_code), api_org_name, NA)) |>
  # get names for post merger trusts
  left_join(psc_icb_trust_lookup |> select(api_current_code, api_current_org_name), 
            by = c('api_org_current_code' = 'api_current_code')) |>
  mutate(api_org_current_name = if_else(is.na(api_succ_code), 
                                        api_org_current_name, api_current_org_name)) 


new_trusts_psc_info <- new_trusts_succession_history |>
  select(api_parent_code = api_org_current_code, 
         api_current_org_name = api_org_current_name, 
         trust_site_code, api_site_name) |>
  left_join(site_lookup |> select(psc, ods_code), by = c('trust_site_code' = 'ods_code')) |>
  mutate(updated_psc_name = case_when(psc == 'Eastern' ~ 'Eastern HIN',
                         psc == 'HIN Manchester' ~ 'Manchester HIN',
                         psc == 'HIN South London' ~ 'South London HIN',
                         psc == 'KSS' ~ 'Kent Surrey Sussex HIN',
                         psc == 'North West Coast' ~ 'North West Coast HIN',
                         psc == 'UCLP' ~ 'UCLPartners HIN',
                         psc == 'West Midlands' ~ 'West Midlands HIN',
                         psc == 'West Mids' ~ 'West Midlands HIN',
                         psc == 'Y&H' ~ 'Yorkshire & Humber HIN'
                         ),
         .before = api_parent_code
         ) |>
  select(-psc)

# add info
psc_trust_site_lookup <- call_parent_codes  |>
  left_join(psc_icb_trust_lookup, by = c('api_parent_code' = 'api_current_code')) |>
  # removing icb info as this look up it won't be shown for Martha's rule
  select(-contains('icb')) |>
  filter(!is.na(updated_psc_name)) |>
  bind_rows(new_trusts_psc_info) |>
  # bring phase info
  left_join(site_lookup_parsed |> select(trust_site_code, phase, to_grey_out), by = 'trust_site_code') |>
  # col order
  select(updated_psc_name, api_parent_code, api_current_org_name, 
         trust_site_code, api_site_name,
         phase, to_grey_out) |>
  # row order
  arrange(updated_psc_name, api_current_org_name, api_site_name)

psc_trust_site_lookup<- psc_trust_site_lookup %>%
  mutate(updated_psc_name = if_else(trust_site_code == "RD816", "Eastern", updated_psc_name))

# save work locally and on sharepoint
write.csv(psc_trust_site_lookup, here("lookups", "psc_trust_site_lookup.csv"), row.names = F)

chosenlib$upload_file(
  dest = str_glue("{base_url}/1. Master files/psc_trust_site_lookup.csv"),
  src = "lookups/psc_trust_site_lookup.csv"
)
