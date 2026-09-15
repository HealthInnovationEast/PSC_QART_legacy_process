message('Adding trust site codes...')

# bring psc-icb-trust lookup created via active-organisation-check.R  
psc_icb_trust_lookup <- read.csv(here("lookups", "psc_icb_trust_lookup.csv")) 
pscs <- unique(psc_icb_trust_lookup$updated_psc_name)

# download look up provided by improvement team
message(str_glue('Site look up list provided: {site_lookup_file_name}'))
site_lookup_file <- master_files_dr$get_item(str_glue({site_lookup_file_name}))
site_lookup_file$download(dest = here("lookups", str_glue({site_lookup_file_name})), 
                            overwrite = T)

# upload look up
sites_phase_1 <- read_excel(here("lookups", str_glue({site_lookup_file_name})),
                            sheet = 'Phase 1 sites') |>
  clean_names() |>
  select(hin_psc, site_sdcs_code) |>
  mutate(phase = '1')

sites_phase_2 <- read_excel(here("lookups", str_glue({site_lookup_file_name})),
                          sheet = 'Phase 2 sites') |>
  clean_names() |>
  select(hin_psc, site_sdcs_code) |>
  mutate(phase = '2')

sites_phase_3_matneo <- read_excel(here("lookups", str_glue({site_lookup_file_name})),
                            sheet = 'Phase 3 MatNeo') |>
  clean_names() |>
  select(hin_psc, site_sdcs_code) |>
  mutate(phase = 'matneo')

sites_phase_3_ed <- read_excel(here("lookups", str_glue({site_lookup_file_name})),
                                   sheet = 'Phase 3 ED') |>
  clean_names() |>
  select(hin_psc, site_sdcs_code) |>
  mutate(phase = 'ed')

# combine all sites 
# there's duplicate codes here given different phases
site_lookup <- bind_rows(sites_phase_1, sites_phase_2, 
                         sites_phase_3_matneo, sites_phase_3_ed)

# wrangle to allow left join to psc_icb_trust_lookup
site_lookup_parsed <- site_lookup |>
  select(trust_site_code = site_sdcs_code) |>
  # have a unique list of codes so API calls aren't repeated unnecessarily
  distinct(trust_site_code)

# use site codes in an API call
call_by_site_code <- function(trust_site_code) {
  site_info <- content(GET(paste0(
    "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/",
    trust_site_code
  )))
  
  api_site_name <- site_info$Organisation$Name
  api_site_postcode <- site_info$Organisation$GeoLoc$Location$PostCode
  
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
    api_site_postcode,
    api_parent_code
  )
}

call_site_postcode_and_parent_codes <- apply(site_lookup_parsed #|> 
                                               #select(trust_site_code)
                                             , 
                                             1, call_by_site_code) |>
  bind_rows() 

# get trust names, start, and end dates using function developed in active_orgsanisation_check.R 
call_org_names_and_dates <- apply(
  call_site_postcode_and_parent_codes |> 
    distinct(api_parent_code)
                                  , 1, 
                                  call_by_org_code) |>
  bind_rows() |>
  relocate(api_date_end, .after = api_date_start) |>
  mutate(api_date_start = ymd(api_date_start),
         api_date_end = ymd(api_date_end))

# start building look up
# merge ods details for nhs trsut sites and parent trusts
ods_site_trust_details <- call_org_names_and_dates |>
  left_join(call_site_postcode_and_parent_codes, 
            by = c('api_org_code' = 'api_parent_code')) |>
  select(api_org_code, api_org_name, 
         trust_site_code, api_site_name,
         api_site_postcode) 
  
# join ods details to list by hin
psc_trust_site_lookup <- site_lookup |>
  left_join(ods_site_trust_details, 
            by = c('site_sdcs_code' = 'trust_site_code')) |>
  mutate(updated_psc_name = 
           case_when(
             hin_psc == 'East Midlands' ~ 'East Midlands HIN',
             hin_psc == 'East' ~ 'Eastern HIN',
             hin_psc == 'Eastern' ~ 'Eastern HIN',
             hin_psc == 'ICHP' ~ 'Imperial College Health Partners HIN',
             hin_psc == 'KSS' ~ 'Kent Surrey Sussex HIN',
             hin_psc == 'Manchester' ~ 'Manchester HIN',
             hin_psc == 'NENC' ~ 'North East and North Cumbria (NENC) HIN',
             hin_psc == 'NWC' ~ 'North West Coast HIN',
             hin_psc == 'Oxford' ~ 'Oxford HIN',
             hin_psc == 'HIN South London' ~ 'South London HIN',
             hin_psc == 'South West' ~ 'South West HIN',
             hin_psc == 'UCLP' ~ 'UCLPartners HIN',
             hin_psc == 'Wessex' ~ 'Wessex HIN',
             hin_psc == 'West Midlands' ~ 'West Midlands HIN',
             hin_psc == 'WoE' ~ 'West of England HIN',
             hin_psc == 'Y&H' ~ 'Yorkshire & Humber HIN'
             ),
         .before = site_sdcs_code
         ) |>
  select(-hin_psc) |>
  # col order
  select(updated_psc_name, api_org_code, api_org_name, 
         site_sdcs_code, api_site_name, api_site_postcode,
         phase) |>
  # row order
  arrange(updated_psc_name, api_org_name, api_site_name)

# save work locally and on sharepoint
write.csv(psc_trust_site_lookup, here("lookups", "psc_trust_site_lookup.csv"), row.names = F)

chosenlib$upload_file(
  dest = str_glue("{base_url}/1. Master files/psc_trust_site_lookup.csv"),
  src = "lookups/psc_trust_site_lookup.csv"
)
