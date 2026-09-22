message('Adding trust site codes...')

# bring psc-icb-trust lookup created via active-organisation-check.R
psc_icb_trust_lookup <- read.csv(here("lookups", "psc_icb_trust_lookup.csv"))

# download look up provided by improvement team
message(str_glue('Site look up list provided: {site_lookup_file_name}'))
get_SP_file(paste0(master_files_folder, "/", site_lookup_file_name),
            here("lookups", site_lookup_file_name))

# prepare look up
full_sites_list <- read_excel(here("lookups", site_lookup_file_name)) |>
  remove_empty("rows") |>
  select(`HIN/ PSC`, `Site SDCS Code`, `Site phase`, MatNeo, `ED Adult`, `ED Paeds`)

MatNeo_ED_phases <- full_sites_list |>
  mutate(ED = ifelse(`ED Adult` == "y" | `ED Paeds` == "y",
                     "y", "n")) |>
  select(-c(`ED Adult`, `ED Paeds`)) |>
  pivot_longer(cols = c(MatNeo, ED),
               names_to = "extra_phases",
               values_to = "exists") |>
  filter(exists == "y") |>
  select(-c(exists, `Site phase`)) |>
  rename(phase = extra_phases)

site_lookup <- full_sites_list |>
  select(`HIN/ PSC`, `Site SDCS Code`, phase = `Site phase`) |>
  mutate(phase = as.character(phase)) |>
  bind_rows(MatNeo_ED_phases) |>
  rename(hin_psc = `HIN/ PSC`,
         site_sdcs_code = `Site SDCS Code`)

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

  relationships <- length(site_info$Organisation$Rels$Rel)

  # setup counter for how many hits were potentially correct
  n_status <- 0
  rel_n <- NA

  for (relationship in 1:relationships) {
    rel_id <- site_info$Organisation$Rels$Rel[[relationship]]$id
    rel_status <- site_info$Organisation$Rels$Rel[[relationship]]$Status
    if (rel_id == "RE6" & rel_status == "Active") { # RE5 is the parent trust relationship
      n_status <- n_status + 1
      rel_n <- relationship # extract relationship number=
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

call_site_postcode_and_parent_codes <- apply(site_lookup_parsed,
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

upload_SP_file(here("lookups/psc_trust_site_lookup.csv"),
               paste0(master_files_folder, "/psc_trust_site_lookup.csv"))
