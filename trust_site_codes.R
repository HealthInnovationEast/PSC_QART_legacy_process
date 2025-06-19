# bring psc-icb-trust lookup created via active_Organisation_check.R  
psc_icb_trust_lookup <- read.csv(here("lookups", "psc_icb_trust_lookup.csv")) 
pscs <- unique(psc_icb_trust_lookup$updated_psc_name)

# download look up provided by improvement team
site_lookup_file_name <- '20250618 Site list for QART template.xlsx'
site_lookup_file <- master_files_dr$get_item(str_glue({site_lookup_file_name}))
site_lookup_file$download(dest = here("lookups", str_glue({site_lookup_file_name})), 
                            overwrite = T)

# upload look up
site_lookup <- read_excel(here("lookups", str_glue({site_lookup_file_name})),
                          sheet = 'PHASE 2 SITES',
                          n_max = 63) |>
  clean_names()

# wrangle to allow left join to psc_icb_trust_lookup
site_lookup_parsed <- site_lookup |>
  select(-c(psc), # not useful as using abbreviated names
         name_of_trust,
         name_of_site,
         trust_site_code = ods_code, 
         to_grey_out = starts_with('will_be')
         ) |>
  filter(trust_site_code != 'RTGX1', # invalid code, doesn't exist in API
         trust_site_code != 'NQTA5' # independent sector, closed organisation
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

# psc_icb_trust_lookup |>
#   left_join(site_lookup_parsed, by = c('api_current_code' = 'trust_code_derived')) |>
#   View()

