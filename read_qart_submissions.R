# script to find submissions from PSC on sharepoint and collate data for data processing outputs 

# first go to master files folder and download martha's rule data submitted by nhse improvement team
message('Fetching phase 2 site submissions by NHSE improvement team')

master_files_location <- chosenlib$get_item(glue::glue("{base_url}/{master_files_folder}"))

master_files <- master_files_location$list_files()

marthas_nhse_quarterly_submission_file <- master_files |>
  select(name) |>
  # submissions must be saved with returned ending on file name
  filter(str_detect(name, marthas_phase_2_nhse_submission))

if (nrow(marthas_nhse_quarterly_submission_file) == 0) {
  stop(glue::glue("file not found in location '{master_files_folder}':\n file name provided: '{marthas_phase_2_nhse_submission}'"))
}

marthas_nhse_quarterly_submission <- master_files_location$get_item(marthas_nhse_quarterly_submission_file)

# download file 
marthas_nhse_quarterly_submission_path <- here('data', str_glue('{marthas_nhse_quarterly_submission_file}'))

marthas_nhse_quarterly_submission$download(
  dest = marthas_nhse_quarterly_submission_path,
  overwrite = T
)

# read martha's data submitted by nhse 
# note that we will to append additional data from PSC's in loops
data_marthas_nhse <- read_excel(
  path = marthas_nhse_quarterly_submission_path, 
  sheet = 1,
  # select frist 100 rows (in case list of sites increases)
  range = "A7:M100"
) |> clean_names() |>
  # removing decorative column
  select(-'x10') |>
  # remove any empty rows 
  remove_empty("rows") |>
  # flag 
  mutate(nhse_natps_submisison = TRUE)

# name reparation 
names(data_marthas_nhse) <- c('updated_psc_name', 'trust_code', 'name_of_trust', 
                         'site_code', 'name_of_site', 'phase',
                         'adults_patient_check_in', 
                         'adults_independent_clinical_review', 
                         'adults_escalation_available_to_patient_carers',
                         'paediatric_patient_check_in', 
                         'paediatric_independent_clinical_review', 
                         'paediatric_escalation_available_to_patient_carers',
                         'nhse_natps_submisison')

# navigate sharepoint to hin locations
hin_folders <- dr$list_files() |>
  select(name) |>
  filter(str_detect(name, "HIN$")) |>
  arrange(name) |>
  as.vector() |>
  unlist() |> # so that vector length reflects number of pscs
  unname()

# write current hin names for lookup file(s)
hin_names <- hin_folders |> as.data.frame()

write.csv(hin_names,
          file = here("lookups", "hin_names.csv"),
          row.names = F
)

# empty data frames to save submissions
results_marthas <- tibble()
results_mat_neo <- tibble()

#hin_folders = 'Manchester HIN'

for (hin in hin_folders) {
  # identify submission
  message(glue::glue("** Checking data for {hin} **"))

  hin_dir <- chosenlib$get_item(glue::glue("{base_url}/{hin}"))

  hin_files <- hin_dir$list_files()

  hin_submission_file <- hin_files |>
    select(name) |>
    # submissions must be saved with returned ending on file name
    filter(str_detect(name, "(?i)returned"))

  if (nrow(hin_submission_file) == 0) {
    print(glue::glue("Skipping {hin}"))
    print(glue::glue("There are no returned files"))
    next
  }

  if (nrow(hin_submission_file) > 1) {
    print(glue::glue("Skipping {hin}"))
    print("There are multiple returned files:")
    print(glue::glue("{hin_submission_file}"))
    next
  }

  hin_submission <- hin_dir$get_item(hin_submission_file)

  # download file and store temporarily
  tf <- tempfile(
    pattern = str_remove(hin_submission_file, fixed(".xlsx")),
    fileext = ".xlsx"
  )

  hin_submission$download(
    dest = tf,
    overwrite = T
  )
  
  # read martha's rule data
  message("Reading Martha's Rule submissions")
  
  data_marthas <- read_excel(
    path = tf, sheet = "Martha's Rule",
    range = "B7:M30" 
  ) |> 
    clean_names() |>
    # no data in this column, used for better print out in excel doc
    select(-'x9') |>
    remove_empty("rows")
  
  names(data_marthas) <- c('trust_code', 'name_of_trust', 
                           'site_code', 'name_of_site', 'phase',
                           'adults_patient_check_in', 
                           'adults_independent_clinical_review', 
                           'adults_escalation_available_to_patient_carers',
                           'paediatric_patient_check_in', 
                           'paediatric_independent_clinical_review', 
                           'paediatric_escalation_available_to_patient_carers')
  
  data_marthas_tidy <- data_marthas |>
    #filter(!is.na(adults_patient_check_in) & !is.na(paediatric_patient_check_in)) |>
    filter(if_any(matches('adults|paediatric'), ~!is.na(.))) |>
    mutate(updated_psc_name = hin, .before = trust_code) |>
    bind_rows(data_marthas_nhse |> filter(updated_psc_name == hin)) |>
    mutate(
      quarter = reporting_quarter_string,
      .after = name_of_site
    )
  
  # check there's info for all codes
  missing_code_check <- data_marthas |> 
    filter(!site_code %in% data_marthas_tidy$site_code) 
  
  incomplete_sites <- missing_code_check |> 
    select(site_code, name_of_site) |> 
    as.character() |>
    paste(collapse = '-')
  
  if (nrow(missing_code_check) > 0) {
    warning(str_glue('{hin}: MR data missing for {incomplete_sites}'))
  }
  
  # check psc's are not submitting info expected from nhse and viceversa
  duplicate_code_check <- data_marthas_tidy |>
    group_by(site_code, name_of_site) |>
    summarise(site_occurrence = n()) |>
    ungroup() |>
    filter(site_occurrence > 1)
  
  duplicated_sites <- duplicate_code_check |> 
    select(site_code, name_of_site) |> 
    as.character() |>
    paste(collapse = '-')
  
  if (nrow(duplicate_code_check) > 0) {
    warning(str_glue("Skipping {hin}"))
    warning(str_glue('{hin}: duplicated MR data for {duplicated_sites}'))
    next
  }
  
  # append extracted data to list
  results_marthas <- bind_rows(results_marthas, data_marthas_tidy)

  # read matneo data
  message("Reading MatNeo submissions")
  
  data_mat_neo <- read_excel(
    path = tf, sheet = "MatNeo",
    range = "B7:N44"
  )

  # cut 1 opt data
  data_opt <- data_mat_neo[1:16, ]

  data_opt[1, 1] <- "api_icb_code"
  data_opt[1, 2] <- "api_org_code"
  data_opt[1, 3] <- "api_icb_name"
  data_opt[1, 4] <- "organisation_name_verified"
  
  data_opt_tidy <- data_opt |>
    row_to_names(row_number = 1) |>
    remove_empty("rows")

  # validation - if cells are blank don't continue with upload
  if (purrr::is_empty(which(is.na(data_opt_tidy))) == FALSE) {
    print(glue::glue("Skipping {hin}"))
    print("Optimisation data grid was not fully complete")
    next
  }

  # cut 2 newtt2 data
  data_nwwtt2_mews <- data_mat_neo[21:46, 1:8]

  nwwtt2_location <- data.frame(which(data_nwwtt2_mews == "NEWTT 2", arr.ind = T))
  # validation

  if (nwwtt2_location$col != 5) {
    print(glue::glue("Skipping {hin}"))
    print("NWWTT2 data not found in expected location")
    next
  }

  data_nwwtt2 <- data_nwwtt2_mews[, 1:nwwtt2_location$col] |>
    tail(-2)

  names(data_nwwtt2) <- c('api_icb_code', 'api_org_code', 'api_icb_name', 
                          'organisation_name_verified', "Newtt2")

  data_nwwtt2_tidy <- data_nwwtt2 |>
    remove_empty("rows")

  # validation
  if (purrr::is_empty(which(is.na(data_nwwtt2_tidy))) == FALSE) {
    print(glue::glue("Skipping {hin}"))
    print("NWWTT2 grid was not fully complete")
    next
  }

  # cut 3 mews data
  mews_location <- data.frame(which(data_nwwtt2_mews == "MEWS", arr.ind = T))

  # validation
  if (mews_location$col != 7) {
    print(glue::glue("Skipping {hin}"))
    print("MEWS data not found in expected location")
    next
  }

  data_mews <- data_nwwtt2_mews[, c(1:4, mews_location$col)] |>
    tail(-2)

  names(data_mews) <- c('api_icb_code', 'api_org_code', 'api_icb_name', 
                        'organisation_name_verified', "Mews")

  data_mews_tidy <- data_mews |>
    remove_empty("rows")

  # validation
  if (purrr::is_empty(which(is.na(data_mews_tidy))) == FALSE) {
    print(glue::glue("Skipping {hin}"))
    print("MEWS grid was not fully complete")
    next
  }

  # now join all 3 cuts
  data_combined <- data_opt_tidy |>
    mutate(
      updated_psc_name = hin,
      .before = api_icb_code
    ) |>
    left_join(data_nwwtt2_tidy, by = c('api_icb_code', 'api_org_code', 'api_icb_name', 
                                       'organisation_name_verified')) |>
    left_join(data_mews_tidy, by = c('api_icb_code', 'api_org_code', 'api_icb_name', 
                                     'organisation_name_verified')) |>
    mutate(
      quarter = reporting_quarter_string,
      .after = organisation_name_verified
    ) |>
    # order columns to emulate structure in previous submissions data file
    select(updated_psc_name, api_icb_code, api_icb_name,
           api_org_code, organisation_name_verified, quarter:Mews)

  print(glue::glue("Successful data extraction for {hin}. Data retrieved from:"))
  print(glue::glue("{hin_submission_file}"))

  results_mat_neo <- rbind(results_mat_neo, data_combined)
  
}

if (length(unique(results_marthas$updated_psc_name)) != 15 & 
    length(unique(results_mat_neo$updated_psc_name)) != 15
    ){
  stop('Data not appended correctly')
}

# write quarterly data files
quarter_string <- reporting_quarter_string |>
  str_replace_all("/| ", "_")

time_stamp_ext <- format(Sys.time(), "%Y_%m_%d_%H%M%S.csv")
marthas_submissions_path <- glue::glue("output/marthas_psc_nhse_submissions_{quarter_string}_processed_{time_stamp_ext}")
mat_neo_opt_submissions_path <- glue::glue("output/mat_neo_optimisation_psc_submissions_{quarter_string}_processed_{time_stamp_ext}")

write.csv(results_marthas,
  file = here(marthas_submissions_path),
  row.names = F
)

write.csv(results_mat_neo,
          file = here(mat_neo_opt_submissions_path),
          row.names = F
)

message(glue::glue('{reporting_quarter_string} data files have been produced'))
