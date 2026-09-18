# script to find submissions from PSC on sharepoint and collate data for data processing outputs 

# navigate sharepoint to hin locations
hin_folders <- list_SP_files(base_url) |>
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
          row.names = FALSE
)

# ==============================================================================
# function to process data from excel sheets
# ==============================================================================

#' This function goes into a the completed excel worksheet sent by a HIN
#' It extracts data from the different Martha's rule tabs and organises it in a tidy format.

#' @param tf_name String. Temporary file path used to save worksheet locally.
#            Worksheet is downloaded from sharepoint site
#' @param sheet_name String. Name of a tab in the worksheet to extract data from
#' @param sheet_range String. Valid excel sheet range where data is recorded (e.g., "B6:M30")
#' @param del_cols Vector. Column names to be deleted. These columns do not contain data. 
#'                 They serve layout purposes in excel sheet 
#' @param tidy_col_names Vector. Tidy names to be used to rename columns 
#' @param col_names Boolean. Controls whether first row of range provided is used as col names
#'                  Defaulted to TRUE
#' @param hin_name String. Name of a health innovation network that submitted data

#' @return Tidy table containing adoption data

collect_sheet_data <- function(
    tf_name,
    sheet_name,
    sheet_range,
    del_cols,
    tidy_col_names,
    col_names = TRUE,
    hin_name) {

  # locate raw data submiited by HIN
  data_marthas <- read_excel(
    path = tf_name,
    sheet = sheet_name,
    range = sheet_range,
    col_names = col_names
  ) |>
    clean_names() |>
    # no data in this column, used for better print out in excel doc
    select(-all_of(del_cols)) |>
    remove_empty("rows")

  # assign tidy names
  names(data_marthas) <- tidy_col_names

  data_marthas_tidy <- data_marthas |>
    # TODO: REMOVE ???
    #filter(if_any(matches('adults|paediatric'), ~!is.na(.))) |>
    mutate(updated_psc_name = hin, .before = trust_code) |>
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

  return(data_marthas_tidy)
}

# empty data frames to save submissions
results_marthas_adult_paeds <- tibble()
results_marthas_mat_neo <- tibble()
results_marthas_ed <- tibble()

# for testing
# hin_folders = 'Manchester HIN'

for (hin in hin_folders) {
  # identify submission
  message(glue::glue("** Checking data for {hin} **"))
  hin_files <- list_SP_files(paste0(base_url, "/", hin))

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

  tf_name <- paste0("data/", hin, "_submission_file.xlsx")
  get_SP_file(paste0(hin, "/", hin_submission_file),
              here(tf_name))

  # read martha's rule data
  message("Reading Martha's Rule submissions: adult and paeds")

  col_names_adult_paeds <- c('trust_code', 'name_of_trust',
                 'site_code', 'name_of_site', 'phase',
                 'adults_patient_check_in',
                 'adults_independent_clinical_review',
                 'adults_escalation_available_to_patient_carers',
                 'paediatric_patient_check_in',
                 'paediatric_independent_clinical_review',
                 'paediatric_escalation_available_to_patient_carers')

  # execute function
  data_marthas_tidy_adult_paeds <- collect_sheet_data(
    tf_name = tf_name,
    sheet_name = "MR - Adult & Paeds",
    sheet_range = "B6:M30",
    del_cols = c('x9'),
    tidy_col_names = col_names_adult_paeds,
    hin_name = hin)

  # repeat for matneo settings
  message("Reading Martha's Rule submissions: maternity and neonatal")

  col_names_adult_paeds <- c('trust_code', 'name_of_trust',
                             'site_code', 'name_of_site',
                             'antenatal_patient_check_in',
                             'antenatal_independent_clinical_review',
                             'antenatal_escalation_available_to_patient_carers',
                             'intrapartum_patient_check_in',
                             'intrapartum_independent_clinical_review',
                             'intrapartum_escalation_available_to_patient_carers',
                             'postnatal_patient_check_in',
                             'postnatal_independent_clinical_review',
                             'postnatal_escalation_available_to_patient_carers',
                             'transitional_patient_check_in',
                             'transitional_independent_clinical_review',
                             'transitional_escalation_available_to_patient_carers',
                             'neonatal_patient_check_in',
                             'neonatal_independent_clinical_review',
                             'neonatal_escalation_available_to_patient_carers'
                             )
  
  data_marthas_tidy_mat_neo <- collect_sheet_data(
    tf_name = tf_name, 
    sheet_name = "MR - Maternity & Neonatal",
    sheet_range = "B7:X30",
    col_names = F,
    del_cols = c('x8', 'x12', 'x16', 'x20'),
    tidy_col_names = col_names_adult_paeds,
    hin_name = hin)
  
  # and for ed
  message("Reading Martha's Rule submissions: emergency department")
  
  col_names_ed <- c('trust_code', 'name_of_trust',
                     'site_code', 'name_of_site',
                     'adults_waiting_room_patient_check_in',
                     'adults_waiting_room_independent_clinical_review',
                     'adults_waiting_room_escalation_available_to_patient_carers',
                     'adults_majors_patient_check_in',
                     'adults_majors_independent_clinical_review',
                     'adults_majors_escalation_available_to_patient_carers',
                     'adults_tes_patient_check_in',
                     'adults_tes_independent_clinical_review',
                     'adults_tes_escalation_available_to_patient_carers',
                     'adults_resus_patient_check_in',
                     'adults_resus_independent_clinical_review',
                     'adults_resus_escalation_available_to_patient_carers',
                     'paediatric_waiting_room_patient_check_in',
                     'paediatric_waiting_room_independent_clinical_review',
                     'paediatric_waiting_room_escalation_available_to_patient_carers',
                     'paediatric_majors_patient_check_in',
                     'paediatric_majors_independent_clinical_review',
                     'paediatric_majors_escalation_available_to_patient_carers',
                     'paediatric_resus_patient_check_in',
                     'paediatric_resus_independent_clinical_review',
                     'paediatric_resus_escalation_available_to_patient_carers'
  )

  data_marthas_tidy_ed <- collect_sheet_data(
    tf_name = tf_name,
    sheet_name = "MR - Emergency Departments",
    sheet_range = "B7:AF30",
    del_cols = c('x8', 'x12', 'x16', 'x20', 'x24', 'x28'),
    tidy_col_names = col_names_ed,
    col_names = FALSE,
    hin_name = hin)

  # append extracted data to list
  results_marthas_adult_paeds <- bind_rows(results_marthas_adult_paeds,
                                           data_marthas_tidy_adult_paeds)
  results_marthas_mat_neo <- bind_rows(results_marthas_mat_neo,
                                       data_marthas_tidy_mat_neo)
  results_marthas_ed <- bind_rows(results_marthas_ed,
                                  data_marthas_tidy_ed)
  file.remove(tf_name)
}

if (length(unique(results_marthas_adult_paeds$updated_psc_name)) != 15 &
    length(unique(results_marthas_mat_neo$updated_psc_name)) != 15 &
    length(unique(results_marthas_ed$updated_psc_name)) != 15
    ){
  stop('Data not appended correctly')
}

# write quarterly data files
quarter_string <- reporting_quarter_string |>
  str_replace_all("/| ", "_")

time_stamp_ext <- format(Sys.time(), "%Y_%m_%d_%H%M%S.csv")

# create paths for saving
marthas_adults_paeds_submissions_path <- glue::glue("output/marthas_adults_paeds_psc_submissions_{quarter_string}_processed_{time_stamp_ext}")
marthas_mat_neo_submissions_path <- glue::glue("output/marthas_mat_neo_psc_submissions_{quarter_string}_processed_{time_stamp_ext}")
marthas_ed_submissions_path <- glue::glue("output/marthas_ed_psc_submissions_{quarter_string}_processed_{time_stamp_ext}")

# loop over results and file names to save as csv's
results <- list(results_marthas_adult_paeds,
                results_marthas_mat_neo,
                results_marthas_ed)

paths <- c(marthas_adults_paeds_submissions_path,
           marthas_mat_neo_submissions_path,
           marthas_ed_submissions_path)

for (i in seq_along(results)){
  write.csv(
    x = results[[i]],
    file = here(paths[[i]]),
    row.names = FALSE
  )
}

message(glue::glue('{reporting_quarter_string} data files have been produced'))
