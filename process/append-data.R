# ==============================================================================
# function to append quarterly submissions to files with cumulative data
# ==============================================================================

#' This function takes the output csv files generated in read-qart-submissions.R
#' and appends the data in those files to cumulative data files 

#' @param previous_submission String. Name of the file containing cumulative data.
#'                            Needs to include .csv extension
#' @param file_path String. Local file path with current submissions from all 15 HINs
#'                  paths are produced in read-qart-submissions.R
#' @param updated_file_name_start String. Reflects the beginning of the
#'              entire file name to be used to save file with current and cumulative data
#' @output updated csv file with current and cumulative data. File is saved on sharepoint

append_data <- function(previous_submission,
                        file_path,
                        updated_file_name_start
                        ) {
  # identify file with data from previous submissions on sharepoint
  master_files <- list_SP_files(paste0(base_url, "/", master_files_folder))

  previous_submission_file <- master_files |>
    select(name) |>
    filter(str_detect(name, previous_submission))

  if (nrow(previous_submission_file) == 0) {
    stop(
      glue::glue("file not found in location '{master_files_location}':\n file name provided: '{previous_submission}'"))
  }

  # download file and save in local data directory
  get_SP_file(paste0(master_files_folder, "/", previous_submission_file),
               here('data', previous_submission_file))

  # read previous submissions data
  previous_submission_data <- read.csv(here('data', previous_submission_file))

  data_name <- str_extract(previous_submission, '^.*?(?=_data_upto)')

  message(glue("Previous {data_name} file has data up to "),
          max(previous_submission_data$quarter))

  # collated data produced in read_quart_submissions.R
  reporting_quarter_submissions <- read.csv(
    here(file_path)) |>
    clean_names() |>
    mutate_all(as.character)

  # bind rows
  all_submissions <- previous_submission_data |>
    mutate_all(as.character) |>
    bind_rows(reporting_quarter_submissions)

  message("Saving updated Martha's data to SharePoint")

  file_name <- glue('{updated_file_name_start}_data_upto_{quarter_string}.csv')

  write.csv(all_submissions,
            str_glue("output/{file_name}"),
            row.names = FALSE)

  upload_SP_file(here("output", file_name),
                 paste0(master_files_folder, "/", file_name))

  message('Submissions for ', unique(reporting_quarter_submissions$quarter),
          ' have been appended')
}

# deploy function, maybe loop
append_data(
  previous_submission = previous_marthas_submissions_adults_paeds,
  file_path = marthas_adults_paeds_submissions_path,
  updated_file_name_start = 'marthas_qart_adults_paeds'
)

append_data(
  previous_submission = previous_marthas_submissions_matneo,
  file_path = marthas_mat_neo_submissions_path,
  updated_file_name_start = 'marthas_qart_matneo'
)

append_data(
  previous_submission = previous_marthas_submissions_ed,
  file_path = marthas_ed_submissions_path,
  updated_file_name_start = 'marthas_qart_ed'
)
