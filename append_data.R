# ==============================================================================
# function to append quarterly submissions to files with cumulative data
# ==============================================================================

#' This function takes the output csv files generated in read_qart_submissions.R
#' and appends the data in those files to cumulative data files 

#' @param previous_submission String. Name of the file containing cumulative data.
#'                            Needs to include .csv extension
#' @param file_path String. Local file path with current submissions from all 15 HINs
#'                  paths are produced in read_qart_submissions.R
#' @param updated_file_name String. Valid excel sheet range where data is recorded (e.g., "B6:M30")

#' @output updated csv file with current and sumulative data. File is saved on sharepoint

append_data <- function(previous_submission,
                        file_path,
                        updated_file_name
                        ){
  # identify file with data from previous submissions on sharepoint
  master_files_location <- chosenlib$get_item(
    glue::glue("{base_url}/{master_files_folder}"))
  
  master_files <- master_files_location$list_files()
  
  previous_submission_file <- master_files |>
    select(name) |>
    filter(str_detect(name, previous_submission))
  
  if (nrow(previous_submission_file) == 0) {
    stop(
      glue::glue("file not found in location '{master_files_location}':\n file name provided: '{previous_submission}'"))
  }
  
  previous_submission_file_item <- master_files_location$get_item(previous_submission_file)
  
  # download file and save in local data directory
  previous_submission_path <- here('data', 
                                   str_glue('{previous_submission_file}'))
  
  previous_submission_file_item$download(
    dest = previous_submission_path,
    overwrite = T
  )
  
  # read previous submissions data
  previous_submission_data <- read.csv(previous_submission_path)
  
  data_name <- str_extract(previous_submission, '^.*?(?=_data_upto)')

  message(glue("Previous {data_name} file has data up to "), 
          max(previous_submission_data$quarter))
  
  # collated data produced in read_quart_submissions.R
  reporting_quarter_submissions <- read.csv(
    here(file_path)) |>
    clean_names() 
  
  # bind rows
  all_submissions <- previous_submission_data |>
    bind_rows(reporting_quarter_submissions) 
  
  message("Saving updated Martha's data to SharePoint")
  
  file_name <- glue('{updated_file_name}_data_upto_{quarter_string}.csv')
  
  write.csv(all_submissions, 
            str_glue("output/{file_name}"),
            row.names = F)
  
  chosenlib$upload_file(
    dest = str_glue("{base_url}/{master_files_folder}/{file_name}"),
    src = str_glue("output/{file_name}")
  )
  
  message('Submissions for ', unique(reporting_quarter_submissions$quarter), 
          ' have been appended')
}

# TODO: add function calls to append data for martha's ed and matneo
# might be better to do once 2627 Q2 data is received 
append_data(
  previous_submission = previous_marthas_submissions_adults_paeds,
  file_path = marthas_adults_paeds_submissions_path,
  updated_file_name = 'marthas_qart_adults_paeds'
  )

