previous_submissions <- c(previous_marthas_submissions_file_name,
                          previous_mat_neo_submissions_file_name
                          )

for (previous_submission in previous_submissions){
  # download previous submission from sharepoint
  previous_submission_file <- master_files |>
    select(name) |>
    # submissions must be saved with returned ending on file name
    filter(str_detect(name, previous_submission))
  
  if (nrow(previous_submission_file) == 0) {
    stop(glue::glue("file not found in location '{master_files_location}':\n file name provided: '{previous_submission}'"))
  }
  
  previous_submission_file_item <- master_files_location$get_item(previous_submission_file)

  # download file 
  previous_submission_path <- here('data', 
                                   str_glue('{previous_submission_file}'))
  
  previous_submission_file_item$download(
    dest = previous_submission_path,
    overwrite = T
  )
  
  # read previous submissions data
  previous_submission_data <- read.csv(previous_submission_path)
  
  if ( str_detect(previous_submission, 'marthas') ) {
    message("Previous Martha's submissions have data up to ", 
            max(previous_submission_data$quarter))
    
    # collated data produced in read_quart_submissions.R
    reporting_quarter_submissions <- read.csv(
      here(marthas_submissions_path)) |>
      clean_names() 
    
    # bind rows
    all_submissions <- previous_submission_data |>
      bind_rows(reporting_quarter_submissions) 
    
    message("Saving updated Martha's data to SharePoint")
    
    file_name <- glue('marthas_qart_all_data_upto_{quarter_string}.csv')
    
  } else if ( str_detect(previous_submission, 'mat_neo') ) {
    
    message("Previous MatNeo submissions have data up to ", 
            max(previous_submission_data$quarter))
    
    # collated data produced in read_quart_submissions.R
    reporting_quarter_submissions <- read.csv(
      here(mat_neo_opt_submissions_path)) |>
      clean_names() 
    
    # bind rows
    all_submissions <- previous_submission_data |>
      bind_rows(reporting_quarter_submissions) 
    
    message('Saving updated MatNeo data to SharePoint')
    
    file_name <- glue('mat_neo_qart_all_data_upto_{quarter_string}.csv')
  }
  
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

