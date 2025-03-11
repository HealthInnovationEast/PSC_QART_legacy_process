# retrieve previous submissions data
if (retroactive_fixes){
  # hard code the file name when retroactive fixes have been made (file produced in data_cleanse.R)
  previous_submissions_cleansed <- read.csv(here(str_glue('output/mat_neo_qart_cleansed_upto_2024_25_Q2.csv'))) 
} else {
  # otherwise file name will be taken from parameter in process_flow.R 
  # i.e., after the first append has been made and retroactive changes have been taken care of 
  previous_submissions_cleansed <- read.csv(here(str_glue('output/{previous_submissions_file_name}')))
}

message('Previous submissions have data up to ', max(previous_submissions_cleansed$quarter))

# collated data from reporting_quarter_submissions_path
reporting_quarter_submissions <- read.csv(here(reporting_quarter_submissions_path)) |>
  clean_names() |>
  rename(organisation_after_cleanse = trust)

message('Submissions for ', unique(reporting_quarter_submissions$quarter), 
        ' will be appended by this script')

# bind rows
all_submissions <- previous_submissions_cleansed |>
  bind_rows(reporting_quarter_submissions) 

# write to share point
write.csv(all_submissions, 
          str_glue("output/mat_neo_qart_all_data_upto_{quarter_string}.csv"),
          row.names = F)

chosenlib$upload_file(
  dest = str_glue("{base_url}/{master_files_folder}/mat_neo_qart_all_data_upto_{quarter_string}.csv"),
  src = str_glue("output/mat_neo_qart_all_data_upto_{quarter_string}.csv")
)
