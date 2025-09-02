# retrieve previous submissions data
previous_mat_neo_submissions_cleansed <- read.csv(here(str_glue('output/{previous_mat_neo_submissions_file_name}')))

message('Previous submissions have data up to ', max(previous_mat_neo_submissions_cleansed$quarter))

# collated data from psc produced in read_quart_submissions.R
reporting_quarter_mat_neo_submissions <- read.csv(here(mat_neo_opt_submissions_path)) |>
  clean_names() 

message('Submissions for ', unique(reporting_quarter_mat_neo_submissions$quarter), 
        ' will be appended by this script')

# bind rows
all_submissions <- previous_mat_neo_submissions_cleansed |>
  bind_rows(reporting_quarter_mat_neo_submissions) 

# write to share point
write.csv(all_submissions, 
          str_glue("output/mat_neo_qart_all_data_upto_{quarter_string}.csv"),
          row.names = F)

chosenlib$upload_file(
  dest = str_glue("{base_url}/{master_files_folder}/mat_neo_qart_all_data_upto_{quarter_string}.csv"),
  src = str_glue("output/mat_neo_qart_all_data_upto_{quarter_string}.csv")
)
