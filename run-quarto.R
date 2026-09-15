### html report with martha's data
quarto_render(
  "marthas-qart-report.qmd", 
  execute_params = list(
    reporting_quarter = reporting_quarter_string,
    master_files_folder = master_files_folder,
    # below parameter is generated in read-qart-submissions.R
    quarter_string = quarter_string
    ))

new_file_name <- glue::glue("Marthas_QART_{quarter_string}.html")

# move the quarto report into the output folder and change name
file.rename('marthas-qart-report.html', here(glue::glue('output/{new_file_name}')))

# save file to SharePoint 
chosenlib$upload_file(
  dest = str_glue("{base_url}/{slides_folder}/{new_file_name}"),
  src = str_glue('output/{new_file_name}')
)
