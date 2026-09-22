### html report with martha's data
quarto_render(
  "marthas-qart-report.qmd",
  execute_params = list(
    master_files_folder = master_files_folder,
    reporting_quarter = reporting_quarter_string,
    quarter_string = reporting_quarter_file_string
    ))

new_file_name <- glue::glue("Marthas_QART_{quarter_string}.html")

# move the quarto report into the output folder and change name
file.rename('marthas-qart-report.html', here(glue::glue('output/{new_file_name}')))

# save file to SharePoint
upload_SP_file(here("output", new_file_name),
               paste0(slides_folder, "/", new_file_name))
