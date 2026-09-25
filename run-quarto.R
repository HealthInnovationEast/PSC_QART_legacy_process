### html report with martha's data
quarto_render(
  "marthas-qart-report.qmd",
  execute_params = list(
    master_files_folder = master_files_folder,
    reporting_quarter = reporting_quarter_string,
    quarter_string = reporting_quarter_file_string
    ))

new_file_name <- glue::glue("Marthas_QART_{reporting_quarter_file_string}.html")

# move the quarto report into the output folder and change name
file.rename('marthas-qart-report.html', here(glue::glue('output/{new_file_name}')))

# save file to SharePoint
upload_SP_file(here("output", new_file_name),
               paste0(slides_folder, "/", new_file_name))

### PowerPoint with MatNeo data
quarto_render(
  "mat_neo_qart_slides.qmd",
  execute_params = list(
    master_files_folder = master_files_folder,
    reporting_quarter = reporting_quarter_string,
    quarter_string = reporting_quarter_file_string
    ))

new_file_name <- glue::glue("MatNeo_QART_slides_{reporting_quarter_file_string}.pptx")

# move the quarto report into the output folder and change name
file.rename('mat_neo_qart_slides.pptx', here(glue::glue('output/{new_file_name}')))

# save file to SharePoint
upload_SP_file(here("output", new_file_name),
               paste0(slides_folder, "/", new_file_name))
