### report 1 - html report with martha's data
quarto_render("marthas_qart_data_vis.qmd", 
              execute_params = list(reporting_quarter = reporting_quarter_string,
                                    master_files_folder = master_files_folder,
                                    # below parameter is generated in read_qart_submissions.R
                                    quarter_string = quarter_string
                                    )
              )

new_file_name <- glue::glue("Marthas_QART_{quarter_string}.html")

# move the quarto report into the output folder and change name
file.rename('marthas_qart_data_vis.html', here(glue::glue('output/{new_file_name}')))

# save file to SharePoint 
chosenlib$upload_file(
  dest = str_glue("{base_url}/{slides_folder}/{new_file_name}"),
  src = str_glue('output/{new_file_name}')
)

### report 2 - ppt slides showing share of stages across trusts by PSC

# download template file from SharePoint
master_files_dr <- chosenlib$get_item(glue::glue("{base_url}/{master_files_folder}"))

matneo_overview_file <- master_files_dr$get_item(str_glue('matneo_overview.PNG'))

matneo_overview_file$download(dest = here("data", 'matneo_overview.PNG'), 
                              overwrite = T)

# run quarto file and save output in output folder
quarto_render("mat_neo_qart_slides.qmd", 
              execute_params = 
                list(reporting_quarter = reporting_quarter_string,
                     master_files_folder = master_files_folder,
                     # below parameter is generated in read_qart_submissions.R
                     quarter_string = quarter_string
                )
)

new_file_name <- glue::glue("matneo_qart_slides_{quarter_string}.pptx")

# move the quarto report into the output folder and change name
file.rename('mat_neo_qart_slides.pptx', here(glue::glue('output/{new_file_name}')))

# save file to SharePoint 
chosenlib$upload_file(
  dest = str_glue("{base_url}/{slides_folder}/{new_file_name}"),
  src = str_glue('output/{new_file_name}')
)
