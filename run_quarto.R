# download template file from SharePoint
master_files_dr <- chosenlib$get_item(glue::glue("{base_url}/{master_files_folder}"))

matneo_overview_file <- master_files_dr$get_item(str_glue('matneo_overview.PNG'))

matneo_overview_file$download(dest = here("data", 'matneo_overview.PNG'), 
                       overwrite = T)

### presentation 1 - share of stages across trusts by PSC

# run quarto file and save output in output folder
quarto_render("qart_slides.qmd", 
              execute_params = list(reporting_quarter = reporting_quarter_string,
                                    master_files_folder = master_files_folder,
                                    # below parameter is generated in read_qart_submissions.R
                                    quarter_string = quarter_string
                                    )
              )

file_name <- glue::glue("matneo_qart_slides_{quarter_string}.pptx")

# move the quarto report into the output folder and change name
file.rename('qart_slides.pptx', here(glue::glue('output/{file_name}')))

# save file to SharePoint 
chosenlib$upload_file(
  dest = str_glue("{base_url}/{slides_folder}/{file_name}"),
  src = str_glue('output/{file_name}')
)


### presentation 2 - progression journeys

hin_name = "North West Coast"

# run quarto file and save output in output folder
quarto_render("progression_slides.qmd", 
              execute_params = list(reporting_quarter = reporting_quarter_string,
                                    master_files_folder = master_files_folder,
                                    # below parameter is generated in read_qart_submissions.R
                                    quarter_string = quarter_string,
                                    hin_name = hin_name
              )
)

file_name <- glue::glue("matneo_qart_{hin_name}_progression_slides_{quarter_string}.pptx")

# move the quarto report into the output folder and change name
file.rename('progression_slides.pptx', here(glue::glue('output/{file_name}')))

# save file to SharePoint 
chosenlib$upload_file(
  dest = str_glue("{base_url}/{slides_folder}/{file_name}"),
  src = str_glue('output/{file_name}')
)
