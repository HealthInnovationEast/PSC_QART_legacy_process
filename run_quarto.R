# run quarto file and save output in output folder

# NICE TO HAVE: validation rule to make sure quarter param is valid?

quarto_render("qart_slides.qmd", 
              execute_params = list(reporting_quarter = reporting_quarter,
                                    master_files_folder = master_files_folder,
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