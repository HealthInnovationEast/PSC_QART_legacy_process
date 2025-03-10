# run quarto file and save output in output folder
library(here)
library(quarto)
library(glue)

# NICE TO HAVE: validation rule to make sure quarter param is valid?

quarto_render("qart_slides.qmd", 
              execute_params = list(reporting_quarter = reporting_quarter,
                                    master_files_folder = master_files_folder,
                                    quarter_string = quarter_string
                                    )
              )

file_name <- format(Sys.time(), "qart_test_%Y_%m_%d_%H%M%S.pptx")

# move the quarto report into the output folder and change name
# TO DO: save file to SharePoint quart_cleansed_date_2425_Q3
file.rename('qart_slides.pptx', here(glue::glue('output/{file_name}')))