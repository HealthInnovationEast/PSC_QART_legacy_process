#run quarto fie and save output in output folder
library(here)
library(quarto)
library(glue)

#### MAKE A QUARTER PARAMETER

quarto_render("qart_slides.qmd", 
              execute_params = list(report_quarter = "2024/25 Q3")
              # NICE TO HAVE: validation rule to make sure quarter param is valid
              )

file_name <- format(Sys.time(), "qart_test_%Y_%m_%d_%H%M%S.pptx")

#move the quarto report into the output folder and change name
file.rename('qart_slides.pptx', here(glue::glue('output/{file_name}')))