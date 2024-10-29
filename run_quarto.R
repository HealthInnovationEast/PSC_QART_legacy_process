#run quarto fie and save output in output folder
library(here)
library(quarto)
library(glue)

quarto_render("qart_slides.qmd")
file_name <- format(Sys.time(), "qart_test_%Y_%m_%d_%H%M%S.pptx")

#move the quarto report into the output folder and change name
file.rename('qart_slides.pptx', here(glue::glue('output/{file_name}')))