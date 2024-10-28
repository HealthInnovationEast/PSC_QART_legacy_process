#run quarto fie and save output in output folder
library(here)
library(quarto)

quarto_render("qart_slides.qmd")

#move the quarto report into the output folder and change name
file.rename('qart_slides.pptx', here('output/test.pptx'))