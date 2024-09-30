library(tidyverse)
library(readxl)

files<-list.files("completed tables")

for(i in files){
  
optimisation=read_excel(str_c("completed tables/",i),"MatNeo optimisation")

}
