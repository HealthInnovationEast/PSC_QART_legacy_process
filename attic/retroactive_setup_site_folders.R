# This file will be run once once settled on location
# upload a readme file to a folder with the name of each psc.
# To create a folder, a file must be uploaded to the folder- which creates the folder.
source('config-sharepoint-location.R')

# read latest psc (hin) names
hin_names <- read.csv(here("lookups", "hin_names.csv")) |>
  arrange(hin_folders) |>
  unlist() |>
  unname()

for (hin in hin_names) {
 chosenlib$upload_file(dest = str_glue("{base_url}/{psc_name}/readme.txt"), 
                       src = "lookups/readme.txt"
                       )
 
}