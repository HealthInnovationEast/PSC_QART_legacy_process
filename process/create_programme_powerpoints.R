# Create or empty folder for powerpoints data
input_folder <- "data/powerpoints/"
if (dir.exists(input_folder)) {
  unlink(list.files(input_folder, full.names = TRUE), recursive = TRUE)
} else {
  dir.create(input_folder, recursive = TRUE)
}

# Create or empty folder for powerpoints outputs
output_folder <- "output/powerpoints/"
if (dir.exists(output_folder)) {
  unlink(list.files(output_folder, full.names = TRUE), recursive = TRUE)
} else {
  dir.create(output_folder, recursive = TRUE)
}

# Get PowerPoints for HINs from SharePoint
hin_folders <- list_SP_files(base_url) |>
  select(name) |>
  filter(!str_detect(name, "\\.")) |>
  unlist() |>
  unname()

for (hin in hin_folders) {
  hin_files <- list_SP_files(paste0(base_url, "/", hin))

  hin_pptx_submission <- hin_files |>
    select(name) |>
    filter(str_detect(name, "\\.pptx"))

  if (nrow(hin_pptx_submission) == 0) {
    print(paste0("No PPTX submission for ", hin))
  } else if (nrow(hin_pptx_submission) > 1){
    print(paste0("Multiple PPTX submission for ", hin))
  } else {
    get_SP_file(paste0(hin, "/", hin_pptx_submission),
                here(input_folder, hin_pptx_submission))
  }
}

# Run PowerPoint creation python script
system("python powerpoint_creation.py")

# Upload results to SharePoint
for (p in list.files(output_folder)){
  new_file_name <- paste0(sub("\\.pptx", "", p), "_programme_",
                          reporting_quarter_file_string,
                          ".pptx")
  
  upload_SP_file(here(output_folder, p),
               paste0(slides_folder, "/", new_file_name))
}
