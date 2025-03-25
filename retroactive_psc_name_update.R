# previously known as psc_name_update
master_files_dr <- chosenlib$get_item(glue::glue("{base_url}/1. Master files"))

# download hin names look up
hin_names_file <- master_files_dr$get_item('hin_names.csv')

hin_names_file$download(dest = here("data", "hin_names.csv"), 
                        overwrite = T)

# download raw data file from share point (data up to 2024/25 Q2)
raw_data_file <- master_files_dr$get_item('MatNeoSIP.xlsx')

raw_data_file$download(dest = here("data", "MatNeoSIP.xlsx"), 
                       overwrite = T)

# read latest psc (hin) names
hin_names <- read.csv(here("lookups", "hin_names.csv")) |>
  arrange(hin_folders)

# read previous submission data (i.e., up to 2024/25 Q2)
submissions_previous_data <- read_excel(here("data", "MatNeoSIP.xlsx"), sheet = "Data") |>
  clean_names() |>
  rename(
    "stage_7_all" = "stage_7",
    "quarter" = "date"
  ) |>
  rename_with(~ str_replace(., "x", "stage_"), starts_with("x"))

submissions_previous_psc_name <- submissions_previous_data |>
  mutate(psc = case_when(psc == "Health Innovation Network" ~ "South London HIN",
    psc == "Health Innovation Manchester" ~ "Manchester HIN",
    .default = paste0(psc)
  ))

# replace old psc names with their appropriate HIN name
psc_old_names <- submissions_previous_psc_name |> 
  distinct(psc) |>
  arrange(psc)

psc_name_look_up <- data.frame(hin_names, psc_old_names) |>
  rename('latest_psc_name' = hin_folders,
         'former_psc_name' = psc)

submissions_previous_psc_updated <- submissions_previous_psc_name |> 
  left_join(psc_name_look_up, by = c('psc' = 'former_psc_name')) |>
  relocate(latest_psc_name, 
           .after = psc) |> 
  select(-psc)

source('retroactive_fixes.R')