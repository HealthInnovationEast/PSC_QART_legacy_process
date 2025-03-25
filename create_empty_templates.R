# script to create empty excel templates with organisation names for each PSC. Files will be avialbe on sharepoint
location_lookup <- read.csv(here("lookups", "psc_lookup.csv"))
pscs <- unique(location_lookup$latest_psc_name)

# download template file from SharePoint
master_files_dr <- chosenlib$get_item(glue::glue("{base_url}/{master_files_folder}"))
template_file <- master_files_dr$get_item(str_glue({template_file_name}))
template_file$download(dest = here("lookups", str_glue({template_file_name})), 
                       overwrite = T)

# file friendly naming string for saving results
current_quarter_year <- str_remove_all(reporting_quarter, '20|/')
  
# loop through locations provided in psc_lookup.csv
for (psc in pscs) {
  # grid with icb codes and names
  location_icb <- location_lookup |>
    filter(latest_psc_name == psc) |>
    distinct(api_icb_code, api_icb_name)

  # grid with icb code, icb name, trust code, and trust name
  location_icb_trust <- location_lookup |>
    filter(latest_psc_name == psc) |>
    # choosing this order to accommodate most aesthetic width of cells in template
    select(
      api_icb_code, api_current_code_quarter,
      api_icb_name, api_current_org_name_quarter
    )

  # grid with trust code and trust name
  location_trusts <- location_lookup |>
    filter(latest_psc_name == psc) |>
    # choosing this order to accommodate most aesthetic width of cells in template
    select(api_current_code_quarter, api_current_org_name_quarter)

  # load template excel file
  wb <- openxlsx2::wb_load(here("lookups", str_glue({template_file_name})))

  # order of replacement is gotta be icb code, org code, then names
  # TO DO: replace "this quarter" with "Data for 2024/25 Q1" (example)

  # add ICB names to Medicines tab
  wb <- wb_add_data(wb,
    sheet = "Medicines",
    x = location_icb,
    start_col = 2,
    start_row = 6,
    col_names = FALSE
  )

  wb <- wb_add_data(wb,
    sheet = "Medicines",
    x = location_icb,
    start_col = 2,
    start_row = 17,
    col_names = FALSE
  )

  # add ICB and Trust names to MatNeo tab

  # optimisation grid
  wb <- wb_add_data(wb,
    sheet = "MatNeo",
    x = location_icb_trust,
    start_col = 2,
    start_row = 9,
    col_names = FALSE
  )

  # deterioration tools grid
  wb <- wb_add_data(wb,
    sheet = "MatNeo",
    x = location_icb_trust,
    start_col = 2,
    start_row = 30,
    col_names = FALSE
  )

  # preterm birth lead engagement
  wb <- wb_add_data(wb,
    sheet = "MatNeo",
    x = location_trusts,
    start_col = 3,
    start_row = 62,
    col_names = FALSE
  )

  # PAS score
  wb <- wb_add_data(wb,
    sheet = "MatNeo",
    x = location_icb,
    start_col = 3,
    start_row = 82,
    col_names = FALSE
  )

  wb_save(wb,
    file = "output/empty_template.xlsx"
  )

  # upload
  print(str_glue("Uploading template to:
                 {base_url}/{psc}"))
  
  chosenlib$upload_file(
    dest = str_glue("{base_url}/{psc}/{psc} QART {current_quarter_year}.xlsx"),
    src = "output/empty_template.xlsx"
  )

  # delete template file from local location.
  file.remove("output/empty_template.xlsx")
}
