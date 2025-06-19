message('Creating templates...')

# script to create empty excel templates with organisation names for each PSC. 

# download template file from SharePoint
template_file <- master_files_dr$get_item(str_glue({template_file_name}))
template_file$download(dest = here("lookups", str_glue({template_file_name})), 
                       overwrite = T)

# file friendly naming string for saving results
current_quarter_year <- str_remove_all(reporting_quarter_string, '20|/')

# for testing one template
# pscs = pscs[pscs %in% c('Kent Surrey Sussex HIN')]

# loop through locations provided in psc_lookup.csv
for (psc in pscs) {
  # grid with trust code, site code, trust name, site name
  location_trust_sites <- psc_icb_trust_site_lookup |>
    filter(updated_psc_name == psc) |>
    select(api_current_code, api_current_org_name,
           trust_site_code, api_site_name) |>
    # this is to avoid printing string '#N/A' for empty cells
    mutate(across(where(is.character), ~replace_na(., ' ')))
  
  # grid with icb codes and names
  location_icb <- psc_icb_trust_site_lookup |>
    filter(updated_psc_name == psc) |>
    distinct(api_icb_code, api_icb_name)

  # grid with icb code, trust code, icb name, and trust name
  location_icb_trust <- psc_icb_trust_site_lookup |>
    filter(updated_psc_name == psc) |>
    # choosing this order to accommodate most aesthetic width of cells in template
    distinct(
      api_icb_code, api_current_code,
      api_icb_name, api_current_org_name
    )

  # grid with trust code and trust name
  location_trusts <- psc_icb_trust_site_lookup |>
    filter(updated_psc_name == psc) |>
    # choosing this order to accommodate most aesthetic width of cells in template
    distinct(api_current_code, api_current_org_name)

  # load template excel file
  wb <- openxlsx2::wb_load(here("lookups", str_glue({template_file_name})))
  
  # add trust and sites to Martha's Rule tab
  wb <- wb_add_data(wb,
    sheet = "Martha's Rule",
    x = location_trust_sites,
    start_col = 2,
    start_row = 8,
    col_names = FALSE
  )
  
  # add ICB names to Medicines tab
  wb <- wb_add_data(wb,
    sheet = "Medicines",
    x = location_icb,
    start_col = 2,
    start_row = 6,
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
  
  ## CULTURE PROGRAMME
  
  # moments numbers
  wb <- wb_add_data(wb,
    sheet = "MOMENTS Numbers",
    x = location_trusts,
    start_col = 2,
    start_row = 6,
    col_names = FALSE
  )
  
  # plt eng't with psc
  wb <- wb_add_data(wb,
    sheet = "PLT Engagement with PSC",
    x = location_trusts,
    start_col = 5,
    start_row = 5,
    col_names = FALSE
  )
  
  # plt eng't with cc
  wb <- wb_add_data(wb,
    sheet = "PLT Engagement with CCs",
    x = location_trusts,
    start_col = 5,
    start_row = 5,
    col_names = FALSE
  )
  
  # cc eng't with PSC
  wb <- wb_add_data(wb,
                    sheet = "CC Engagement with PSC",
                    x = location_trusts,
                    start_col = 5,
                    start_row = 5,
                    col_names = FALSE
  )
  
  # cc numbers
  wb <- wb_add_data(wb,
                    sheet = "Culture Coach Numbers",
                    x = location_trusts,
                    start_col = 2,
                    start_row = 6,
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
