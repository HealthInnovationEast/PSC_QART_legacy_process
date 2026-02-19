message('Creating templates...')

# script to create empty excel templates with organisation names for each PSC. 

# download template file from SharePoint
template_file <- master_files_dr$get_item(str_glue({template_file_name}))
template_file$download(dest = here("lookups", str_glue({template_file_name})), 
                       overwrite = T)

# file friendly naming string for saving results
current_quarter_year <- str_remove_all(reporting_quarter_string, '20|/')

# for testing one template
#pscs = pscs[pscs %in% c('North West Coast HIN')]

# loop through locations provided in psc_lookup.csv
for (psc in pscs) {
  # grid with trust code, site code, trust name, site name 
  # generated in trust_site_codes.R
  location_trust_sites <- psc_trust_site_lookup |>
    filter(updated_psc_name == psc) |>
    select(api_parent_code, api_current_org_name,
           trust_site_code, api_site_name, phase) |>
    # this is to avoid printing string '#N/A' for empty cells
    mutate(across(where(is.character), ~replace_na(., ' ')))
  
  sites_to_grey <- psc_trust_site_lookup |>
    filter(updated_psc_name == psc) %>% select(to_grey_out) %>% 
    pull(to_grey_out)
  rows_to_grey_out <- which(str_detect(sites_to_grey,"Yes"))
  
  
  # all other grids generated 
  # grid with icb codes and names
  location_icb <- psc_icb_trust_lookup |>
    filter(updated_psc_name == psc) |>
    distinct(api_current_icb_code, api_current_icb_name)

  # grid with icb code, trust code, icb name, and trust name
  location_icb_trust <- psc_icb_trust_lookup |>
    filter(updated_psc_name == psc) |>
    # choosing this order to accommodate most aesthetic width of cells in template
    distinct(
      api_current_icb_code, api_current_code,
      api_current_icb_name, api_current_org_name
    )

  # grid with trust code and trust name
  location_trusts <- psc_icb_trust_lookup |>
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
  
  
  #greying out step
  for (x in rows_to_grey_out ){
  wb <- wb_add_fill(wb,
              sheet = "Martha's Rule",
               dims =  wb_dims(rows= 8 + x - 1,
                               cols= 2:6),
               color = wb_color("grey")
                )
  }
  
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
  
  # plt engagement with psc
  wb <- wb_add_data(wb,
    sheet = "PLT Engagement with PSC",
    x = location_trusts,
    start_col = 5,
    start_row = 5,
    col_names = FALSE
  )
  
  # culture coach numbers
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
