message('Creating templates...')

# script to create empty excel templates with organisation names for each PSC. 

# download template file from SharePoint
get_SP_file(paste0(master_files_folder, "/", template_file_name),
            here("lookups", template_file_name))

# load lookups
psc_trust_site_lookup <- read.csv(here("lookups", "psc_trust_site_lookup.csv"))
psc_icb_trust_lookup <- read.csv(here("lookups", "psc_icb_trust_lookup.csv"))
ICBs_by_HIN <- read.csv(here("lookups", ICBs_by_HIN_file_name),
                        check.names = FALSE)

# file friendly naming string for saving results
current_quarter_year <- str_remove_all(reporting_quarter_string, '20|/')

# if testing one template
# pscs = pscs[pscs %in% c('North West Coast HIN')]

# loop through locations provided in psc_lookup.csv
for (psc in pscs) {
  # grid with trust code, site code, trust name, site name 
  # generated in trust-site-codes.R
  location_trust_sites <- psc_trust_site_lookup |>
    filter(updated_psc_name == psc) |>
    select(api_org_code, api_org_name,
           site_sdcs_code, api_site_name, phase) |>
    # this is to avoid printing string '#N/A' for empty cells
    mutate(across(where(is.character), ~replace_na(., ' ')))

  # all other grids generated
  # grid with icb codes and names
  location_icb <- ICBs_by_HIN |>
    filter(`HIN Name` == psc) |>
    distinct(`ICB Code`, `ICB Name`)

  # wider form of location_icb_trust for medsip
  location_icb_wide <- location_icb |>
    pivot_wider(names_from = `ICB Code`,
                values_from = `ICB Name`)

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

  # add trust and sites to Martha's Rule tabs
  # MR adult and paeds
  wb <- wb_add_data(wb,
                    sheet = "MR - Adult & Paeds",
                    x = location_trust_sites |> 
                      filter(phase %in% c('1', '2')),
                    start_col = 2,
                    start_row = 7,
                    col_names = FALSE
  )

  # MR matneo
  wb <- wb_add_data(wb,
                    sheet = "MR - Maternity & Neonatal",
                    x = location_trust_sites |>
                      filter(phase == 'MatNeo') |> 
                      select(-phase),
                    start_col = 2,
                    start_row = 7,
                    col_names = FALSE
  )

  # MR ED
  wb <- wb_add_data(wb,
                    sheet = "MR - Emergency Departments",
                    x = location_trust_sites |>
                      filter(phase == 'ED') |> 
                      select(-phase),
                    start_col = 2,
                    start_row = 7,
                    col_names = FALSE
  )

  # add ICB names to Medicines tab
  # long form
  wb <- wb_add_data(wb,
    sheet = "Medicines",
    x = location_icb,
    start_col = 2,
    start_row = 6,
    col_names = FALSE
  )

  # wide form
  wb <- wb_add_data(wb,
    sheet = "Medicines",
    x = location_icb_wide,
    start_col = 4,
    start_row = 15,
    col_names = TRUE
  )

  # add ICB and Trust names to MatNeo tab

  # deterioration tools grid
  wb <- wb_add_data(wb,
    sheet = "MatNeo",
    x = location_icb_trust,
    start_col = 2,
    start_row = 8,
    col_names = FALSE
  )

  # abc grid
  wb <- wb_add_data(wb,
    sheet = "MatNeo",
    x = location_icb_trust,
    start_col = 2,
    start_row = 42,
    col_names = FALSE
  )

  # PAS score
  wb <- wb_add_data(wb,
    sheet = "MatNeo",
    x = location_icb,
    start_col = 3,
    start_row = 63,
    col_names = FALSE
  )

  wb_save(wb,
    file = "output/empty_template.xlsx"
  )

  # upload
  print(str_glue("Uploading template to:
                 {base_url}{psc}"))

  upload_SP_file(here("output/empty_template.xlsx"),
                 paste0(psc, "/", psc, " QART ", current_quarter_year, ".xlsx"))

  # delete template file from local location.
  file.remove("output/empty_template.xlsx")
}
