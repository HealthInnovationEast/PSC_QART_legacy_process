source("psc_name_update.R")

# organisations with identified discrepancies
discrepancies <- read.csv(here("lookups", "discrepancies_lookup.csv")) |>
  rename("organisation_as_recorded" = organisation)

org_discrepancies <- c(
  unique(discrepancies$organisation_as_recorded),
  unique(discrepancies$organisation_tidy)
) |>
  unique()

# select relevant variables from data set
previous_submisisons_data <- submissions_previous_psc_updated |>
  rename("organisation_as_recorded" = organisation) |>
  select(1:15)

# table with discrepancies across quarters
org_disc_quarters <- previous_submisisons_data |>
  select(1:4) |>
  filter(organisation_as_recorded %in% org_discrepancies) |>
  left_join(discrepancies, by = c(
    "latest_psc_name",
    "organisation_as_recorded"
  )) |>
  arrange(quarter) |>
  mutate(
    inactive_during_quarter = quarter > nhs_quarter,
    incorrectly_named = organisation_as_recorded != organisation_tidy
  )

# function skeleton
# identify quarters where there are discrepancies within a psc
quarters_disc_psc_org <- org_disc_quarters |>
  filter(
    str_detect(latest_psc_name, "(?i)Kent") # an argument?
  )

# pull those data points
psc_org_polluted <- quarters_disc_psc_org |>
  arrange(quarter) |>
  select(
    latest_psc_name, ics, organisation_as_recorded, organisation_tidy,
    quarter, api_date_end, nhs_quarter, inactive_during_quarter,
    incorrectly_named
  ) |>
  left_join(previous_submisisons_data,
    by = c("latest_psc_name", "ics", "organisation_as_recorded", "quarter")
  )

# check the records of this psc for a quarter
polluted_quarters <- unique(psc_org_polluted$quarter)

results_quarter_check_psc <- data.frame()

for(quarter in polluted_quarters){
  print(quarter)
}

psc_org_polluted_quarter <- psc_org_polluted |>
  filter(quarter == "2023/24 Q1")

orgs <- unique(psc_org_polluted_quarter$organisation_as_recorded)

# Function to check equality of values in each row

# turn interventions into long format
polluted_long <- psc_org_polluted_quarter |>
  select(-c(
    ics, organisation_tidy, api_date_end, inactive_during_quarter,
    incorrectly_named, nhs_quarter
  )) |>
  pivot_longer(
    cols = c(magnesium_sulphate:mews),
    names_to = "intervention"
  ) |>
  arrange(intervention)

# turn involved orgs into wide format
polluted_evaluated <- polluted_long |>
  pivot_wider(
    names_from = "organisation_as_recorded",
    values_from = "value"
  ) %>% # need this pipe to pass '.' argument in is.na()
  replace(is.na(.), 'no-data') |>
  rowwise() |>
   mutate(
     all_equal = all(
       c_across(all_of(orgs)) == first(c_across(all_of(orgs)))
   ))

results_quarter_check_psc <- polluted_evaluated |> 
  select(quarter, latest_psc_name, intervention, all_equal)
