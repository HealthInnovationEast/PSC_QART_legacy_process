# organisations with identified discrepancies
discrepancies <- read.csv(here("lookups", "discrepancies_lookup.csv")) |>
  rename('organisation_as_recorded' = organisation)

org_discrepancies <- c(unique(discrepancies$organisation_as_recorded),
                       unique(discrepancies$organisation_tidy)) |>
  unique()

# select relevant variables frrom data set 
previous_submisisons_data <- submissions_previous_psc_updated |>
  rename('organisation_as_recorded' = organisation) |>
  select(1:15)

# table with discrepancies across quarters
org_disc_quarters <- previous_submisisons_data |>
  select(1:4) |>
  filter(organisation_as_recorded %in% org_discrepancies) |>
  left_join(discrepancies, by = c("latest_psc_name", 
                                  "organisation_as_recorded")) |>
  arrange(quarter) |>
  mutate(inactive_during_quarter = quarter > nhs_quarter,
         incorrectly_named = organisation_as_recorded != organisation_tidy)
 
# identify outdated orgs in psc
psc_org_disc_quarter <- org_disc_quarters |>
  filter(str_detect(latest_psc_name, '(?i)Kent') #an argument?
         ) 

psc_org_polluted <- psc_org_disc_quarter |> 
  arrange(quarter) |>
  select(latest_psc_name, ics, organisation_as_recorded, organisation_tidy, 
         quarter, api_date_end, nhs_quarter, inactive_during_quarter,
         incorrectly_named) |>
  left_join(previous_submisisons_data,
            by = c('latest_psc_name', 'ics', 'organisation_as_recorded', 'quarter'))

#check the records of this psc for a quarter
polluted_quarters <- unique(psc_org_polluted$quarter)

psc_org_polluted_quarter <- psc_org_polluted |>
  filter(quarter == '2021/22 Q1')
  
orgs <- unique(psc_org_polluted_quarter$organisation_as_recorded)

# Function to check equality of values in each row

psc_org_polluted_quarter |>
  pivot_longer(cols = c(10:19),
               names_to = 'intervention') |> 
  pivot_wider(names_from = 'organisation_as_recorded',
              values_from = 'value') |>
  mutate(equality = str_glue(orgs[1]) == str_glue(orgs[2]) == str_glue(orgs[3])) |>
  View()
