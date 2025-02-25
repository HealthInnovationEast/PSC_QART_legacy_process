source("psc_name_update.R")

# organisations with identified discrepancies
discrepancies <- read.csv(here("lookups", "discrepancies_lookup.csv")) |>
  rename("organisation_as_recorded" = organisation)

org_discrepancies <- c(
  unique(discrepancies$organisation_as_recorded),
  unique(discrepancies$organisation_tidy)
) |>
  unique()

# select relevant variables from data set (i.e, org info and interventions)
previous_submisisons_data <- submissions_previous_psc_updated |>
  rename("organisation_as_recorded" = organisation) |>
  select(latest_psc_name:mews)

# create map of org discrepancies across quarters 
map_org_disc_across_quarters <- previous_submisisons_data |>
  select(latest_psc_name:quarter) |>
  filter(organisation_as_recorded %in% org_discrepancies) |>
  left_join(discrepancies, by = c(
    "latest_psc_name",
    "organisation_as_recorded"
  )) |>
  arrange(quarter) |>
  # determine organisational inaccuracies
  mutate(
    inactive_during_quarter = quarter > nhs_quarter,
    incorrectly_named = organisation_as_recorded != organisation_tidy
  )

# function skeleton
psc <- "(?i)Kent"

# filter discrepancies maps to specific PSC
map_psc_org_disc_across_quarters <- map_org_disc_across_quarters |>
  filter(
    str_detect(latest_psc_name, psc)
  )

# using psc map, pull polluted data points from previous_submisisons_data
psc_org_polluted_data <- map_psc_org_disc_across_quarters |>
  arrange(quarter) |>
  select(
    latest_psc_name, ics, organisation_as_recorded, organisation_tidy,
    quarter, api_date_end, nhs_quarter, inactive_during_quarter,
    incorrectly_named
  ) |>
  left_join(previous_submisisons_data,
    by = c("latest_psc_name", "ics", "organisation_as_recorded", "quarter")
  ) |>
  # DECISION RULE: the correct org name during a quarter is based on activity status
  # (preferring the name of an active org) and use of official legal name 
  # (preferring the name as shown in ODS)
  mutate(valid_org_name = case_when(inactive_during_quarter == T | 
                                      incorrectly_named == T ~ F,
                                    .default = T))

# quarters where there was data pollution
polluted_quarters <- unique(psc_org_polluted_data$quarter)

# empty objects to store loop results
polluted_data_long_all_quarters <- data.frame()
results_evaluated_all_quarters <- data.frame()

# go through every quarter of pulluted data
for (polluted_quarter in polluted_quarters) {
  # filter polluted data in the quarter
  psc_org_polluted_data_quarter <- psc_org_polluted_data |>
    filter(quarter == polluted_quarter)

  # organisations involved
  orgs <- unique(psc_org_polluted_data_quarter$organisation_as_recorded)

  # turn intervention data into long format, which turns involved orgs into columns
  polluted_data_long <- psc_org_polluted_data_quarter |>
    select(-c(organisation_tidy, api_date_end, inactive_during_quarter,
      incorrectly_named, nhs_quarter, valid_org_name
    )) %>%  # need %>% pipe to pass '.' argument in is.na()
    replace(is.na(.), 'Stageless') |>
    pivot_longer(
      cols = c(magnesium_sulphate:mews),
      names_to = "intervention"
    ) |>
    arrange(intervention)

  # turn involved orgs into wide format
  results_polluted_data_evaluated <- polluted_data_long |>
    pivot_wider(
      names_from = "organisation_as_recorded",
      values_from = "value"
    ) |>
    rowwise() |>
    mutate(
      # IMPORTANT: here we test whether the data for each intervention 
      # was the same across organisations
      all_equal = all(
        c_across(all_of(orgs)) == first(c_across(all_of(orgs)))
      ),
      # DECISION RULE: choose which stage value to keep for an intervenetion
      # favouring the least advanced stage (or stageless - NA value- when data was not provided)
      stage_to_keep = min(c_across(all_of(orgs)))
    )

  # append results
  polluted_data_long_all_quarters <- bind_rows(
    polluted_data_long_all_quarters,
    polluted_data_long
  )

  results_evaluated_all_quarters <- bind_rows(
    results_evaluated_all_quarters,
    results_polluted_data_evaluated
  )
}

# how bad the problem was across time
results_evaluated_all_quarters |>
  count(quarter, all_equal) |>
  arrange(quarter) |>
  filter(all_equal == F)

# make cleansed intervention data wide again 
intervention_order <- c('magnesium_sulphate', 
                        'corticosteroids', 'antibiotics',
                        'optimal_cord_management',
                        'normothermia',
                        'maternal_breast_milk',
                        'place_of_birth',
                        'caffeine',
                        'volume_targeted_ventilation',
                        'newtt2',
                        'mews')


psc_keep_interventions <- results_evaluated_all_quarters |>
  select(latest_psc_name, ics, quarter, intervention, stage_to_keep) |>
  pivot_wider(names_from = intervention,
              values_from = stage_to_keep) |>
  #reorder columns to emulate previous_submissions_data order
  select(latest_psc_name:quarter, all_of(intervention_order)) |>
  # restore NA values
  mutate(across(all_of(intervention_order), ~ case_when(.x == 'Stageless' ~ NA,
                                                        .default = .x)))

# create column with organisation to keep 
psc_keep_orgs <- psc_org_polluted_data |> 
  filter(valid_org_name == T) |>
  mutate(organisation_cleansed = organisation_as_recorded) |>
  select(latest_psc_name, quarter, organisation_cleansed)

# join two tables
psc_data_cleansed <- psc_keep_orgs |>
  left_join(psc_keep_interventions, by = c('latest_psc_name', 'quarter')) |>
  # order below reflects structure in previous_submisisons_data
  select(latest_psc_name, ics, organisation_cleansed, quarter,
         all_of(intervention_order))
