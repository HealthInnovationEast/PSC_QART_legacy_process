source("psc_name_update.R")

# select relevant variables from data set (i.e, org info and interventions)
previous_submisisons_data <- submissions_previous_psc_updated |>
  rename("organisation_as_recorded" = organisation) |>
  select(latest_psc_name:mews)

# speficied order of inerventions
intervention_order <- c(
  "magnesium_sulphate",
  "corticosteroids", "antibiotics",
  "optimal_cord_management",
  "normothermia",
  "maternal_breast_milk",
  "place_of_birth",
  "caffeine",
  "volume_targeted_ventilation",
  "newtt2",
  "mews"
)

# identified discrepancies (look up generated in active_organisation_check.R)
discrepancies <- read.csv(here("lookups", "discrepancies_lookup.csv")) |>
  rename("organisation_as_recorded" = organisation)

org_discrepancies <- c(
  unique(discrepancies$organisation_as_recorded),
  unique(discrepancies$organisation_tidy),
  unique(discrepancies$api_current_org_name_quarter)
) |>
  unique()

# we will be fixing 2 observed DQ issues:
# simple organisation name mistakes AND carry over reporting of legacy organisations
psc_legacy_carry_overs <- discrepancies |>
  distinct(latest_psc_name, api_date_end) |>
  filter(!is.na(api_date_end)) |>
  distinct(latest_psc_name) |>
  as.vector() |>
  unlist()

psc_name_mistakes <- discrepancies |>
  filter(!latest_psc_name %in% psc_legacy_carry_overs) |>
  distinct(latest_psc_name) |>
  as.vector() |>
  unlist()

# 1) name mistakes ----------------------------------------

previous_data_name_mistakes <- previous_submisisons_data |>
  filter(latest_psc_name %in% psc_name_mistakes &
    organisation_as_recorded %in% org_discrepancies)

name_corrections <- previous_data_name_mistakes |>
  distinct(latest_psc_name, organisation_as_recorded) |>
  left_join(discrepancies, by = c(
    "latest_psc_name",
    "organisation_as_recorded"
  )) |>
  select(
    latest_psc_name,
    organisation_as_recorded,
    organisation_tidy,
    api_current_org_name_quarter,
  ) |>
  mutate(organisation_as_cleansed = case_when(
    organisation_as_recorded != organisation_tidy &
      organisation_tidy == api_current_org_name_quarter ~
      organisation_tidy,
    organisation_as_recorded != organisation_tidy &
      organisation_tidy != api_current_org_name_quarter ~
      api_current_org_name_quarter,
    organisation_as_recorded == organisation_tidy &
      organisation_tidy != api_current_org_name_quarter ~
      api_current_org_name_quarter
  ))

# apply name corrections
previous_data_org_name_corrected <- name_corrections |>
  select(latest_psc_name, organisation_as_recorded, organisation_as_cleansed) |>
  left_join(previous_data_name_mistakes, by = c(
    "latest_psc_name",
    "organisation_as_recorded"
  )) |>
  # create flags recording correction applied
  mutate(
    retroactive_fix = T,
    retroactive_correction = "trust-name-change"
  ) |>
  unique() # accounts RXW Shrewsbury duplications

# 2) legacy carry overs ----------------------------------------
# this problem is a bit complex, we basically have to work out:
# the quarters where data was reported simultaneously for a legacy organisation AND a current organisation
# whether data for the interventions was identical across legacy and current orgs
# where there are discrepancies, then choose which stage to keep

previous_data_legacy_carry_over <- previous_submisisons_data |>
  filter(latest_psc_name %in% discrepancies$latest_psc_name &
    !latest_psc_name %in% psc_name_mistakes &
    organisation_as_recorded %in% org_discrepancies)

# create map of org discrepancies across quarters
map_org_disc_across_quarters <- previous_data_legacy_carry_over |>
  select(latest_psc_name:quarter) |>
  # get legacy information from lookup
  left_join(discrepancies, by = c(
    "latest_psc_name",
    "organisation_as_recorded"
  )) |>
  arrange(quarter) |>
  # determine organisational inaccuracies
  mutate(
    inactive_during_quarter = case_when(quarter > nhs_quarter ~ T,
      .default = F
    ),
    incorrectly_named = organisation_as_recorded != organisation_tidy
  )

# function skeleton
my_function <- function(psc, orgs) {
  # filter discrepancies maps to specific PSC
  map_psc_org_disc_across_quarters <- map_org_disc_across_quarters |>
    filter(
      str_detect(latest_psc_name, psc),
      str_detect(organisation_as_recorded, orgs)
    )

  # quarters where there was data pollution
  potential_quarters <- unique(map_psc_org_disc_across_quarters$quarter)
  legacy_quarter <- unique(map_psc_org_disc_across_quarters$nhs_quarter[
    !is.na(map_psc_org_disc_across_quarters$nhs_quarter)
  ])

  # DECISION RULE: quarters are polluted a quarter after an organisation became legacy onwards
  polluted_quarters <- potential_quarters[which(potential_quarters > legacy_quarter)]
  message("Identified polluted quarters")
  print(glue::glue("{polluted_quarters}"))

  # using discrepancy map, pull polluted data points from previous_data_legacy_carry_over
  psc_org_polluted_data <- map_psc_org_disc_across_quarters |>
    arrange(quarter) |>
    filter(quarter %in% polluted_quarters) |>
    select(
      latest_psc_name, ics, organisation_as_recorded, organisation_tidy,
      quarter, api_date_end, nhs_quarter, inactive_during_quarter,
      incorrectly_named
    ) |>
    left_join(previous_data_legacy_carry_over,
      by = c("latest_psc_name", "ics", "organisation_as_recorded", "quarter")
    ) |>
    # DECISION RULE: the correct org name during a quarter is based on activity status
    # (preferring the name of an active org) and use of official legal name
    # (preferring the name as shown in ODS)
    mutate(valid_org_name = case_when(
      inactive_during_quarter == T |
        incorrectly_named == T ~ F,
      .default = T
    ))

  # empty objects to store loop results
  polluted_data_long_all_quarters <- data.frame()
  results_evaluated_all_quarters <- data.frame()

  # go through every quarter of polluted data
  for (polluted_quarter in polluted_quarters) {
    # filter polluted data in the quarter
    psc_org_polluted_data_quarter <- psc_org_polluted_data |>
      filter(quarter == polluted_quarter)

    # organisations involved
    orgs <- unique(psc_org_polluted_data_quarter$organisation_as_recorded)

    # turn intervention data into long format, which turns involved orgs into columns
    polluted_data_long <- psc_org_polluted_data_quarter |>
      select(-c(
        organisation_tidy, api_date_end, inactive_during_quarter,
        incorrectly_named, nhs_quarter, valid_org_name
      )) %>% # need %>% pipe to pass '.' argument in is.na()
      replace(is.na(.), "Stageless") |>
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
  message("Number of interventions with a mismatch per quarter")

  results_table <- results_evaluated_all_quarters |>
    count(quarter, all_equal) |>
    arrange(quarter) |>
    filter(all_equal == F)

  print(results_table)

  # make selected intervention data wide again
  psc_keep_interventions <- results_evaluated_all_quarters |>
    select(latest_psc_name, ics, quarter, intervention, stage_to_keep) |>
    pivot_wider(
      names_from = intervention,
      values_from = stage_to_keep
    ) |>
    # reorder columns to emulate previous_submissions_data order
    select(latest_psc_name:quarter, all_of(intervention_order)) |>
    # restore NA values
    mutate(across(all_of(intervention_order), ~ case_when(.x == "Stageless" ~ NA,
      .default = .x
    )))

  # create column with organisation to keep
  psc_keep_orgs <- psc_org_polluted_data |>
    filter(valid_org_name == T | inactive_during_quarter == F) |>
    mutate(organisation_as_cleansed = case_when(
      valid_org_name == T ~ organisation_as_recorded,
      inactive_during_quarter == F & incorrectly_named == T ~ organisation_tidy
    )) |>
    distinct(latest_psc_name, quarter, organisation_as_cleansed)

  # join the two tables
  psc_data_cleansed <- psc_keep_orgs |>
    left_join(psc_keep_interventions, by = c("latest_psc_name", "quarter")) |>
    # order below reflects structure in previous_submisisons_data
    select(
      latest_psc_name, ics, organisation_as_cleansed, quarter,
      all_of(intervention_order)
    )

  return(list(
    "polluted_data_long_all_quarters" = polluted_data_long_all_quarters,
    "results_evaluated_all_quarters" = results_evaluated_all_quarters,
    "psc_data_cleansed" = psc_data_cleansed
  ))
}

# deploy function
# -- kent --
kent_results <- my_function(
  psc = "(?i)kent",
  # orgs = ''
  orgs = "(?i)brighton|western sussex|university hospitals sussex"
)

kent_polluted_data <- kent_results$polluted_data_long_all_quarters
kent_eval_for_qa <- kent_results$results_evaluated_all_quarters
kent_cleansed <- kent_results$psc_data_cleansed

# -- north west coast --
nwc_results <- my_function(
  psc = "(?i)north West Coast",
  orgs = "(?i)southport|st helens|mersey"
)

nwc_polluted_data <- nwc_results$polluted_data_long_all_quarters
nwc_eval_for_qa <- nwc_results$results_evaluated_all_quarters
nwc_cleansed <- nwc_results$psc_data_cleansed

# -- south west 1 --
sw_results_somerset <- my_function(
  psc = "(?i)south West",
  orgs = "(?i)yeovil|somerset"
)

sw_somerset_polluted_data <- sw_results_somerset$polluted_data_long_all_quarters
sw_somerset_eval_for_qa <- sw_results_somerset$results_evaluated_all_quarters
sw_somerset_cleansed <- sw_results_somerset$psc_data_cleansed

# -- south west 2 --
sw_results_royal_devon <- my_function(
  psc = "(?i)south West",
  orgs = "(?i)northern devon|royal devon"
)

sw_royal_devon_polluted_data <- sw_results_royal_devon$polluted_data_long_all_quarters
sw_royal_devon_eval_for_qa <- sw_results_royal_devon$results_evaluated_all_quarters
sw_royal_devon_cleansed <- sw_results_royal_devon$psc_data_cleansed

# bind results
previous_data_legacy_carry_over_corrected <- bind_rows(
  kent_cleansed,
  nwc_cleansed,
  sw_somerset_cleansed,
  sw_royal_devon_cleansed
) |>
  mutate(
    retroactive_fix = T,
    retroactive_correction = "legacy-trust-carry-over"
  )

# bring all corrections together ----------------------------------------
rows_to_replace <- previous_data_name_mistakes |>
  distinct(latest_psc_name, ics, organisation_as_recorded, quarter) |> 
  bind_rows(
    kent_polluted_data,
    nwc_polluted_data,
    sw_somerset_polluted_data,
    sw_royal_devon_polluted_data)|> 
  distinct(latest_psc_name, ics, organisation_as_recorded, quarter)

# below nrow will be less than previous_submisisons_data because of shrewsbury dups
previous_submissions_untouched  previous_submisisons_data |> 
  anti_join(rows_to_replace) 

#bind rows of cleansed data 
