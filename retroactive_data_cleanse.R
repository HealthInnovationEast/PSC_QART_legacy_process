# this script is sourced from retroactive_org_changes.R
# select relevant variables from data set with polluted data
# i.e., data where there's legacy carry overs and outdated org names 
previous_submissions_data_polluted <- previous_submissions_psc_name_updated |>
  rename("organisation_as_recorded" = organisation) |>
  select(updated_psc_name:mews)

# specified order of matneo interventions
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

# identified discrepancies (look up generated in retroactive_org_changes.R)
discrepancies <- read.csv(here("lookups", "discrepancies_lookup.csv")) |>
  rename("organisation_as_recorded" = organisation)

# 1) name mistakes ----------------------------------------
# the problem we address here is the use of org names in the data 
# that don't match names as shown in API (i.e., the legal org name)

discrepancies_name_mistakes <- discrepancies |>
  filter(discrepancy_type == 'trust-name-mistake')

# set out what the incorrect names are an what their replacement will be
name_corrections <- discrepancies_name_mistakes |>
  select(updated_psc_name, organisation_as_recorded, organisation_tidy,
         api_current_code, api_current_org_name, discrepancy_type) |>
  # IMPORTANT although we made corrections in organisation_tidy before putting the org list through the API,
  # the api calls highlighted further corrections, so below we decide the correct name 
  mutate(organisation_name_after_cleanse = case_when(
    # when the name as recorded originally has **already** been corrected in organisation_tidy, 
    # and that tidy name matches the API name, then use the API name
    organisation_as_recorded != organisation_tidy &
      organisation_tidy == api_current_org_name ~
      api_current_org_name,
    # when the recorded name **was** corrected in tidy, but the tidy name **does not** match the API name
    # then use the name retrieved via API
    organisation_as_recorded != organisation_tidy &
      organisation_tidy != api_current_org_name ~
      api_current_org_name,
    # when the recorded name was kept as it was (therefore same as the name in tidy),
    # but that name **was still** an outdated name, use the API name 
    organisation_as_recorded == organisation_tidy &
      organisation_tidy != api_current_org_name ~
      api_current_org_name
  ))

# apply name corrections
previous_data_org_name_corrected <- previous_submissions_data_polluted |> 
  right_join(name_corrections |>
               select(-c(organisation_tidy, api_current_org_name)),
             by = c(
               "updated_psc_name",
                "organisation_as_recorded")
              ) |>
  relocate(api_current_code, 
           organisation_name_after_cleanse, 
           .after =  "organisation_as_recorded") |>
  # create flags recording correction applied
  mutate(
    retroactive_fix = T,
    .before = 'discrepancy_type'
  ) |>
  # removes  RXW Shrewsbury duplicated data 
  unique()  

# 2) legacy carry overs ----------------------------------------
# this problem is a bit complex, we basically have to work out:
# the quarters where data was reported simultaneously for a legacy organisation AND its successor 
# whether data for each intervention was identical across legacy and successor 
# where there are discrepancies, then choose which stage to keep
discrepancies_carry_overs <- discrepancies |> 
  filter(discrepancy_type == 'legacy-trust-carry-over')

# filter polluted data
previous_data_legacy_carry_over <- previous_submissions_data_polluted |>
  filter(organisation_as_recorded %in% discrepancies_carry_overs$organisation_as_recorded |
           organisation_as_recorded %in% discrepancies_carry_overs$organisation_tidy |
           organisation_as_recorded %in% discrepancies_carry_overs$api_current_org_name
         )

# create map of potential discrepancies across quarters
# NOTE: the API calls highlighted the discrepancies based on legacy status, 
# but we have to work out **when** the discrepancy applies (i.e., the polluted quarters) 

map_potential_carry_over_across_quarters <- discrepancies_carry_overs |>
  # add discrepancy information to quarterly data
  right_join(previous_data_legacy_carry_over |> select(updated_psc_name:quarter),
    by = c(
      "updated_psc_name",
      "organisation_as_recorded"
    )
  ) |>
  select(
    # this order will make reading the table easier
    updated_psc_name, ics, organisation_as_recorded,
    quarter, 
    # these 3 below correspond to the API info of the predecessor orgs
    api_org_code, api_date_end, nhs_quarter, 
    # these 2 correspond to the successor info
    api_current_code, api_current_org_name
  ) |>
  arrange(quarter, updated_psc_name) |>
  # determine organisational inaccuracies
  mutate(
    # was the organisation legacy during a quarter 
    inactive_during_quarter = case_when(quarter > nhs_quarter ~ T,
      .default = F
    ),
    name_mismatch = organisation_as_recorded != api_current_org_name
  )

# the function below will be filtering data based on a combination of PSC and trust names
# then it will identify which quarters of data were polluted 
# it will return a cut of the previous submissions filtered by polluted quarter

identify_polluted_quarters <- function(map = map_potential_carry_over_across_quarters, 
                                       previous_data = previous_data_legacy_carry_over, 
                                       psc, 
                                       orgs) {
  
  # map = defaulted to map_potential_carry_over_across_quarters - this is a look up of psc, org, and quarter with potential legacy carry overs
  # previous_data = defaulted to previous_data_legacy_carry_over - this the data to be examined
  # psc = patient safety collaborative filter 
  # orgs = trust(s) filter
  
  # filter discrepancies map to specific combination of PSC and orgs 
  map_psc_org_potential_carry_over <- map |>
    filter(
      str_detect(updated_psc_name, psc),
      str_detect(organisation_as_recorded, orgs)
    )

  # determine quarters where there was data pollution (not all quarters might have had pollution) 
  # e.g, the org name in the template was still accurate because the org change hadn't happened
  potential_quarters <- unique(map_psc_org_potential_carry_over$quarter)
  legacy_quarter <- unique(map_psc_org_potential_carry_over$nhs_quarter[
    !is.na(map_psc_org_potential_carry_over$nhs_quarter)
  ])

  # DECISION RULE: quarters are polluted starting a quarter after an organisation became legacy 
  polluted_quarters <- potential_quarters[which(potential_quarters > legacy_quarter)]
  message("Identified polluted quarters")
  print(glue::glue("{polluted_quarters}"))

  # extract relevant data points from previous_data_legacy_carry_over
  psc_org_polluted_data <- map_psc_org_potential_carry_over |>
    arrange(quarter) |>
    # pull polluted quarters from potential carry over map
    filter(quarter %in% polluted_quarters) |>
    select(
      updated_psc_name, ics, organisation_as_recorded,
      quarter, api_date_end, nhs_quarter, inactive_during_quarter,
      name_mismatch
    ) |>
    # then use that map as a look up to extract polluted previous submissions
    left_join(previous_data,
      by = c("updated_psc_name", "ics", "organisation_as_recorded", "quarter")
    ) |>
    # DECISION RULE: the correct org name during a quarter is based on activity status
    # favouring the name of an active org as shown in ODS
    mutate(valid_org_name_as_recorded = case_when(
      inactive_during_quarter == T | name_mismatch == T ~ F,
      .default = T),
      .after = name_mismatch)
  
  # work out the list of orgs that will remain for every quarter
  psc_keep_orgs <- psc_org_polluted_data |>
    filter(valid_org_name_as_recorded == T 
           #| inactive_during_quarter == F
    ) |>
    mutate(organisation_name_after_cleanse = case_when(
      valid_org_name_as_recorded == T ~ organisation_as_recorded#,
      #   inactive_during_quarter == F & name_mismatch == T ~ organisation_as_recorded
    )) |>
    distinct(updated_psc_name, quarter, organisation_name_after_cleanse)
  
  return(list('psc_org_polluted_data' = psc_org_polluted_data,
              'psc_keep_orgs' = psc_keep_orgs))
 }


# this function will take the cut of data identified via identify_polluted_quarters()
# to examine data for each intervention across organisations 
# as this has to be done every quarter, we evaluate via loop
# and return all the results of evaluation in one table 

deduplicate_polluted_data <- function(psc_org_polluted_data){
  
  # psc_org_polluted_data = df object, cut of previous submission data where pollution occurred 
  
  # these will be the quarters to loop through 
  polluted_quarters <- unique(psc_org_polluted_data$quarter)
  
  # empty objects to store loop results
  results_evaluated_all_quarters <- data.frame()
  
  # go through every quarter of polluted data (tested doing all quarters at once but results were incorrect)
  for (polluted_quarter in polluted_quarters) {
    # filter polluted data in the quarter
    psc_org_polluted_data_quarter <- psc_org_polluted_data |>
      filter(quarter == polluted_quarter)
    
    # organisations involved (number might change between quarters)
    orgs <- unique(psc_org_polluted_data_quarter$organisation_as_recorded)
    
    # turn interventions from columns into rows
    polluted_data_long_intervention <- psc_org_polluted_data_quarter |>
      select(-c(
        #organisation_tidy, 
        #organisation_as_recorded, 
        api_date_end, inactive_during_quarter,
        name_mismatch, nhs_quarter, valid_org_name_as_recorded
      )) %>% # need %>% pipe to pass '.' argument in is.na()
      replace(is.na(.), "Stageless") |>
      pivot_longer(
        cols = c(magnesium_sulphate:mews),
        names_to = "intervention"
      ) #|>
      #arrange(intervention)
    
    # evaluate data across organisation for each intervention 
    results_polluted_data_evaluated <- polluted_data_long_intervention |>
      # turn organisation names into columns 
      pivot_wider(
        names_from = "organisation_as_recorded",
        values_from = "value"
      ) |>
      rowwise() |>
      mutate(
        # IMPORTANT: here we test whether the data for each intervention
        # was the same across organisations
        equal_across_orgs = all(
          c_across(all_of(orgs)) == first(c_across(all_of(orgs)))
        ),
        # DECISION RULE: choose which stage value to keep for an intervenetion
        # favouring the least advanced stage 
        # this will be stageless [NA value] when data was not recorded against any of the orgs invovled
        # otherwise stageless will be ignored to favor an actual stage recorded  e.g. : min('Stageless', 'Stage 1')
        stage_to_keep = min(c_across(all_of(orgs)))
      )
    
    results_evaluated_all_quarters <- bind_rows(
      results_evaluated_all_quarters,
      results_polluted_data_evaluated
    )
  }
  
  # how bad the problem was across time
  message("Number of interventions with a mismatch across orgs per quarter")
  
  results_table <- results_evaluated_all_quarters |>
    count(quarter, equal_across_orgs) |>
    arrange(quarter) |>
    filter(equal_across_orgs == F)
  
  print(results_table)
  
  return(results_evaluated_all_quarters)
  }

# function below will use two tables to return cleansed data
# table 1 is results_evaluated_all_quarters - which tells us which data point to keep for a quarter and intervention
# table 2 is psc_keep_orgs which was filtered based on org discrepancies to work out the active organisation for a quarter
# output will be a table with the correct organisation name for a quarter and de-duplicated intervention data

return_cleansed_data <- function(results_evaluated_all_quarters, psc_keep_orgs){
  # results_evaluated_all_quarters = df with tests data equality across organisations 
  # one row per intervention, per quarter - generated via deduplicate_polluted_data()
  # psc_keep_orgs = table with containing active trust per quarter - generated in identify_polluted_quarters() 
  
  # make cleansed intervention data wide again
  psc_keep_interventions <- results_evaluated_all_quarters |>
    select(updated_psc_name, ics, quarter, intervention, stage_to_keep) |>
    pivot_wider(
      names_from = intervention,
      values_from = stage_to_keep
    ) |>
    # reorder columns to emulate previous_submissions_data order
    select(updated_psc_name:quarter, all_of(intervention_order)) |>
    # restore NA values
    mutate(across(all_of(intervention_order), ~ case_when(.x == "Stageless" ~ NA,
                                                          .default = .x
    )))
  
  # join the cleansed results
  psc_data_cleansed <- psc_keep_orgs |>
    left_join(psc_keep_interventions, by = c("updated_psc_name", "quarter")) |>
    # order below reflects structure in previous_submissions_data
    select(
      updated_psc_name, ics, organisation_name_after_cleanse, quarter,
      all_of(intervention_order)
    )
}


x = identify_polluted_quarters(
  map = map_potential_carry_over_across_quarters,
  previous_data = previous_data_legacy_carry_over,
  psc = "(?i)kent",
  orgs = "(?i)brighton|western sussex|university hospitals sussex"
) 

y = x$psc_org_polluted_data |>
  deduplicate_polluted_data() %>% # pipe needed to pass '.' below
  return_cleansed_data(., psc_keep_orgs = x$psc_keep_orgs)

# deploy function
# IMPORTANT: choosing to be overly explicit here whilst we QA this logic. 
# Looping might be more appropriate  
# -- kent --
kent_results <- my_function(
  map = map_potential_carry_over_across_quarters,
  previous_data = previous_data_legacy_carry_over,
  psc = "(?i)kent",
  orgs = "(?i)brighton|western sussex|university hospitals sussex"
)

kent_polluted_data <- kent_results$polluted_data_long_all_quarters
kent_eval_for_qa <- kent_results$results_evaluated_all_quarters
kent_cleansed <- kent_results$psc_data_cleansed

# -- north west coast --
nwc_results <- my_function(
  map = map_potential_carry_over_across_quarters,
  previous_data = previous_data_legacy_carry_over,
  psc = "(?i)north West Coast",
  orgs = "(?i)southport|st helens|mersey"
)

nwc_polluted_data <- nwc_results$polluted_data_long_all_quarters
nwc_eval_for_qa <- nwc_results$results_evaluated_all_quarters
nwc_cleansed <- nwc_results$psc_data_cleansed

# -- south west 1 --
sw_results_somerset <- my_function(
  map = map_potential_carry_over_across_quarters,
  previous_data = previous_data_legacy_carry_over,
  psc = "(?i)south West",
  orgs = "(?i)yeovil|somerset"
)

sw_somerset_polluted_data <- sw_results_somerset$polluted_data_long_all_quarters
sw_somerset_eval_for_qa <- sw_results_somerset$results_evaluated_all_quarters
sw_somerset_cleansed <- sw_results_somerset$psc_data_cleansed

# -- south west 2 --
sw_results_royal_devon <- my_function(
  map = map_potential_carry_over_across_quarters,
  previous_data = previous_data_legacy_carry_over,
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
rows_to_replace <- discrepancies_name_mistakes |>
  distinct(updated_psc_name, ics, organisation_as_recorded, quarter) |> 
  bind_rows(
    # need to use the polluted dfs here because that contains 
    # the confirmed quarters with a legacy carry over 
    kent_polluted_data,
    nwc_polluted_data,
    sw_somerset_polluted_data,
    sw_royal_devon_polluted_data)|> 
  distinct(updated_psc_name, ics, organisation_as_recorded, quarter)

# remove all the rows that have undergone a retroactive correction
previous_submissions_prunned <- previous_submissions_data |> 
  anti_join(rows_to_replace, by = c('updated_psc_name', 'ics',
                                    'organisation_as_recorded', 'quarter')) 

previous_submissions_data_cleansed <- previous_submissions_prunned |> 
  bind_rows(
    # this data df kept col organisation_as_recorded
    previous_data_org_name_corrected,
    # this one didn't
    previous_data_legacy_carry_over_corrected) |>
  relocate(organisation_name_after_cleanse, .after = organisation_as_recorded) |>
  mutate(organisation_name_after_cleanse = 
           case_when(!is.na(organisation_name_after_cleanse) ~ organisation_name_after_cleanse,
                     is.na(organisation_name_after_cleanse) ~ organisation_as_recorded)) |>
  rename(icb = ics)

# write to share point
file_name <- 'mat_neo_qart_cleansed_upto_2024_25_Q2.csv'
  
write.csv(previous_submissions_data_cleansed, 
          here(str_glue('output/{file_name}')),
          row.names = F)

chosenlib$upload_file(
  dest = str_glue('{base_url}/{master_files_folder}/{file_name}'),
  src =  str_glue('output/{file_name}')
)
