# this script is sourced in retroactive_org_changes.R

# select relevant variables from data set with polluted data
# i.e., the handed-over data where there's legacy carry overs and outdated org names 
previous_submissions_data_polluted <- previous_submissions_psc_name_updated |>
  rename("organisation_as_recorded" = organisation) |>
  select(updated_psc_name:mews,
         # we remove the ics variable as there were mistakes in the mapping of RD8 and RJN to their respective ICB's  
         -ics
         )

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
  rename('organisation_name_after_cleanse' = api_current_org_name)

# apply name corrections
previous_data_org_name_corrected <- previous_submissions_data_polluted |> 
  right_join(name_corrections |> select(-c(organisation_tidy)),
             by = c("updated_psc_name",
                "organisation_as_recorded")
              ) |>
  relocate(api_current_code, 
           organisation_name_after_cleanse, 
           .after = "organisation_as_recorded") |>
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
# whether data for each intervention was identical across legacy and successor orgs
# where there are discrepancies between orgs, then choose which stage to keep

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
  mutate(api_current_org_name = case_when(is.na(api_current_org_name) ~ paste0(organisation_as_recorded), 
                                     .default =  api_current_org_name)
    ) |>
  select(
    # this order will make reading the table easier
    updated_psc_name, #ics, 
    organisation_as_recorded,
    quarter, 
    # these 3 below correspond to the API info of the predecessor orgs
    api_org_code, api_date_end, nhs_inactive_from_quarter, 
    # these 2 correspond to the successor info
    api_current_code, api_current_org_name
  ) |>
  arrange(quarter, updated_psc_name) |>
  # determine organisational inaccuracies
  mutate(
    # was the organisation legacy during a quarter? 
    inactive_during_quarter = case_when(quarter >= nhs_inactive_from_quarter ~ T,
      .default = F
    ),
    # was the organisation using their legal name as of 24/25 Q3
    # this accounts for points in time where a merger (that resulted in a name change) hadn't occurred 
    current_name_in_use = organisation_as_recorded == api_current_org_name
  )

# the function below will be filtering data based on a combination of PSC and trust names
# then it will identify which quarters of data were polluted 
# it will return a cut of the previous submissions by polluted quarter
# it will also return the list of organisations and quarters that will be kept after corrections   

identify_polluted_quarters <- function(map = map_potential_carry_over_across_quarters, 
                                       previous_data = previous_data_legacy_carry_over, 
                                       psc, 
                                       orgs) {
  
  # map = defaulted to map_potential_carry_over_across_quarters - this is a look up of psc, org, and quarter with potential legacy carry overs
  # previous_data = defaulted to previous_data_legacy_carry_over - this the data to be examined
  # psc = patient safety collaborative filter 
  # orgs = trusts filter
  
  # filter discrepancies map to specific combination of PSC and orgs 
  map_psc_org_potential_carry_over <- map |>
    filter(
      str_detect(updated_psc_name, psc),
      str_detect(organisation_as_recorded, orgs)
    )

  # determine quarters where there was data pollution (not all quarters might have had pollution) 
  # e.g, the org name in the template was still accurate because a merger hadn't happened
  potential_quarters <- unique(map_psc_org_potential_carry_over$quarter)
  legacy_quarter <- unique(map_psc_org_potential_carry_over$nhs_inactive_from_quarter[
    !is.na(map_psc_org_potential_carry_over$nhs_inactive_from_quarter)
  ])

  # DECISION RULE: quarters are polluted starting from the quarter an organisation became legacy 
  polluted_quarters <- potential_quarters[which(potential_quarters >= legacy_quarter)]
  message("Identified polluted quarters")
  print(glue::glue("{polluted_quarters}"))

  # extract data for those quarters
  psc_org_polluted_data <- map_psc_org_potential_carry_over |>
    arrange(quarter) |>
    # pull polluted quarters from potential carry over map
    filter(quarter %in% polluted_quarters) |>
    select(
      updated_psc_name, #ics, 
      organisation_as_recorded,
      quarter, api_date_end, nhs_inactive_from_quarter, inactive_during_quarter,
      current_name_in_use, api_current_org_name
    ) |>
    # then use that map as a look up to extract polluted previous submissions
    left_join(previous_data,
      by = c("updated_psc_name", #"ics", 
             "organisation_as_recorded", "quarter")
    ) |>
    # DECISION RULE: determine whether the name the organisation was valid for that quarter 
    mutate(valid_org_name_as_recorded = case_when(
      # name was NOT valid when the organisation was already legacy in that quarter
      inactive_during_quarter == T ~ F,
      # name was NOT valid when the organisation was active but using a name that differed from the up-to-date name for 24/025 Q3
      inactive_during_quarter == F & current_name_in_use == F ~ F,
      .default = T),
      .after = current_name_in_use)
  
  # work out the list of orgs that will remain for every quarter
  psc_keep_orgs <- psc_org_polluted_data |>
    filter(
      # names might be eligible for keeping when it was valid for that quarter
      # or when the organisation shows as NOT inactive and a name change was upcoming or hadn't happened 
      valid_org_name_as_recorded == T |
        inactive_during_quarter == F & current_name_in_use == F |
        inactive_during_quarter == F & current_name_in_use == T 
    ) |>
    # here we choose ONE organisation name to keep for a quarter 
    mutate(organisation_name_after_cleanse = case_when(
      # keep the name if it was valid for that quarter
      valid_org_name_as_recorded == T ~ organisation_as_recorded , 
      # when the org showed as NOT inactive, choose the api name to account for 
      # past and upcoming rename changes 
      inactive_during_quarter == F & current_name_in_use == F ~ api_current_org_name,
      inactive_during_quarter == F & current_name_in_use == T ~ api_current_org_name
    )) |>
    distinct(updated_psc_name, quarter, organisation_name_after_cleanse)
  
  # 2 outputs, one is the polluted data by quarter 
  # and the other is the list of quarters and organisation to keep  
  return(list('psc_org_polluted_data' = psc_org_polluted_data,
              'psc_keep_orgs' = psc_keep_orgs))
 }


# next function will take the cut of data identified via identify_polluted_quarters()
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
        api_date_end, inactive_during_quarter,
        current_name_in_use, nhs_inactive_from_quarter, valid_org_name_as_recorded
      )) %>% # need %>% pipe to pass '.' argument in is.na()
      replace(is.na(.), "Stageless") |>
      pivot_longer(
        cols = c(magnesium_sulphate:mews),
        names_to = "intervention"
      )
    
    # evaluate data across organisations for each intervention 
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
        # favouring the **least** advanced stage 
        # this will be stageless [NA value] when data was not recorded against any of the orgs involved
        # otherwise stageless will be ignored to favor an actual stage recorded  e.g. : min('Stageless', 'Stage 1')
        stage_to_keep = min(c_across(all_of(orgs)))
      )
    
    results_evaluated_all_quarters <- bind_rows(
      results_evaluated_all_quarters,
      results_polluted_data_evaluated
    )
  }
  
  # how bad the problem was across quarters
  message("Number of interventions with a mismatch across orgs per quarter")
  
  results_table <- results_evaluated_all_quarters |>
    count(quarter, equal_across_orgs) |>
    arrange(quarter) |>
    filter(equal_across_orgs == F)
  
  print(results_table)
  
  return(results_evaluated_all_quarters)
  }

# next function below will use two tables to return cleansed data
# table 1 is results_evaluated_all_quarters - which tells us which intervention values to keep for a quarter
# table 2 is psc_keep_orgs which tells us whic organisation to keep for a quarter 
# output will be a table with the corrected organisation name and intervention data for a quarter

return_cleansed_data <- function(results_evaluated_all_quarters, psc_keep_orgs){
  # results_evaluated_all_quarters = df with tests of data equality across organisations 
  # one row per intervention, per quarter - generated via deduplicate_polluted_data()
  # psc_keep_orgs = table containing correclty named active trust per quarter - generated in identify_polluted_quarters() 
  
  # make cleansed intervention data wide again
  psc_keep_interventions <- results_evaluated_all_quarters |>
    select(updated_psc_name, #ics, 
           quarter, intervention, stage_to_keep) |>
    pivot_wider(
      names_from = intervention,
      values_from = stage_to_keep
    ) |>
    # reorder columns to emulate previous_submissions_data_polluted order
    select(updated_psc_name:quarter, all_of(intervention_order)) |>
    # restore NA values
    mutate(across(all_of(intervention_order), ~ case_when(.x == "Stageless" ~ NA,
                                                          .default = .x
    )))
  
  # join the cleansed results
  psc_data_cleansed <- psc_keep_orgs |>
    left_join(psc_keep_interventions, by = c("updated_psc_name", "quarter")) |>
    # order below reflects structure in previous_submissions_data_polluted
    select(
      updated_psc_name, #ics, 
      organisation_name_after_cleanse, quarter,
      all_of(intervention_order)
    )
}


# deploy function
# IMPORTANT: choosing to be overly explicit here for ease of QA

# -- kent --
kent_polluted <- identify_polluted_quarters(
  psc = "(?i)kent",
  orgs = "(?i)brighton|western sussex|university hospitals sussex"
) 

# final result is a df with data for just university hospitals sussex
# since brighton and western sussex were legacy since data collection started 
# and should have never been in the templates 
kent_cleansed <- kent_polluted$psc_org_polluted_data |>
  deduplicate_polluted_data() %>% # pipe needed to pass '.' below
  return_cleansed_data(., psc_keep_orgs = kent_polluted$psc_keep_orgs)

# -- north west coast --

nwc_polluted <- identify_polluted_quarters(
  psc = "(?i)north West Coast",
  orgs = "(?i)southport|st helens|mersey"
)

# final df reflects the dissolution of south port and subsequent transfer to st helens 
# which then changed their name to mersey
nwc_cleansed <-  nwc_polluted$psc_org_polluted_data |>
  deduplicate_polluted_data() %>% # pipe needed to pass '.' below
  return_cleansed_data(., psc_keep_orgs = nwc_polluted$psc_keep_orgs)

# -- south west 1 --

sw_somerset_poluted = identify_polluted_quarters(
  psc = "(?i)south West",
  orgs = "(?i)yeovil|somerset"
)

# final df reflects acquisition of yeovil by somerset
sw_somerset_cleansed = sw_somerset_poluted$psc_org_polluted_data |>
  deduplicate_polluted_data() %>% # pipe needed to pass '.' below
  return_cleansed_data(., psc_keep_orgs = sw_somerset_poluted$psc_keep_orgs)

# -- south west 2 --

sw_royal_devon_poluted = identify_polluted_quarters(
  psc = "(?i)south West",
  orgs = "(?i)northern devon|royal devon"
)

# final df reflects the acquisition of northern devon by royal devon and exeter 
# which then changed their name to royal devon university healthcare
sw_royal_devon_cleansed = sw_royal_devon_poluted$psc_org_polluted_data |>
  deduplicate_polluted_data() %>% # pipe needed to pass '.' below
  return_cleansed_data(., psc_keep_orgs = sw_royal_devon_poluted$psc_keep_orgs)

# bind cleansed results
previous_data_legacy_carry_over_corrected <- bind_rows(
  kent_cleansed,
  nwc_cleansed,
  sw_somerset_cleansed,
  sw_royal_devon_cleansed ) |>
  left_join( discrepancies |> distinct(api_current_org_name, api_current_code),
             by = c('organisation_name_after_cleanse' = 'api_current_org_name')) |>
  relocate(api_current_code, .before = organisation_name_after_cleanse) |>
  mutate(
    retroactive_fix = T,
    discrepancy_type = "legacy-trust-carry-over"
  )

# bring all corrections together ----------------------------------------

# find the rows to be removed from original data set (i.e., the polluted rows)
rows_to_replace <- previous_data_org_name_corrected |>
  distinct(updated_psc_name, organisation_as_recorded, quarter) |> 
  bind_rows(
    # need to use the polluted dfs here because that contains 
    # the confirmed quarters with a legacy carry over 
    kent_polluted$psc_org_polluted_data,
    nwc_polluted$psc_org_polluted_data,
    sw_somerset_poluted$psc_org_polluted_data,
    sw_royal_devon_poluted$psc_org_polluted_data)|> 
  distinct(updated_psc_name, 
           organisation_as_recorded, quarter) |>
  arrange(updated_psc_name)

# separate the unchanged data from the changed data
previous_submissions_prunned <- previous_submissions_data_polluted |> 
  # remove all the rows that have undergone a retroactive correction
  anti_join(rows_to_replace, by = c('updated_psc_name', 'organisation_as_recorded', 'quarter')) |>
  # add org codes and ICB info to remaining data using map produced in API calls
  # because we're working with unchanged data we use organisation names as originally recorded
  left_join(map_trusts_legacy_status_icb_details |> distinct(organisation, api_org_code, api_icb_code, api_icb_name),
            by = c('organisation_as_recorded' = 'organisation')) |>
  relocate(api_icb_code, api_icb_name, api_org_code, 
           .before = organisation_as_recorded)

# bring all retroactive corrections together
previous_submissions_all_corrected <- bind_rows(
  # this data df kept col organisation_as_recorded
  previous_data_org_name_corrected,
  # this one didn't
  previous_data_legacy_carry_over_corrected) |>
  # add ICB info from lookup generated after determining legacy status 
  left_join(map_psc_trust_icb_active_orgs |> distinct(api_current_code, api_current_org_name, api_icb_code, api_icb_name),
            by = c('organisation_name_after_cleanse' = 'api_current_org_name',
                   'api_current_code'
                    )) |>
  rename('api_org_code' = api_current_code) |>
  relocate(api_icb_code, api_icb_name, api_org_code, .before = organisation_as_recorded)

# produce final data frame with cleansed data
previous_submissions_data_cleansed <- previous_submissions_prunned |> 
  bind_rows(previous_submissions_all_corrected) |>
  relocate(organisation_name_after_cleanse, .after = organisation_as_recorded) |>
   mutate(organisation_name_after_cleanse = 
            case_when(!is.na(organisation_name_after_cleanse) ~ organisation_name_after_cleanse,
                      is.na(organisation_name_after_cleanse) ~ organisation_as_recorded)) 
# write to share point
file_name <- 'mat_neo_qart_cleansed_upto_2024_25_Q3.csv'
  
write.csv(previous_submissions_data_cleansed, 
          here(str_glue('output/{file_name}')),
          row.names = F)

master_files_folder <- '1. Master files'

chosenlib$upload_file(
  dest = str_glue('{base_url}/{master_files_folder}/{file_name}'),
  src =  str_glue('output/{file_name}')
)
