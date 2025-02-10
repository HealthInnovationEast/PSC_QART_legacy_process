library(here)
library(readxl)
library(tidyverse)
library(janitor)
library(glue)
library(Microsoft365R)

reporting_quarter <- "2024/25 Q2"

site_url <- "https://nhs.sharepoint.com/sites/MED/ps2/it/mit"

site <- get_sharepoint_site(site_url = site_url, tenant = "nhs")

# retrieve hin folders

reslib <- site$get_drive("Restricted Library")

dr <- reslib$get_item("Measurement/QART")

hin_folders <- dr$list_files() |>
  select(name) |>
  filter(str_detect(name, "HIN$")) |>
  arrange(name) |>
  as.vector() |>
  unlist() # so that vector length reflects number of pscs

results <- tibble()

for (hin in hin_folders) {
  # identify submission
  print(glue::glue("Checking data for {hin}"))

  hin_dir <- reslib$get_item(glue::glue("Measurement/QART/{hin}"))

  hin_files <- hin_dir$list_files()

  hin_submission_file <- hin_files |>
    select(name) |>
    # submissions must be saved with returned ending on file name
    filter(str_detect(name, "(?i)returned"))

  if (nrow(hin_submission_file) == 0) {
    print(glue::glue("Skipping {hin}"))
    print(glue::glue("There are no returned files"))
    next
  }

  if (nrow(hin_submission_file) > 1) {
    print(glue::glue("Skipping {hin}"))
    print("There are multiple returned files:")
    print(glue::glue("{hin_submission_file}"))
    next
  }

  hin_submission <- hin_dir$get_item(hin_submission_file)

  # # download file and store temporarily
  tf <- tempfile(
    pattern = str_remove(hin_submission_file, fixed(".xlsx")),
    fileext = ".xlsx"
  )

  hin_submission$download(
    dest = tf,
    overwrite = T
  )

  # read
  data <- read_excel(
    path = tf, sheet = "MatNeo",
    range = "B7:L44"
  )

  # cut 1 opt data
  data_opt <- data[1:16, ]

  data_opt[2, 1] <- "ICB"
  data_opt[2, 2] <- "Trust"

  data_opt_tidy <- data_opt |>
    row_to_names(row_number = 2) |>
    remove_empty("rows")

  # validation - if cells are blank don't continue with upload
  if (purrr::is_empty(which(is.na(data_opt_tidy))) == FALSE) {
    print(glue::glue("Skipping {hin}"))
    print("Optimisation data grid was not fully complete")
    next
  }

  # cut 2 newtt2 data
  data_nwwtt2_mews <- data[22:46, 1:6]

  nwwtt2_location <- data.frame(which(data_nwwtt2_mews == "NEWTT 2", arr.ind = T))
  # validation

  if (nwwtt2_location$col != 3) {
    print(glue::glue("Skipping {hin}"))
    print("NWWTT2 data not found in expected location")
    next
  }

  data_nwwtt2 <- data_nwwtt2_mews[, 1:nwwtt2_location$col] |>
    tail(-2)

  names(data_nwwtt2) <- c("ICB", "Trust", "Newtt2")

  data_nwwtt2_tidy <- data_nwwtt2 |>
    remove_empty("rows")

  # validation
  if (purrr::is_empty(which(is.na(data_nwwtt2_tidy))) == FALSE) {
    print(glue::glue("Skipping {hin}"))
    print("NWWTT2 grid was not fully complete")
    next
  }

  # cut 3 mews data
  mews_location <- data.frame(which(data_nwwtt2_mews == "MEWS", arr.ind = T))

  # validation
  if (mews_location$col != 5) {
    print(glue::glue("Skipping {hin}"))
    print("MEWS data not found in expected location")
    next
  }

  data_mews <- data_nwwtt2_mews[, c(1:2, mews_location$col)] |>
    tail(-2)

  names(data_mews) <- c("ICB", "Trust", "Mews")

  data_mews_tidy <- data_mews |>
    remove_empty("rows")

  # validation
  if (purrr::is_empty(which(is.na(data_mews_tidy))) == FALSE) {
    print(glue::glue("Skipping {hin}"))
    print("MEWS grid was not fully complete")
    next
  }

  # now join all 3 cuts
  data_combined <- data_opt_tidy |>
    mutate(
      hin_name = hin,
      .before = ICB
    ) |>
    left_join(data_nwwtt2_tidy, by = c("ICB", "Trust")) |>
    left_join(data_mews_tidy, by = c("ICB", "Trust")) |>
    mutate(
      quarter = reporting_quarter,
      .after = Trust
    )

  print(glue::glue("Successful data extraction for {hin}"))

  results <- results |>
    bind_rows(data_combined)
}

# write combined data
quarter_string <- reporting_quarter |>
  str_replace_all("/| ", "_")

time_stamp_ext <- format(Sys.time(), "%Y-%m-%d_%H%M%S.csv")

write.csv(data_combined,
  file = here(glue::glue("output/psc_submissions_{quarter_string}_processed_{time_stamp_ext}")),
  row.names = F
)

# write current hin names for lookup file(s)
hin_names <- hin_folders |> as.data.frame()

write.csv(hin_names,
  file = here("hin_names.csv"),
  row.names = F
)
