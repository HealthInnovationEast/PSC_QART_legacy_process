library(here)
library(readxl)
library(tidyverse)
library(janitor)
library(glue)
library(Microsoft365R)

reporting_quarter <- '2024/25 Q2'

site_url <- "https://nhs.sharepoint.com/sites/MED/ps2/it/mit"

site <- get_sharepoint_site(site_url = site_url, tenant="nhs")

# retrieve hin folders

reslib <- site$get_drive("Restricted Library")

dr <- reslib$get_item("Measurement/QART")

hin_folders <- dr$list_files() |>
  select(name) |>
  filter(str_detect(name, 'HIN$')) |>
  filter(str_detect(name, 'East Midlands')) |>
  as.vector()

results <- list()
  
for (hin in hin_folders){
  # identify submission
  hin_dir <- reslib$get_item(glue::glue("Measurement/QART/{hin}"))
  
  hin_files <- hin_dir$list_files()
  
  # PROBLEM: need to find appropriate way to identify files
  hin_submission_file <- hin_files$name[1]
  
  hin_submission <- hin_dir$get_item(hin_submission_file)

  # download file and store temporarily
  tf <- tempfile(pattern = str_remove(hin_submission_file, fixed('.xlsx')), 
                 fileext = '.xlsx')
       
  hin_submission$download(dest = tf, 
                          overwrite = T)
  
  # read
  data <- read_excel(path = tf, sheet = 'MatNeo', 
                     range = 'B7:L44')
  
  #cut 1 opt data
  data_opt <- data[1:16,] 
  
  data_opt[2,1] <- 'ICB'
  data_opt[2,2] <- 'Trust'
  
  data_opt_tidy <- data_opt |>
    row_to_names(row_number = 2) |>
    remove_empty('rows')
  
  #cut 2 newtt2 data
  nwwtt2_coordinates <- which(data == 'NEWTT 2', arr.ind = T)
  nwwtt2_row <- nwwtt2_coordinates[1:1]
  
  data_nwwtt2 <- data[nwwtt2_row:37, 1:3] |>
    tail(-2) 
  
  names(data_nwwtt2) <- c('ICB', 'Trust', 'Newtt2')
  
  data_nwwtt2_tidy <- data_nwwtt2 |>
    remove_empty('rows')
  
  #cut 3 mews data 
  mews_coordinates <- which(data == 'MEWS', arr.ind = T)
  mews_row <- mews_coordinates[1:1]
  mews_col <- mews_coordinates[3]
  
  data_mews <- data[mews_row:37, c(1:2, mews_col)] |>
    tail(-2)
  
  names(data_mews) <- c('ICB', 'Trust', 'Mews')
  
  data_mews_tidy <- data_mews |>
    remove_empty('rows')
  
  #now join all 3 cuts
  data_combined <- data_opt_tidy |>
    left_join(data_nwwtt2_tidy, by = c('ICB', 'Trust')) |>
    left_join(data_mews_tidy, by = c('ICB', 'Trust')) |>
    mutate(quarter = reporting_quarter, 
           .after = Trust
    )
  
  results[[hin]] <- data_combined
}

data_combined <- do.call(rbind, results) |>
  rownames_to_column('psc_origin_row')

#write
quarter_string <- reporting_quarter |> 
  str_replace_all('/| ', '_')

time_stamp_ext <- format(Sys.time(), "%Y-%m-%d_%H%M%S.csv")

write.csv(data_combined, 
          file = here(glue::glue('output/{quarter_string}_{time_stamp_ext}')),
          row.names = F
          )
