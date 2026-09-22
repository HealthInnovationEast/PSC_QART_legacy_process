base_url <- "HIN Informatic_Data & Analytics - 00_General/42 PSC/QART reporting/"

if (sharepoint_local_access) {
  sharepoint_path <- Sys.getenv("sharepoint_path")
  if (sharepoint_path == ""){
    stop('No SharePoint path set')
  }
  SP_folder_path <- paste0(sharepoint_path, base_url)
} else {
  site_url <- Sys.getenv("sharepoint_url")
  site <- get_sharepoint_site(site_url = site_url)
  chosenlib <- site$get_drive("Documents")
}

# Path variables for files (SP_file_path, local_location, local_file, SP_destination) should always include the file name. In addition, the local locations should already have here() applied if necessary

get_SP_file <- function(SP_file_path, local_location) {
  if (sharepoint_local_access){
    copy_success <- file.copy(paste0(SP_folder_path,
                                     SP_file_path),
                              local_location,
                              overwrite = TRUE)
    if (!copy_success) {
      stop(paste0("File download failed: ",
                  paste0(SP_folder_path, SP_file_path),
                  " to ", local_location))
    }
  } else {
    temp_item <- dr$get_item(SP_file_path)

    temp_item$download(
      dest = local_location,
      overwrite = TRUE
    )
  }
}

upload_SP_file <- function(local_file, SP_destination){
  if (sharepoint_local_access){
    copy_success <- file.copy(local_file,
                              paste0(SP_folder_path,
                                     SP_destination),
                              overwrite = TRUE)
    if (!copy_success) {
      stop(paste0("File upload failed: ",
                  local_file, " to ",
                  paste0(SP_folder_path, SP_destination)))
    }
  } else {
    chosenlib$upload_file(
      dest = SP_destination,
      src = local_file,
      overwrite = TRUE
    )
  }
}

list_SP_files <- function(folder_path){
  if (sharepoint_local_access){
    return(data.frame(name = list.files(paste0(sharepoint_path,
                                               folder_path),
                                        include.dirs = TRUE)))
  } else {
    temp_folder_item <- chosenlib$get_item(folder_path)

    return(temp_folder_item$list_files())
  }
}
