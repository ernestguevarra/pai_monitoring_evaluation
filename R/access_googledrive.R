################################################################################
#
#'
#' Download files from Google Drive
#'
#
################################################################################

download_googledrive <- function(filename, 
                                 path = paste0("data/", filename),
                                 overwrite = FALSE) {
  ## Authenticate
  googledrive::drive_auth(
    email = Sys.getenv("GOOGLE_AUTH_EMAIL"),
    path = Sys.getenv("GOOGLE_AUTH_FILE")
  )
  
  ## 
  googledrive::drive_download(
    file = filename,
    path = path,
    overwrite = overwrite
  )
}


################################################################################
#
#'
#' Access Google Sheets
#'
#
################################################################################

get_googlesheets <- function(id = std_data_id) {
  googlesheets4::gs4_auth(
    email = Sys.getenv("GOOGLE_AUTH_EMAIL"),
    path = Sys.getenv("GOOGLE_AUTH_FILE")
  )
  
  googlesheets4::read_sheet(
    ss = id, sheet = 1
  )
}


get_googlesheets_id <- function(filename) {
  ## Authenticate
  googledrive::drive_auth(
    email = Sys.getenv("GOOGLE_AUTH_EMAIL"),
    path = Sys.getenv("GOOGLE_AUTH_FILE")
  )
  
  files <- googledrive::drive_find()
  
  id <- files |>
    subset(name == filename, select = id) |>
    as.character()

  id
}
