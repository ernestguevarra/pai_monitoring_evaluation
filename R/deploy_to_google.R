################################################################################
#
#'
#' Deploy to Google Drive
#'
#
################################################################################

deploy_to_googledrive <- function(media, path, name) {
  ## Authenticate
  googledrive::drive_auth(
    email = Sys.getenv("GOOGLE_AUTH_EMAIL"),
    path = Sys.getenv("GOOGLE_AUTH_FILE")
  )
  
  ##
  x <- googledrive::drive_upload(
    media = media,
    path = path,
    name = name,
    overwrite = TRUE
  )
  
  list(
    x,
    googledrive::drive_link(x)
  )
}