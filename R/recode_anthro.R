################################################################################
#
#' 
#' Recode anthro data
#'
#' @param raw_data Survey raw data  
#' 
#
################################################################################

recode_anthro <- function(raw_data) {
  anthro_df <- raw_data |>
    subset(
      select = c(
        cid, did, eid, `_geolocation`, sex, age, muac, oedema, sam, mam
      )
    ) |>
    (\(x) { x$oedema <- ifelse(x$oedema == '1', 1, 0); x })() |>
    (\(x) { x$gam <- ifelse(x$muac < 12.5, 1, 0); x })() |>
    (\(x) 
     {
       geocoords <- do.call(rbind, x$`_geolocation`)
       x$longitude <- geocoords[ , 2]
       x$latitude <- geocoords[ , 1]
       x <- subset(x, select = -`_geolocation`)
       x
    }
    )()
  
  oedema <- ifelse(anthro_df$oedema == "1", 1, 0)
  gam <- ifelse(anthro_df$muac < 12.5, 1, 0)
  
  anthro_df <- data.frame(
    anthro_df[ , c("cid", "did", "eid", "longitude", "latitude", "sex", "age", "muac")],
    anthro_df[ , c("mam", "sam")], gam, oedema
  )
  
  anthro_df
}


