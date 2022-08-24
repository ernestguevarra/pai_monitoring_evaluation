################################################################################
#
#' 
#' Recode screening data
#'
#' @param raw_data Survey raw data  
#' 
#
################################################################################

recode_screening <- function(raw_data) {
  screening_df <- raw_data |>
    subset(
      select = c(cid, did, eid, `_geolocation`, muac_screen, oedema_screen)
    ) |>
    (\(x) 
     {
       geocoords <- do.call(rbind, x$`_geolocation`)
       x$longitude <- geocoords[ , 2]
       x$latitude <- geocoords[ , 1]
       x <- subset(x, select = -`_geolocation`)
       x
    }
    )()
  
  muac_screen <- ifelse(
    screening_df$muac_screen == "2", 0,
    ifelse(
      screening_df$muac_screen == "99", NA, 1
    )
  )
  
  oedema_screen <- ifelse(
    screening_df$oedema_screen == "2", 0,
    ifelse(
      screening_df$oedema_screen == "99", NA, 1
    )
  )

  screening_df <- data.frame(
    screening_df[ , c("cid", "did", "eid", "longitude", "latitude")],
    muac_screen, oedema_screen
  )
  
  screening_df
}

