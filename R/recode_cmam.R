################################################################################
#
#' 
#' Recode CMAM data
#'
#' @param raw_data Survey raw data  
#' 
#
################################################################################

recode_cmam <- function(raw_data) {
  raw_cmam_df <- raw_data |>
    subset(
      select = c(
        cid, did, eid, `_geolocation`, sex, age, muac, oedema, 
        #muac_screen, oedema_screen, 
        sam, mam, nut_status, cov_status, 
        `in`, recovering, out, q1, q1a, q1a_1, q1a_2, q1a_3, q1a_4, q1a_5,
        q1a_6, q2, q3, q4, q4t, q4_q4a, q4_q4b, q4_q4c, q4_q4d, q4_q4h, q4_q4i, 
        q4_q4p, q4_q4q, q4_q4s, q5, q6, q6e, q6_q6a, q6_q6b, q6_q6d
      )
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
    
  # muac_screen <- ifelse(
  #   raw_cmam_df$muac_screen == "2", 0,
  #   ifelse(
  #     raw_cmam_df$muac_screen == "99", NA, 1
  #   )
  # )
  
  # oedema_screen <- ifelse(
  #   raw_cmam_df$oedema_screen == "2", 0,
  #   ifelse(
  #     raw_cmam_df$oedema_screen == "99", NA, 1
  #   )
  # )
  
  sam_in <- ifelse(
    raw_cmam_df$sam == 1 & raw_cmam_df$cov_status == "1", 1,
    ifelse(
      raw_cmam_df$sam == 0, NA, 0
    )
  )
  
  sam_out <- ifelse(
    raw_cmam_df$sam == 1 & raw_cmam_df$cov_status == "2", 1,
    ifelse(
      raw_cmam_df$sam == 0, NA, 0
    )
  )
  
  rec_in <- ifelse(
    raw_cmam_df$sam == 0 & raw_cmam_df$cov_status == "1", 1,
    ifelse(
      raw_cmam_df$sam == 1, NA, 0
    )
  )
  
  # rec_in <- ifelse(
  #   raw_cmam_df$sam == 0 & raw_cmam_df$cov_status == "1", 1, NA
  # )
  
  q1 <- ifelse(raw_cmam_df$q1 == "2", 0, 1)
  q1a_1 <- raw_cmam_df$q1a_1
  q1a_2 <- raw_cmam_df$q1a_2
  q1a_3 <- raw_cmam_df$q1a_3
  q1a_4 <- raw_cmam_df$q1a_4
  q1a_5 <- raw_cmam_df$q1a_5
  q1a_6 <- raw_cmam_df$q1a_6
  q2 <- ifelse(raw_cmam_df$q2 == "2", 0, 1)
  q3 <- ifelse(raw_cmam_df$q3 == "2", 0, 1)
  
  ## Child not measured
  q4a <- raw_cmam_df$q4_q4a
  q4a[raw_cmam_df$q4t == "His MUAC was not  done  at ELWA Hospital on two occasions"] <- 1
  
  ## Too far
  q4b <- raw_cmam_df$q4_q4b
  
  ## No time/too busy
  q4c <- raw_cmam_df$q4_q4c
  
  ## Mother/carer is sick
  q4d <- raw_cmam_df$q4_q4d
  
  ## No one else to take care of other siblings
  q4h <- raw_cmam_df$q4_q4h
  
  ## Child rejected by the programme already
  q4i <- raw_cmam_df$q4_q4i
  
  ## RUTF out-of-stock
  q4p <- raw_cmam_df$q4_q4p
  
  ## Lack of money for transport
  q4q <- raw_cmam_df$q4_q4q
  q4q[raw_cmam_df$q4t == "No money to carry the child"] <- 1
  
  ## Other
  q4u <- vector(mode = "numeric", length = nrow(raw_cmam_df))
  q4u[q3 == 0 | is.na(q3)] <- NA
  q4u[raw_cmam_df$q4t == "Child was not able to eat any thing "] <- 1
  q4u[raw_cmam_df$q4t != "Child was not able to eat any thing "] <- 0
  
  ## Did not know that child is sick
  q4v <- vector(mode = "numeric", length = nrow(raw_cmam_df))
  q4v[q3 == 0 | is.na(q3)] <- NA
  q4v[raw_cmam_df$q4t == "Don't know that the child is sick"] <- 1
  q4v[raw_cmam_df$q4t == "Mother doesn't know that her son has Malnutrition"] <- 1
  q4v[raw_cmam_df$q4t == "Mother don't know that the child is SAM"] <- 1
  q4v[raw_cmam_df$q4t == "Mother is not aware of her daughter  being Malnourished  that's  why she hasn't  gone to the Hospital  within  the last 6 months."] <- 1
  q4v[raw_cmam_df$q4t == "No I don't know about anything"] <- 1
  q4v[raw_cmam_df$q4t == "Wasn't aware of SAM case"] <- 1

  
  cmam_df <- data.frame(
    raw_cmam_df[ , c("cid", "did", "eid", "longitude", "latitude", 
                     "age", "sex", "muac", "oedema")],
    #muac_screen, oedema_screen, 
    sam_in, sam_out, rec_in,
    q1, q1a_1, q1a_2, q1a_3, q1a_4, q1a_5, q1a_6, q2, q3,
    q4a, q4b, q4c, q4d, q4h, q4i, q4p, q4q, q4u, q4v
  )
  
  cmam_df
}


################################################################################
#
#'
#' Aggregate CMAM indicators by enumeration area
#'
#
################################################################################

aggregate_cmam <- function(cmam_df) {
  cmam_df <- cmam_df |>
    dplyr::group_by(eid) |>
    dplyr::summarise(
      cid = cid[1],
      did = did[1],
      longitude = mean(longitude),
      latitude = mean(latitude),
      #muac_screen = mean(muac_screen, na.rm = TRUE),
      #oedema_screen = mean(oedema_screen, na.rm = TRUE),
      sam_in = sum(sam_in, na.rm = TRUE),
      sam_out = sum(sam_out, na.rm = TRUE),
      rec_in = sum(rec_in, na.rm = TRUE),
      case_finding = calculate_case_finding(sam_in, sam_out),
      treatment = calculate_treatment(sam_in, sam_out, rec_in, k = 3),
      q1 = mean(q1, na.rm = TRUE),
      q1a_1 = mean(q1a_1, na.rm = TRUE),
      q1a_2 = mean(q1a_2, na.rm = TRUE),
      q1a_3 = mean(q1a_3, na.rm = TRUE),
      q1a_4 = mean(q1a_4, na.rm = TRUE),
      q1a_5 = mean(q1a_5, na.rm = TRUE),
      q1a_6 = mean(q1a_6, na.rm = TRUE),
      q2 = mean(q2, na.rm = TRUE),
      q3 = mean(q3, na.rm = TRUE),
      q4a = mean(q4a, na.rm = TRUE),
      q4b = mean(q4b, na.rm = TRUE),
      q4c = mean(q4c, na.rm = TRUE),
      q4d = mean(q4d, na.rm = TRUE),
      q4h = mean(q4h, na.rm = TRUE),
      q4i = mean(q4i, na.rm = TRUE),
      q4p = mean(q4p, na.rm = TRUE),
      q4q = mean(q4q, na.rm = TRUE),
      q4u = mean(q4u, na.rm = TRUE),
      q4v = mean(q4v, na.rm = TRUE)
    )
  
  cmam_df
}
