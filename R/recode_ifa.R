################################################################################
#
#' 
#' Recode iron-folic acid coverage data
#'
#' @param raw_data Survey raw data  
#' 
#
################################################################################

recode_ifa <- function(raw_data) {
  ifa_df <- raw_data |>
    subset(
      select = c(
        cid, did, eid, `_geolocation`, ifa1, ifa2, ifa3, ifa3a, ifa3a_other,
        ifa3a_1, ifa3a_2, ifa3a_88, ifa4, ifa4a, ifa4a_other, ifa4a_88, ifa5
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
  
  ## During your pregnancy for your youngest child, did you see anyone for 
  ## antenatal care?
  ifa1 <- ifelse(
    ifa_df$ifa1 == "2", 0,
    ifelse(
      ifa_df$ifa1 == "99", NA, 1
    )
  )
  
  ## During your pregnancy for your youngest child, did you receive any 
  ## information about taking iron-folic acid tablets?
  ifa2 <- ifelse(
    ifa_df$ifa2 == "2", 0,
    ifelse(
      ifa_df$ifa2 == "99", NA, 1
    )
  )

  ## During your pregnancy for your youngest child, did you receive and/or 
  ## purchase iron-folic acid tablets?
  ifa3 <- ifelse(
    ifa_df$ifa3 == "2" | ifa2 == 0, 0,
    ifelse(
      ifa_df$ifa3 == "99", NA, 1
    )
  )
  
  ## Reasons for not receiving/purchasing iron-folic acid tablets
  
  # Health centre ran out
  ifa3a_1 <- ifelse(ifa_df$ifa3a_1 == 1, 1, 0)
  ifa3a_1[ifa_df$ifa3a_other == "Did not receive"] <- 1
  ifa3a_1[ifa_df$ifa3a_other == "Was not given to me"] <- 1
  
  # Took too long to get tablets
  ifa3a_2 <- ifelse(ifa_df$ifa3a_2 == 1, 1, 0)
  
  # Too expensive
  ifa3a_3 <- vector(mode = "integer", length = nrow(ifa_df))
  ifa3a_3 <- ifelse(ifa_df$ifa3a_88 == 0, NA, ifa3a_3)
  ifa3a_3[ifa_df$ifa3a_other == "Always told me to buy it and I didn't have money"] <- 1
  
  # Not aware of IFA
  ifa3a_4 <- vector(mode = "integer", length = nrow(ifa_df))
  ifa3a_4 <- ifelse(ifa_df$ifa3a_88 == 0, NA, ifa3a_4)
  ifa3a_4[ifa_df$ifa3a_other == "Did not know about it"] <- 1
  ifa3a_4[ifa_df$ifa3a_other == "Don't know it"] <- 1
  ifa3a_4[ifa_df$ifa3a_other == "Haven't heard about the tablet"] <- 1
  ifa3a_4[ifa_df$ifa3a_other == "I didn't hear about it, neither did I see it."] <- 1
  ifa3a_4[ifa_df$ifa3a_other == "Haven't heard  about the tablet "] <- 1
  ifa3a_4[ifa_df$ifa3a_other == "Don't remember receiving medicine like this"] <- 1
  ifa3a_4[ifa_df$ifa3a_other == "I don't know about it"] <- 1
  ifa3a_4[ifa_df$ifa3a_other == "No knowledge"] <- 1
  ifa3a_4[ifa_df$ifa3a_other == "No knowledge "] <- 1
  ifa3a_4[ifa_df$ifa3a_other == "The clinic never mentioned the iron-folic tablets to her during her entire pregnancy for this child,  haven't  heard nor see  it."] <- 1
  ifa3a_4[ifa_df$ifa3a_other == "No knowledge "] <- 1
  ifa3a_4[ifa_df$ifa3a_other == "Never took it before "] <- 1
  
  # Don't believe in it
  ifa3a_5 <- vector(mode = "integer", length = nrow(ifa_df))
  ifa3a_5 <- ifelse(ifa_df$ifa3a_88 == 0, NA, ifa3a_5)
  ifa3a_5[ifa_df$ifa3a_other == "Don't  believe  in it."] <- 1  
  
  # Don't like it
  ifa3a_6 <- vector(mode = "integer", length = nrow(ifa_df))
  ifa3a_6 <- ifelse(ifa_df$ifa3a_88 == 0, NA, ifa3a_6)
  ifa3a_6[ifa_df$ifa3a_other == "Don't like it"] <- 1
  ifa3a_6[ifa_df$ifa3a_other == "She hates medication "] <- 1
  
  # Health centre very far
  ifa3a_7 <- vector(mode = "integer", length = nrow(ifa_df))
  ifa3a_7 <- ifelse(ifa_df$ifa3a_88 == 0, NA, ifa3a_7)
  ifa3a_7[ifa_df$ifa3a_other == "Health Center veryfar"] <- 1
  
  # Didn't go to the hospital/clinic
  ifa3a_8 <- vector(mode = "integer", length = nrow(ifa_df))
  ifa3a_8 <- ifelse(ifa_df$ifa3a_88 == 0, NA, ifa3a_8)
  ifa3a_8[ifa_df$ifa3a_other == "I didn't go to the hospital "] <- 1
  ifa3a_8[ifa_df$ifa3a_other == "Mother didn't visit hospital during pregnancy. She was treated in the Bush with traditional herbs by her family people"] <- 1
  ifa3a_8[ifa_df$ifa3a_other == "Never went to clinic "] <- 1
  ifa3a_8[ifa_df$ifa3a_other == "Never went to the clinic "] <- 1
  ifa3a_8[ifa_df$ifa3a_other == "Respondent didn't attend ANC at all"] <- 1
  ifa3a_8[ifa_df$ifa3a_other == "Respondent never had access to health facility "] <- 1
  ifa3a_8[ifa_df$ifa3a_other == "She doesn't  go  to  Hospital. "] <- 1
  ifa3a_8[ifa_df$ifa3a_other == "She never  went to the Hospital. "] <- 1
  ifa3a_8[ifa_df$ifa3a_other == "She never went to the Hospital  when she was Pregnant. "] <- 1
  ifa3a_8[ifa_df$ifa3a_other == "The mother said  she just didn't  want to give herself  hard time to go to the Hospital."] <- 1
  
  # Don't know
  ifa3a_9 <- vector(mode = "integer", length = nrow(ifa_df))
  ifa3a_9 <- ifelse(ifa_df$ifa3a_88 == 0, NA, ifa3a_9)
  ifa3a_9[ifa_df$ifa3a_other == "I don't know"] <- 1
  ifa3a_9[ifa_df$ifa3a_other == "Nothing"] <- 1

  ## During your pregnancy for your youngest child, did you take 
  ## iron-folic acid tablets?
  ifa4 <- ifelse(
    ifa_df$ifa4 == "2" | ifa3 == 0, 0,
    ifelse(
      ifa_df$ifa4 == "99", NA, 1
    )
  )
  
  ## Reasons for not taking tablets
  
  # Concerns about side effects
  ifa4a_1 <- vector(mode = "integer", length = nrow(ifa_df))
  ifa4a_1 <- NA_character_
  
  # Don't need it
  ifa4a_2 <- vector(mode = "integer", length = nrow(ifa_df))
  ifa4a_2 <- NA_character_
  
  # I was told not to take it
  ifa4a_3 <- vector(mode = "integer", length = nrow(ifa_df))
  ifa4a_3 <- NA_character_
  
  # I don't think it helps
  ifa4a_4 <- vector(mode = "integer", length = nrow(ifa_df))
  ifa4a_4 <- NA_character_
  
  # Don't know
  ifa4a_5 <- vector(mode = "integer", length = nrow(ifa_df))
  ifa4a_5 <- ifelse(ifa_df$ifa4a_88 == 0, NA, ifa4a_5)
  ifa4a_5[ifa_df$ifa4a_other == "Don't know"] <- 1
    
  ## How many days did you take IFA?
  ifa5 <- ifa_df$ifa5
  
  ## At least 90 days of IFA consumption
  ifa5a <- ifelse(ifa5 >= 90, 1, 0)
  
  ifa_df <- data.frame(
    ifa_df[ , c("cid", "did", "eid", "longitude", "latitude")],
    ifa1, ifa2, ifa3, ifa3a_1, ifa3a_2, ifa3a_3, ifa3a_4, ifa3a_5, ifa3a_6,
    ifa3a_7, ifa3a_8, ifa3a_9, ifa4, ifa4a_1, ifa4a_2, ifa4a_3, ifa4a_4, ifa4a_5,
    ifa5, ifa5a
  )

  ifa_df
}



