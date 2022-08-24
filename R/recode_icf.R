################################################################################
#
#' 
#' Recode IYCF counselling coverage data
#'
#' @param raw_data Survey raw data  
#' 
#
################################################################################

recode_icf <- function(raw_data) {
  icf_df <- raw_data |>
    subset(
      select = c(
        cid, did, eid, `_geolocation`, icf1, icf2, icf2a, icf2a_1, icf2a_2,
        icf2a_3, icf2a_4, icf2a_88, icf2a_other, icf3
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
  
  ## During your pregnancy for your youngest child, did you receive any 
  ## information or heard about any service that provided counselling for 
  ## pregnant women on infant and young child feeding?
  icf1 <- ifelse(
    icf_df$icf1 == "2", 0,
    ifelse(
      icf_df$icf1 == "99", NA, 1
    )
  )
  
  ## During your pregnancy for your youngest child, did you attend any 
  ## counselling session/s for pregnant women on infant and young child feeding?
  icf2 <- ifelse(
    icf_df$icf2 == "2" | icf1 == 0, 0,
    ifelse(
      icf_df$icf2 == "99", NA, 1
    )
  )

  ## Reasons for not attending IYCF counselling
  
  # Timing was not convenient
  icf2a_1 <- ifelse(icf_df$icf2a_1 == 1, 1, 0)
  
  # Not interested
  icf2a_2 <- ifelse(icf_df$icf2a_2 == 1, 1, 0)
  icf2a_2[icf_df$icf2a_other == "She was only concern with the treatment aspect"] <- 1
  
  # Do not trust counsellor
  icf2a_3 <- ifelse(icf_df$icf2a_3 == 1, 1, 0)
  
  # Don't think I need it
  icf2a_4 <- ifelse(icf_df$icf2a_4 == 1, 1, 0)

  # Arrive late to sessions
  icf2a_5 <- vector(mode = "integer", length = nrow(icf_df))
  icf2a_5 <- ifelse(icf_df$icf2a_88 == 0, NA, icf2a_5)
  icf2a_5[icf_df$icf2a_other == "Because I was always later, I use to go after the health talk"] <- 1  
  
  # Clinic/hospital too far away
  icf2a_6 <- vector(mode = "integer", length = nrow(icf_df))
  icf2a_6 <- ifelse(icf_df$icf2a_88 == 0, NA, icf2a_6)
  icf2a_6[icf_df$icf2a_other == "Clinic is very  far away"] <- 1
  icf2a_6[icf_df$icf2a_other == "Long distance"] <- 1
  icf2a_6[icf_df$icf2a_other == "No access to health facility "] <- 1
  
  # Counselling not offered at facility
  icf2a_7 <- vector(mode = "integer", length = nrow(icf_df))
  icf2a_7 <- ifelse(icf_df$icf2a_88 == 0, NA, icf2a_7)
  icf2a_7[icf_df$icf2a_other == "Conseling not offered at facility"] <- 1
  icf2a_7[icf_df$icf2a_other == "No counseling center at the facility "] <- 1
  icf2a_7[icf_df$icf2a_other == "No service for counseling. No one to give her the information"] <- 1
  icf2a_7[icf_df$icf2a_other == "No support "] <- 1
  icf2a_7[icf_df$icf2a_other == "Not Available "] <- 1
  icf2a_7[icf_df$icf2a_other == "Not available to her"] <- 1
  icf2a_7[icf_df$icf2a_other == "Respondent said they did no teach her"] <- 1
  icf2a_7[icf_df$icf2a_other == "Service not provided"] <- 1
  icf2a_7[icf_df$icf2a_other == "She did  not see or heard about it at the facility (St. Joseph Catholic Hospital) that she used to attend."] <- 1
  
  # Other
  icf2a_8 <- vector(mode = "integer", length = nrow(icf_df))
  icf2a_8 <- ifelse(icf_df$icf2a_88 == 0, NA, icf2a_8)
  icf2a_8[icf_df$icf2a_other == "Could not go because  my stomach was too big"] <- 1

  # Don't know about counselling sessions
  icf2a_9 <- vector(mode = "integer", length = nrow(icf_df))
  icf2a_9 <- ifelse(icf_df$icf2a_88 == 0, NA, icf2a_9)
  icf2a_9[icf_df$icf2a_other == "Did not hear about it "] <- 1
  icf2a_9[icf_df$icf2a_other == "Did not know"] <- 1
  icf2a_9[icf_df$icf2a_other == "Did not know about  anything  of such. "] <- 1
  icf2a_9[icf_df$icf2a_other == "Did not know about  it."] <- 1
  icf2a_9[icf_df$icf2a_other == "Did not know about it"] <- 1
  icf2a_9[icf_df$icf2a_other == "Didn't hear from the hospital, heard it from radio"] <- 1
  icf2a_9[icf_df$icf2a_other == "Didn't know about it"] <- 1
  icf2a_9[icf_df$icf2a_other == "Do not know"] <- 1
  icf2a_9[icf_df$icf2a_other == "Don't know about any counselling."] <- 1
  icf2a_9[icf_df$icf2a_other == "Don't know about it"] <- 1
  icf2a_9[icf_df$icf2a_other == "Don't know about it "] <- 1
  icf2a_9[icf_df$icf2a_other == "Don't know anything about it"] <- 1
  icf2a_9[icf_df$icf2a_other == "Health  facility did not tell participant what to do"] <- 1
  icf2a_9[icf_df$icf2a_other == "I didn't go because no one told me to go there"] <- 1
  icf2a_9[icf_df$icf2a_other == "I didn't hear about it"] <- 1
  icf2a_9[icf_df$icf2a_other == "I didn't hear it from the hospital, people in the community told me about it"] <- 1
  icf2a_9[icf_df$icf2a_other == "I was not invited for counseling"] <- 1
  icf2a_9[icf_df$icf2a_other == "Mother claims she has never  been  aware of such ."] <- 1
  icf2a_9[icf_df$icf2a_other == "Mother did  not get any information. "] <- 1
  icf2a_9[icf_df$icf2a_other == "Mother said she was never told about  it."] <- 1
  icf2a_9[icf_df$icf2a_other == "Never heard about it before "] <- 1
  icf2a_9[icf_df$icf2a_other == "Never knew about it"] <- 1
  icf2a_9[icf_df$icf2a_other == "No body told me to go for counselling "] <- 1
  icf2a_9[icf_df$icf2a_other == "No knowledge"] <- 1
  icf2a_9[icf_df$icf2a_other == "No knowledge "] <- 1
  icf2a_9[icf_df$icf2a_other == "No one ask me to go anywhere"] <- 1
  icf2a_9[icf_df$icf2a_other == "No one told me about it"] <- 1
  icf2a_9[icf_df$icf2a_other == "No one told me anything about it at that time "] <- 1
  icf2a_9[icf_df$icf2a_other == "Not ware about it"] <- 1
  icf2a_9[icf_df$icf2a_other == "She claims they never  told her at the Hospital  she went to."] <- 1
  icf2a_9[icf_df$icf2a_other == "She does not know"] <- 1
  icf2a_9[icf_df$icf2a_other == "She doesn't know about the teaching "] <- 1
  icf2a_9[icf_df$icf2a_other == "She never heard about it"] <- 1
  icf2a_9[icf_df$icf2a_other == "Started taking full treatment at 7 months didn't hear about counseling and didn't attend"] <- 1
  icf2a_9[icf_df$icf2a_other == "Was not inform"] <- 1
  icf2a_9[icf_df$icf2a_other == "Was not invited for any "] <- 1
  icf2a_9[icf_df$icf2a_other == "Was not invited for any of it"] <- 1
  icf2a_9[icf_df$icf2a_other == "Was not knowledgeable  of it."] <- 1
  
  ## Don't remember
  icf2a_10 <- vector(mode = "integer", length = nrow(icf_df))
  icf2a_10 <- ifelse(icf_df$icf2a_88 == 0, NA, icf2a_10)
  icf2a_10[icf_df$icf2a_other == "I don't remember"] <- 1

  ## Don't think they are any good
  icf2a_11 <- vector(mode = "integer", length = nrow(icf_df))
  icf2a_11 <- ifelse(icf_df$icf2a_88 == 0, NA, icf2a_11)
  icf2a_11[icf_df$icf2a_other == "I don't think I attended any because no one really me good"] <- 1
  
  # Did not go to the hospital
  icf2a_12 <- vector(mode = "integer", length = nrow(icf_df))
  icf2a_12 <- ifelse(icf_df$icf2a_88 == 0, NA, icf2a_12)
  icf2a_12[icf_df$icf2a_other == "She doesn't  go to the Hospital. "] <- 1
  icf2a_12[icf_df$icf2a_other == "She never use to go to  the  Hospital "] <- 1
  icf2a_12[icf_df$icf2a_other == "She never went to the Hospital  when she was Pregnant. "] <- 1
  icf2a_12[icf_df$icf2a_other == "She never use to go to  the  Hospital "] <- 1
  icf2a_12[icf_df$icf2a_other == "Went for one antenatal visit once  during the entire pregnancy. "] <- 1
  
  ## 
  icf3 <- icf_df$icf3
  
  icf_df <- data.frame(
    icf_df[ , c("cid", "did", "eid", "longitude", "latitude")],
    icf1, icf2, icf2a_1, icf2a_2, icf2a_3, icf2a_4, icf2a_5, icf2a_6,
    icf2a_7, icf2a_8, icf2a_9, icf2a_10, icf2a_11, icf2a_12, icf3
  )

  icf_df
}



