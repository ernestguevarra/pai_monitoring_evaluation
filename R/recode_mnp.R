################################################################################
#
#' 
#' Recode micronutrient powder data
#'
#' @param raw_data Survey raw data  
#' 
#
################################################################################

recode_mnp <- function(raw_data) {
  mnp_df <- raw_data |>
    subset(
      select = c(
        cid, did, eid, `_geolocation`, mnp1, mnp2, mnp2a, mnp2a_1, mnp2a_2,
        mnp2a_3, mnp2a_4, mnp2a_5, mnp2a_88, mnp2a_other,
        mnp3, mnp3a, mnp3a_4, mnp3a_9, mnp3a_88, mnp3a_other,
        mnp4
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
  
  ## Have you heard about MNP?
  mnp1 <- ifelse(
    mnp_df$mnp1 == "2", 0,
    ifelse(
      mnp_df$mnp1 == "99", NA, 1
    )
  )
  
  ## Have your received/bought MNP for your child?
  mnp2 <- ifelse(
    mnp_df$mnp2 == "2", 0,
    ifelse(
      mnp_df$mnp2 == "99", NA, 1
    )
  )

  ## Why did you not receive/buy MNP?
  
  # mnp2a_1: Health centre ran out
  mnp2a_1 <- mnp_df$mnp2a_1
  mnp2a_1[mnp_df$mnp2a_other == "Not Available"] <- 1
  
  # mnp2a_2: Took too long to get MNP
  mnp2a_2 <- mnp_df$mnp2a_2
  
  # mnp2a_3: Too expensive
  mnp2a_3 <- mnp_df$mnp2a_3
  
  # mnp2a_4: Child doesn't need it
  mnp2a_4 <- mnp_df$mnp2a_4
  mnp2a_4[mnp_df$mnp2a_other == "Didn't ask for it  because her child is nourished "] <- 1
  
  # mnp2a_5: Heard it doesn't work/help
  mnp2a_5 <- mnp_df$mnp2a_5
  
  # mnp2a_6: Didn't ask for it
  mnp2a_6 <- vector(mode = "integer", length = nrow(mnp_df))
  mnp2a_6 <- ifelse(mnp_df$mnp2a_88 == 0, NA, mnp2a_6)
  mnp2a_6[mnp_df$mnp2a_other == "Didn't ask for it"] <- 1
  
  # mnp2a_7: Don't know about MNP
  mnp2a_7 <- vector(mode = "integer", length = nrow(mnp_df))
  mnp2a_7 <- ifelse(mnp_df$mnp2a_88 == 0, NA, mnp2a_7)
  #mnp2a_7[mnp_df$mnp2a_other == ""] <- 1
  mnp2a_7[stringr::str_detect(mnp_df$mnp2a_other, pattern = "hear|know|seen|aware|idea|told")] <- 1
  
  # mnp2a_8: Child not brought to hospital/clinic
  mnp2a_8 <- vector(mode = "integer", length = nrow(mnp_df))
  mnp2a_8 <- ifelse(mnp_df$mnp2a_88 == 0, NA, mnp2a_8)
  mnp2a_8[mnp_df$mnp2a_other == "Haven't taken child for treatment "] <- 1
  mnp2a_8[mnp_df$mnp2a_other == "Mother didn't go for it"] <- 1
  
  # mnp2a_9: Don't remember reason
  mnp2a_9 <- vector(mode = "integer", length = nrow(mnp_df))
  mnp2a_9 <- ifelse(mnp_df$mnp2a_88 == 0, NA, mnp2a_9)
  mnp2a_9[mnp_df$mnp2a_other == "I don't remember "] <- 1
  
  # mnp2a_10: Child not given during last visit to clinic/hospital
  mnp2a_10 <- vector(mode = "integer", length = nrow(mnp_df))
  mnp2a_10 <- ifelse(mnp_df$mnp2a_88 == 0, NA, mnp2a_10)
  mnp2a_10[mnp_df$mnp2a_other == "Mother took the child to Phebe Hospital before coming to Monrovia but nothing was given,"] <- 1
  mnp2a_10[mnp_df$mnp2a_other == "Never prescribed it for Francis"] <- 1
  mnp2a_10[mnp_df$mnp2a_other == "No one give me some "] <- 1
  mnp2a_10[mnp_df$mnp2a_other == "The facility did not give it to us"] <- 1
  mnp2a_10[mnp_df$mnp2a_other == "The hospital didn't offer some"] <- 1
  mnp2a_10[mnp_df$mnp2a_other == "The hospital never recommended it and she does  not know why"] <- 1
  mnp2a_10[mnp_df$mnp2a_other == "They have not prescribed  it for my baby "] <- 1

  # mnp2a_11: No access to health facility
  mnp2a_11 <- vector(mode = "integer", length = nrow(mnp_df))
  mnp2a_11 <- ifelse(mnp_df$mnp2a_88 == 0, NA, mnp2a_11)
  mnp2a_11[mnp_df$mnp2a_other == "No access to health facility "] <- 1
  
  # mnp2a_12: Clinic/hospital too far
  mnp2a_12 <- vector(mode = "integer", length = nrow(mnp_df))
  mnp2a_12 <- ifelse(mnp_df$mnp2a_88 == 0, NA, mnp2a_12)
  mnp2a_12[mnp_df$mnp2a_other == "She didn't go to the Health facility after deliver cause the place is far"] <- 1
  
  ## mnp3: Did you give MNP to child?
  mnp3 <- ifelse(
    mnp_df$mnp3 == "2" | mnp2 == 0, 0,
    ifelse(
      mnp_df$mnp3 == "99", NA, 1
    )
  )
  
  ## Why did you not give the MNP to child?
  
  # mnp3a_1: Too expensive
  #mnp3a_1 <- mnp_df$mnp3a_1
  mnp3a_1 <- NA
  
  # mnp3a_2: Not available in the market
  #mnp3a_2 <- mnp_df$mnp3a_2
  mnp3a_2 <- NA
  
  # mnp3a_3: Do not need MNP
  #mnp3a_3 <- mnp_df$mnp3a_3
  mnp3a_3 <- NA
  
  # mnp3a_4: Heard other's bad experience
  mnp3a_4 <- mnp_df$mnp3a_4
  
  # mnp3a_5: Advised not to use it
  #mnp3a_5 <- mnp_df$mnp3a_5
  mnp3a_5 <- NA
  
  # mnp3a_6: Not seen other mother's use it
  #mnp3a_6 <- mnp_df$mnp3a_6
  mnp3a_6 <- NA
  
  # mnp3a_7: Don't trust the product
  #mnp3a_7 <- mnp_df$mnp3a_7
  mnp3a_7 <- NA
  
  # mnp3a_8: Using another product
  #mnp3a_8 <- mnp_df$mnp3a_7
  mnp3a_8 <- NA
  
  # mnp3a_9: Haven't seen it
  #mnp3a_9 <- mnp_df$mnp3a_9
  mnp3a_9 <- NA
  
  ## mnp4: How many times did you give MNP
  mnp4 <- ifelse(mnp_df$mnp4 < 7 | mnp3 == 0, 0, 1)

  mnp_df <- data.frame(
    mnp_df[ , c("cid", "did", "eid", "longitude", "latitude")],
    mnp1, mnp2, mnp2a_1, mnp2a_2, mnp2a_3, mnp2a_4, mnp2a_5, mnp2a_6, mnp2a_7,
    mnp2a_8, mnp2a_9, mnp2a_10, mnp2a_11, mnp2a_12,
    mnp3, mnp3a_1, mnp3a_2, mnp3a_3, mnp3a_4, mnp3a_5, mnp3a_6, mnp3a_7,
    mnp3a_8, mnp3a_9, mnp4
  )
  
  mnp_df
}



