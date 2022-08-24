################################################################################
#
#' 
#' Recode vitamin A data
#'
#' @param raw_data Survey raw data  
#' 
#
################################################################################

recode_vita <- function(raw_data) {
  vita_df <- raw_data |>
    subset(
      select = c(
        cid, did, eid, `_geolocation`, vit1, vit1a, vit1a_1, vit1a_2, vit1a_3, 
        vit1a_88, vit1a_other, vit2, vit3
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
  
  vit1 <- ifelse(
    vita_df$vit1 == "2", 0,
    ifelse(
      vita_df$vit1 == "99", NA, 1
    )
  )

  # vit1a_1: Health centre ran out
  vit1a_1 <- vita_df$vit1a_1
  vit1a_1[stringr::str_detect(vita_df$vit1a_other, "Hospital not available")] <- 1
  vit1a_1[stringr::str_detect(vita_df$vit1a_other, "Not adiviable")] <- 1
  vit1a_1[stringr::str_detect(vita_df$vit1a_other, "Not been available")] <- 1
  
  # vit1a_2: Took too long to get drops
  vit1a_2 <- vita_df$vit1a_2
  
  # vit1a_3: Child doesn't need it
  vit1a_3 <- vita_df$vit1a_3
  vit1a_3[vita_df$vit1a_other == "Because she's not sick"] <- 1
  vit1a_3[vita_df$vit1a_other == "Child wasn't sick to go to the hospital "] <- 1
  
  
  # vit1a_4: Heard it doesn't work/help
  vit1a_4 <- vita_df$vit1a_4
  if (is.null(vit1a_4)) vit1a_4 <- NA
  
  # vit1a_5: Child was sick
  vit1a_5 <- vector(mode = "integer", length = nrow(vita_df))
  vit1a_5 <- ifelse(vita_df$vit1a_88 == 0, NA, vit1a_5)
  vit1a_5[stringr::str_detect(vita_df$vit1a_other, "coma")] <- 1
  
  # vit1a_6: Child not given in latest visit to hospital/clinic
  vit1a_6 <- vector(mode = "integer", length = nrow(vita_df))
  vit1a_6 <- ifelse(vita_df$vit1a_88 == 0, NA, vit1a_6)
  vit1a_6[stringr::str_detect(vita_df$vit1a_other, pattern = "\\*")] <- 1
  vit1a_6[vita_df$vit1a_other == "Clinic didn't give us it"] <- 1
  vit1a_6[vita_df$vit1a_other == "Haven't receive it in any clinic or hospital "] <- 1
  vit1a_6[vita_df$vit1a_other == "Health workers didn't give it"] <- 1
  vit1a_6[vita_df$vit1a_other == "Did not receive it"] <- 1
  vit1a_6[vita_df$vit1a_other == "Don't receive it from the company clinic yet"] <- 1
  vit1a_6[vita_df$vit1a_other == "I didn't receive it"] <- 1
  vit1a_6[vita_df$vit1a_other == "I didn't see it neither receive it"] <- 1
  vit1a_6[vita_df$vit1a_other == "I dint see it neither receive it."] <- 1
  vit1a_6[vita_df$vit1a_other == "I don't remember seeing or receiving this "] <- 1
  vit1a_6[stringr::str_detect(vita_df$vit1a_other, pattern = "Susanna")] <- 1
  vit1a_6[vita_df$vit1a_other == "No health workers give us some "] <- 1
  vit1a_6[vita_df$vit1a_other == "Not provided "] <- 1
  vit1a_6[vita_df$vit1a_other == "She did not ask and so, she doesn't  know Why."] <- 1
  vit1a_6[vita_df$vit1a_other == "The Company clinic didn't provide it"] <- 1
  vit1a_6[vita_df$vit1a_other == "The medical practitioners never offer it to her son"] <- 1
  vit1a_6[vita_df$vit1a_other == "Was not given to her  at the hospital"] <- 1
  
  # vit1a_7: Mother told child not eligible
  vit1a_7 <- vector(mode = "integer", length = nrow(vita_df))
  vit1a_7 <- ifelse(vita_df$vit1a_88 == 0, NA, vit1a_7)
  vit1a_7[vita_df$vit1a_other == "Age not reach  for vitamin A"] <- 1
  vit1a_7[vita_df$vit1a_other == "Mother did know whether Melvin  should have taken vitamin A  at this age"] <- 1
  
  # vit1a_8: Don't remember reason/Don't know why
  vit1a_8 <- vector(mode = "integer", length = nrow(vita_df))
  vit1a_8 <- ifelse(vita_df$vit1a_88 == 0, NA, vit1a_8)
  vit1a_8[vita_df$vit1a_other == "Been very long"] <- 1
  vit1a_8[vita_df$vit1a_other == "Can't remember"] <- 1
  vit1a_8[vita_df$vit1a_other == "I don't remember "] <- 1
  vit1a_8[vita_df$vit1a_other == "She does not know why "] <- 1
  vit1a_8[vita_df$vit1a_other == "She does not know why. "] <- 1
  vit1a_8[vita_df$vit1a_other == "The mother does not know why."] <- 1
  
  # vit1a_9: Child was in school or not available
  vit1a_9 <- vector(mode = "integer", length = nrow(vita_df))
  vit1a_9 <- ifelse(vita_df$vit1a_88 == 0, NA, vit1a_9)
  vit1a_9[vita_df$vit1a_other == "Child was in school"] <- 1
  vit1a_9[vita_df$vit1a_other == "I was not around doing distribution"] <- 1
  
  # vit1a_10: Child was not brought to hospital/clinic
  vit1a_10 <- vector(mode = "integer", length = nrow(vita_df))
  vit1a_10 <- ifelse(vita_df$vit1a_88 == 0, NA, vit1a_10)
  vit1a_10[vita_df$vit1a_other == "Child was not taken to the facility during the past 6 mon"] <- 1
  vit1a_10[vita_df$vit1a_other == "Did not  go  to  the Hospital. "] <- 1
  vit1a_10[vita_df$vit1a_other == "Did not  go to the Hospital. "] <- 1
  vit1a_10[vita_df$vit1a_other == "Did not take child  for it"] <- 1
  vit1a_10[vita_df$vit1a_other == "Had not been to health facility "] <- 1
  vit1a_10[vita_df$vit1a_other == "Haven't carried the child for it before "] <- 1
  vit1a_10[vita_df$vit1a_other == "Mother  hasn't  been  going  to the  Hospital "] <- 1
  vit1a_10[stringr::str_detect(vita_df$vit1a_other, "Alieu")] <- 1
  vit1a_10[vita_df$vit1a_other == "Mother didn't carry child to hospital  for vitamin A"] <- 1
  vit1a_10[vita_df$vit1a_other == "Mother didn't carry the child to the clinic for vitamin A"] <- 1
  vit1a_10[vita_df$vit1a_other == "Mother didn't take child to hospital "] <- 1
  vit1a_10[vita_df$vit1a_other == "Mother didn't visit facility"] <- 1
  vit1a_10[vita_df$vit1a_other == "Mother does not carry her child to the Hospital Regularly. "] <- 1
  vit1a_10[vita_df$vit1a_other == "Mother doesn't  go  to  the  Hospital "] <- 1
  vit1a_10[vita_df$vit1a_other == "Mother hardly takes child to hospital"] <- 1
  vit1a_10[vita_df$vit1a_other == "Mother hasn't  been  taking her child to the Hospital. "] <- 1
  vit1a_10[vita_df$vit1a_other == "Mother was tire  to carry child to hospital for vitamin A"] <- 1
  vit1a_10[vita_df$vit1a_other == "Never been to the  hospital"] <- 1
  vit1a_10[vita_df$vit1a_other == "Never gone to the hospital in the last 6 months"] <- 1
  vit1a_10[vita_df$vit1a_other == "Never taken to health center"] <- 1
  vit1a_10[vita_df$vit1a_other == "Never went to hospital "] <- 1
  vit1a_10[vita_df$vit1a_other == "Never went to the hospital "] <- 1
  vit1a_10[vita_df$vit1a_other == "Never went to the hospital "] <- 1
  vit1a_10[vita_df$vit1a_other == "She didn't  go to the  Hospital. "] <- 1
  vit1a_10[vita_df$vit1a_other == "She has not taken to hospital for long period"] <- 1
  vit1a_10[vita_df$vit1a_other == "She never took him to any health facility "] <- 1
  
  
  # vit1a_11: Hospital/clinic too far
  vit1a_11 <- vector(mode = "integer", length = nrow(vita_df))
  vit1a_11 <- ifelse(vita_df$vit1a_88 == 0, NA, vit1a_11)
  vit1a_11[vita_df$vit1a_other == "Didn't go back to the Clinic  cause the place far"] <- 1
  vit1a_11[vita_df$vit1a_other == "No access to health facility "] <- 1
  
  # vit1a_12: Didn't hear/don't know about vitamin A supplementation
  vit1a_12 <- vector(mode = "integer", length = nrow(vita_df))
  vit1a_12 <- ifelse(vita_df$vit1a_88 == 0, NA, vit1a_12)
  vit1a_12[vita_df$vit1a_other == "Didn't hear about it or see it"] <- 1
  vit1a_12[vita_df$vit1a_other == "Didn't heard about it so I didn't carry my daughter for it"] <- 1
  vit1a_12[vita_df$vit1a_other == "Do know about it"] <- 1
  vit1a_12[vita_df$vit1a_other == "Do not know"] <- 1
  vit1a_12[vita_df$vit1a_other == "Doesn't know about it"] <- 1
  vit1a_12[vita_df$vit1a_other == "Don't  know"] <- 1
  vit1a_12[vita_df$vit1a_other == "Don't  know "] <- 1
  vit1a_12[vita_df$vit1a_other == "Don't know"] <- 1
  vit1a_12[vita_df$vit1a_other == "Don't Know"] <- 1
  vit1a_12[vita_df$vit1a_other == "Don't know "] <- 1
  vit1a_12[vita_df$vit1a_other == "Don't know about it"] <- 1
  vit1a_12[vita_df$vit1a_other == "Don't know about it "] <- 1
  vit1a_12[vita_df$vit1a_other == "Don't know about the medicine"] <- 1
  vit1a_12[vita_df$vit1a_other == "Don't know whether Child should have  taken Vitamin A"] <- 1
  vit1a_12[vita_df$vit1a_other == "Have no idea about it"] <- 1
  vit1a_12[vita_df$vit1a_other == "Have not seen it before "] <- 1
  vit1a_12[vita_df$vit1a_other == "Haven't  see or heard  about it "] <- 1
  vit1a_12[vita_df$vit1a_other == "Have no idea about it"] <- 1
  vit1a_12[vita_df$vit1a_other == "I don't know "] <- 1
  vit1a_12[vita_df$vit1a_other == "I don't know about it"] <- 1
  vit1a_12[vita_df$vit1a_other == "I don't know about such capsule"] <- 1
  vit1a_12[vita_df$vit1a_other == "It is looking strange "] <- 1
  vit1a_12[vita_df$vit1a_other == "It is looking strange "] <- 1
  vit1a_12[vita_df$vit1a_other == "Mother didn't know about them "] <- 1
  vit1a_12[vita_df$vit1a_other == "Mother doesn't know about them"] <- 1
  vit1a_12[vita_df$vit1a_other == "Mother doesn't know about them "] <- 1
  vit1a_12[vita_df$vit1a_other == "Mother has no knowledge about them"] <- 1
  vit1a_12[vita_df$vit1a_other == "Mother is not aware it vitamin A"] <- 1
  vit1a_12[vita_df$vit1a_other == "No knowledge"] <- 1
  vit1a_12[vita_df$vit1a_other == "No knowledge "] <- 1
  vit1a_12[vita_df$vit1a_other == "She did not come across it"] <- 1
  
  # vit1a_13: Didn't know child is supposed to take it again
  vit1a_13 <- vector(mode = "integer", length = nrow(vita_df))
  vit1a_13 <- ifelse(vita_df$vit1a_88 == 0, NA, vit1a_13)
  vit1a_13[stringr::str_detect(vita_df$vit1a_other, "vaccine")] <- 1
  
  # vit1a_14: Mother too busy
  vit1a_14 <- vector(mode = "integer", length = nrow(vita_df))
  vit1a_14 <- ifelse(vita_df$vit1a_88 == 0, NA, vit1a_14)
  vit1a_14[vita_df$vit1a_other == "Mother is too busy "] <- 1
  vit1a_14[vita_df$vit1a_other == "Too busy"] <- 1
  
  # vit1a_15: Supplements not brought to beneficiaries
  vit1a_15 <- vector(mode = "integer", length = nrow(vita_df))
  vit1a_15 <- ifelse(vita_df$vit1a_88 == 0, NA, vit1a_15)
  vit1a_15[vita_df$vit1a_other == "Nobody carry it to them"] <- 1
  vit1a_15[vita_df$vit1a_other == "She claimed that no one pass at her house "] <- 1
  
  # vit1a_11: Other
  # vit1a_11 <- vector(mode = "integer", length = nrow(vita_df))
  # vit1a_11 <- ifelse(vita_df$vit1a_88 == 0, NA, vit1a_11)
  # vit1a_11[vita_df$vit1a_other == "Did not receive it"] <- 1
  # vit1a_11[vita_df$vit1a_other == "Don't receive it from the company clinic yet"] <- 1
  # vit1a_11[vita_df$vit1a_other == "I didn't receive it"] <- 1
  
  
  vita_df <- data.frame(
    vita_df[ , c("cid", "did", "eid", "longitude", "latitude")],
    vit1, vit1a_1, vit1a_2, vit1a_3, vit1a_4, vit1a_5, vit1a_6, vit1a_7,
    vit1a_8, vit1a_9, vit1a_10, vit1a_11, vit1a_12, vit1a_13, vit1a_14, vit1a_15
  )
  
  vita_df
}



