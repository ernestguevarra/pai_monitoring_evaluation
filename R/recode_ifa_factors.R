################################################################################
#
#'
#' Recode reasons for non-coverage - IFA
#'
#
################################################################################

recode_ifa_factors <- function(urban_montserrado_ifa_df,
                               grand_bassa_ifa_df) {
  ## Factor 1: Health centre ran out
  gm_factor1 <- sum(urban_montserrado_ifa_df$ifa3a_1, na.rm = TRUE)
  gb_factor1 <- sum(grand_bassa_ifa_df$ifa3a_1, na.rm = TRUE)
  
  ## Factor 2: Took too long to get tablets
  gm_factor2 <- sum(urban_montserrado_ifa_df$ifa3a_2, na.rm = TRUE)
  gb_factor2 <- sum(grand_bassa_ifa_df$ifa3a_2, na.rm = TRUE)
  
  ## Factor 3: Too expensive
  gm_factor3 <- sum(urban_montserrado_ifa_df$ifa3a_3, na.rm = TRUE)
  gb_factor3 <- sum(grand_bassa_ifa_df$ifa3a_3, na.rm = TRUE)
  
  ## Factor 4: Not aware of IFA
  gm_factor4 <- sum(urban_montserrado_ifa_df$ifa3a_4, na.rm = TRUE)
  gb_factor4 <- sum(grand_bassa_ifa_df$ifa3a_4, na.rm = TRUE)

  ## Factor 5: Don't believe in it
  gm_factor5 <- sum(urban_montserrado_ifa_df$ifa3a_5, na.rm = TRUE)
  gb_factor5 <- sum(grand_bassa_ifa_df$ifa3a_5, na.rm = TRUE)
  
  ## Factor 6: Don't like it
  gm_factor6 <- sum(urban_montserrado_ifa_df$ifa3a_6, na.rm = TRUE)
  gb_factor6 <- sum(grand_bassa_ifa_df$ifa3a_6, na.rm = TRUE)
  
  ## Factor 7: Health centre very far
  gm_factor7 <- sum(urban_montserrado_ifa_df$ifa3a_7, na.rm = TRUE)
  gb_factor7 <- sum(grand_bassa_ifa_df$ifa3a_7, na.rm = TRUE)

  ## Factor 8: Didn't go to the hospital/clinic
  gm_factor8 <- sum(urban_montserrado_ifa_df$ifa3a_8, na.rm = TRUE)
  gb_factor8 <- sum(grand_bassa_ifa_df$ifa3a_8, na.rm = TRUE)
  
  ## Factor 9: Don't know
  gm_factor9 <- sum(urban_montserrado_ifa_df$ifa3a_9, na.rm = TRUE)
  gb_factor9 <- sum(grand_bassa_ifa_df$ifa3a_9, na.rm = TRUE)
  
  out_reasons <- c(
    "Health centre ran out",
    "Took too long to get tablets",
    "Too expensive",
    "Not aware of IFA",
    "Don't believe in it",
    "Don't like it",
    "Health centre very far",
    "Didn't go to the hospital/clinic",
    "Don't know"
  )
  
  gm_reasons <- data.frame(
    survey_area = "Urban Montserrado",
    n = c(
      gm_factor1, gm_factor2, gm_factor3, gm_factor4, gm_factor5,
      gm_factor6, gm_factor7, gm_factor8, gm_factor9
    )
  )
  
  gb_reasons <- data.frame(
    survey_area = "Grand Bassa",
    n = c(
      gb_factor1, gb_factor2, gb_factor3, gb_factor4, gb_factor5,
      gb_factor6, gb_factor7, gb_factor8, gb_factor9
    )
  )
  
  ifa_factors_df <- rbind(
    data.frame(
      out_reasons, gm_reasons
    ),
    data.frame(
      out_reasons, gb_reasons
    )
  )
  
  ifa_factors_df
}


