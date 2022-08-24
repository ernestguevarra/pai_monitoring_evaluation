################################################################################
#
#'
#' Recode reasons for non-coverage - ICF
#'
#
################################################################################

recode_icf_factors <- function(urban_montserrado_icf_df,
                               grand_bassa_icf_df) {
  ## Factor 1: Timing was not convenient
  gm_factor1 <- sum(urban_montserrado_icf_df$icf2a_1, na.rm = TRUE)
  gb_factor1 <- sum(grand_bassa_icf_df$icf2a_1, na.rm = TRUE)
  
  ## Factor 2: Not interested
  gm_factor2 <- sum(urban_montserrado_icf_df$icf2a_2, na.rm = TRUE)
  gb_factor2 <- sum(grand_bassa_icf_df$icf2a_2, na.rm = TRUE)
  
  ## Factor 3: Do not trust counsellor
  gm_factor3 <- sum(urban_montserrado_icf_df$icf2a_3, na.rm = TRUE)
  gb_factor3 <- sum(grand_bassa_icf_df$icf2a_3, na.rm = TRUE)
  
  ## Factor 4: Don't think I need it
  gm_factor4 <- sum(urban_montserrado_icf_df$icf2a_4, na.rm = TRUE)
  gb_factor4 <- sum(grand_bassa_icf_df$icf2a_4, na.rm = TRUE)

  ## Factor 5: Arrive late to sessions
  gm_factor5 <- sum(urban_montserrado_icf_df$icf2a_5, na.rm = TRUE)
  gb_factor5 <- sum(grand_bassa_icf_df$icf2a_5, na.rm = TRUE)
  
  ## Factor 6: Clinic/hospital too far away
  gm_factor6 <- sum(urban_montserrado_icf_df$icf2a_6, na.rm = TRUE)
  gb_factor6 <- sum(grand_bassa_icf_df$icf2a_6, na.rm = TRUE)
  
  ## Factor 7: Counselling not offered at facility
  gm_factor7 <- sum(urban_montserrado_icf_df$icf2a_7, na.rm = TRUE)
  gb_factor7 <- sum(grand_bassa_icf_df$icf2a_7, na.rm = TRUE)

  ## Factor 8: Other
  gm_factor8 <- sum(urban_montserrado_icf_df$icf2a_8, na.rm = TRUE)
  gb_factor8 <- sum(grand_bassa_icf_df$icf2a_8, na.rm = TRUE)
  
  ## Factor 9: Don't know about counselling sessions
  gm_factor9 <- sum(urban_montserrado_icf_df$icf2a_9, na.rm = TRUE)
  gb_factor9 <- sum(grand_bassa_icf_df$icf2a_9, na.rm = TRUE)
  
  ## Factor 10: Don't remember
  gm_factor10 <- sum(urban_montserrado_icf_df$icf2a_10, na.rm = TRUE)
  gb_factor10 <- sum(grand_bassa_icf_df$icf2a_10, na.rm = TRUE)
  
  ## Factor 11: Don't think they are any good
  gm_factor11 <- sum(urban_montserrado_icf_df$icf2a_11, na.rm = TRUE)
  gb_factor11 <- sum(grand_bassa_icf_df$icf2a_11, na.rm = TRUE)
  
  ## Factor 12: Did not go to the hospital
  gm_factor12 <- sum(urban_montserrado_icf_df$icf2a_12, na.rm = TRUE)
  gb_factor12 <- sum(grand_bassa_icf_df$icf2a_12, na.rm = TRUE)
  
  out_reasons <- c(
    "Timing was not convenient",
    "Not interested",
    "Do not trust counsellor",
    "Don't think I need it",
    "Arrive late to sessions",
    "Clinic/hospital too far away",
    "Counselling not offered at facility",
    "Other",
    "Don't know about counselling sessions",
    "Don't remember",
    "Don't think they are any good",
    "Did not go to the hospital"
  )
  
  gm_reasons <- data.frame(
    survey_area = "Urban Montserrado",
    n = c(
      gm_factor1, gm_factor2, gm_factor3, gm_factor4, gm_factor5,
      gm_factor6, gm_factor7, gm_factor8, gm_factor9, gm_factor10,
      gm_factor11, gm_factor12
    )
  )
  
  gb_reasons <- data.frame(
    survey_area = "Grand Bassa",
    n = c(
      gb_factor1, gb_factor2, gb_factor3, gb_factor4, gb_factor5,
      gb_factor6, gb_factor7, gb_factor8, gb_factor9, gb_factor10,
      gb_factor11, gb_factor12
    )
  )
  
  icf_factors_df <- rbind(
    data.frame(
      out_reasons, gm_reasons
    ),
    data.frame(
      out_reasons, gb_reasons
    )
  )
  
  icf_factors_df
}


