################################################################################
#
#'
#' Recode reasons for non-coverage - vitamin A
#'
#
################################################################################

recode_vita_factors <- function(urban_montserrado_vita_df,
                                grand_bassa_vita_df) {
  ## Factor 1: Health centre ran out
  gm_factor1 <- sum(urban_montserrado_vita_df$vit1a_1, na.rm = TRUE)
  gb_factor1 <- sum(grand_bassa_vita_df$vit1a_1, na.rm = TRUE)
  
  ## Factor 2: Took too long to get drops
  gm_factor2 <- sum(urban_montserrado_vita_df$vit1a_2, na.rm = TRUE)
  gb_factor2 <- sum(grand_bassa_vita_df$vit1a_2, na.rm = TRUE)
  
  ## Factor 3: Child doesn't need it
  gm_factor3 <- sum(urban_montserrado_vita_df$vit1a_3, na.rm = TRUE)
  gb_factor3 <- sum(grand_bassa_vita_df$vit1a_3, na.rm = TRUE)
  
  ## Factor 4: Heard it doesn't work/help
  gm_factor4 <- sum(urban_montserrado_vita_df$vit1a_4, na.rm = TRUE)
  gb_factor4 <- sum(grand_bassa_vita_df$vit1a_4, na.rm = TRUE)

  ## Factor 5: Child was sick
  gm_factor5 <- sum(urban_montserrado_vita_df$vit1a_5, na.rm = TRUE)
  gb_factor5 <- sum(grand_bassa_vita_df$vit1a_5, na.rm = TRUE)
  
  ## Factor 6: Child not given in latest visit to hospital/clinic
  gm_factor6 <- sum(urban_montserrado_vita_df$vit1a_6, na.rm = TRUE)
  gb_factor6 <- sum(grand_bassa_vita_df$vit1a_6, na.rm = TRUE)
  
  ## Factor 7: Mother was told child not eligible
  gm_factor7 <- sum(urban_montserrado_vita_df$vit1a_7, na.rm = TRUE)
  gb_factor7 <- sum(grand_bassa_vita_df$vit1a_7, na.rm = TRUE)

  ## Factor 8: Don't remember reason/Don't know why
  gm_factor8 <- sum(urban_montserrado_vita_df$vit1a_8, na.rm = TRUE)
  gb_factor8 <- sum(grand_bassa_vita_df$vit1a_8, na.rm = TRUE)
  
  ## Factor 9: Child was in school or not available
  gm_factor9 <- sum(urban_montserrado_vita_df$vit1a_9, na.rm = TRUE)
  gb_factor9 <- sum(grand_bassa_vita_df$vit1a_9, na.rm = TRUE)
  
  ## Factor 10: Child was not brought to hospital/clinic
  gm_factor10 <- sum(urban_montserrado_vita_df$vit1a_10, na.rm = TRUE)
  gb_factor10 <- sum(grand_bassa_vita_df$vit1a_10, na.rm = TRUE)
  
  ## Factor 11: Hospital/clinic too far
  gm_factor11 <- sum(urban_montserrado_vita_df$vit1a_11, na.rm = TRUE)
  gb_factor11 <- sum(grand_bassa_vita_df$vit1a_11, na.rm = TRUE)
  
  ## Factor 12: Didn't hear/don't know about vitamin A supplementation
  gm_factor12 <- sum(urban_montserrado_vita_df$vit1a_12, na.rm = TRUE)
  gb_factor12 <- sum(grand_bassa_vita_df$vit1a_12, na.rm = TRUE)
  
  ## Factor 13: Didn't know child is supposed to take it again
  gm_factor13 <- sum(urban_montserrado_vita_df$vit1a_13, na.rm = TRUE)
  gb_factor13 <- sum(grand_bassa_vita_df$vit1a_13, na.rm = TRUE)
  
  ## Factor 14: Mother too busy
  gm_factor14 <- sum(urban_montserrado_vita_df$vit1a_14, na.rm = TRUE)
  gb_factor14 <- sum(grand_bassa_vita_df$vit1a_14, na.rm = TRUE)
  
  ## Factor 15: Supplements not brought to beneficiaries
  gm_factor15 <- sum(urban_montserrado_vita_df$vit1a_15, na.rm = TRUE)
  gb_factor15 <- sum(grand_bassa_vita_df$vit1a_15, na.rm = TRUE)
  
  out_reasons <- c(
    "Health centre ran out",
    "Took too long to get drops",
    "Child doesn't need it",
    "Heard it doesn't work/help",
    "Child was sick",
    "Child not given in latest visit to hospital/clinic",
    "Mother was told child not eligible",
    "Don't remember reason/Don't know why",
    "Child was in school or not available",
    "Child was not brought to hospital/clinic",
    "Hospital/clinic too far",
    "Didn't hear/don't know about vitamin A supplementation",
    "Didn't know child is supposed to take it again",
    "Mother too busy",
    "Supplements not brought to beneficiaries"
  )
  
  gm_reasons <- data.frame(
    survey_area = "Urban Montserrado",
    n = c(
      gm_factor1, gm_factor2, gm_factor3, gm_factor4, gm_factor5,
      gm_factor6, gm_factor7, gm_factor8, gm_factor9, gm_factor10, gm_factor11,
      gm_factor12, gm_factor13, gm_factor14, gm_factor15
    )
  )
  
  gb_reasons <- data.frame(
    survey_area = "Grand Bassa",
    n = c(
      gb_factor1, gb_factor2, gb_factor3, gb_factor4, gb_factor5,
      gb_factor6, gb_factor7, gb_factor8, gb_factor9, gb_factor10, gb_factor11,
      gb_factor12, gb_factor13, gb_factor14, gb_factor15
    )
  )
  
  vita_factors_df <- rbind(
    data.frame(
      out_reasons, gm_reasons
    ),
    data.frame(
      out_reasons, gb_reasons
    )
  )
  
  vita_factors_df
}


