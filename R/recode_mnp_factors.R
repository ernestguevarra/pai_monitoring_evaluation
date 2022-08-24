################################################################################
#
#'
#' Recode reasons for non-coverage - MNP
#'
#
################################################################################

recode_mnp_factors <- function(urban_montserrado_mnp_df,
                               grand_bassa_mnp_df) {
  ## Factor 1: Health centre ran out
  gm_factor1 <- sum(urban_montserrado_mnp_df$mnp2a_1, na.rm = TRUE)
  gb_factor1 <- sum(grand_bassa_mnp_df$mnp2a_1, na.rm = TRUE)
  
  ## Factor 2: Took too long to get MNP
  gm_factor2 <- sum(urban_montserrado_mnp_df$mnp2a_2, na.rm = TRUE)
  gb_factor2 <- sum(grand_bassa_mnp_df$mnp2a_2, na.rm = TRUE)
  
  ## Factor 3: too expensive
  gm_factor3 <- sum(urban_montserrado_mnp_df$mnp2a_3, na.rm = TRUE)
  gb_factor3 <- sum(grand_bassa_mnp_df$mnp2a_3, na.rm = TRUE)
  
  ## Factor 4: Child doesn't need it
  gm_factor4 <- sum(urban_montserrado_mnp_df$mnp2a_4, na.rm = TRUE)
  gb_factor4 <- sum(grand_bassa_mnp_df$mnp2a_4, na.rm = TRUE)

  ## Factor 5: Heard it doesn't work/help
  gm_factor5 <- sum(urban_montserrado_mnp_df$mnp2a_5, na.rm = TRUE)
  gb_factor5 <- sum(grand_bassa_mnp_df$mnp2a_5, na.rm = TRUE)
  
  ## Factor 6: Didn't ask for it
  gm_factor6 <- sum(urban_montserrado_mnp_df$mnp2a_6, na.rm = TRUE)
  gb_factor6 <- sum(grand_bassa_mnp_df$mnp2a_6, na.rm = TRUE)
  
  ## Factor 7: Don't know about MNP
  gm_factor7 <- sum(urban_montserrado_mnp_df$mnp2a_7, na.rm = TRUE)
  gb_factor7 <- sum(grand_bassa_mnp_df$mnp2a_7, na.rm = TRUE)

  ## Factor 8: Child  not brought to clinic/hospital
  gm_factor8 <- sum(urban_montserrado_mnp_df$mnp2a_8, na.rm = TRUE)
  gb_factor8 <- sum(grand_bassa_mnp_df$mnp2a_8, na.rm = TRUE)
  
  ## Factor 9: Don't remember reason
  gm_factor9 <- sum(urban_montserrado_mnp_df$mnp2a_9, na.rm = TRUE)
  gb_factor9 <- sum(grand_bassa_mnp_df$mnp2a_9, na.rm = TRUE)
  
  ## Factor 10: Child not given during last visit to clinic/hospital
  gm_factor10 <- sum(urban_montserrado_mnp_df$mnp2a_10, na.rm = TRUE)
  gb_factor10 <- sum(grand_bassa_mnp_df$mnp2a_10, na.rm = TRUE)
  
  ## Factor 11: No access to health facility
  gm_factor11 <- sum(urban_montserrado_mnp_df$mnp2a_11, na.rm = TRUE)
  gb_factor11 <- sum(grand_bassa_mnp_df$mnp2a_11, na.rm = TRUE)
  
  ## Factor 12: Clinic/hospital too far
  gm_factor12 <- sum(urban_montserrado_mnp_df$mnp2a_12, na.rm = TRUE)
  gb_factor12 <- sum(grand_bassa_mnp_df$mnp2a_12, na.rm = TRUE)
  
  out_reasons <- c(
    "Health centre ran out",
    "Took too long to get MNP",
    "Too expensive",
    "Child doesn't need it",
    "Heard it doesn't work/help",
    "Didn't ask for it",
    "Don't know about MNP",
    "Child not brought to hospital/clinic",
    "Don't remember reason",
    "Child not given during last visit to clinic/hospital",
    "No access to health facility",
    "Clinic/hospital too far"
  )
  
  gm_reasons <- data.frame(
    survey_area = "Urban Montserrado",
    n = c(
      gm_factor1, gm_factor2, gm_factor3, gm_factor4, gm_factor5,
      gm_factor6, gm_factor7, gm_factor8, gm_factor9, gm_factor10, gm_factor11,
      gm_factor12
    )
  )
  
  gb_reasons <- data.frame(
    survey_area = "Grand Bassa",
    n = c(
      gb_factor1, gb_factor2, gb_factor3, gb_factor4, gb_factor5,
      gb_factor6, gb_factor7, gb_factor8, gb_factor9, gb_factor10, gb_factor11,
      gb_factor12
    )
  )
  
  mnp_factors_df <- rbind(
    data.frame(
      out_reasons, gm_reasons
    ),
    data.frame(
      out_reasons, gb_reasons
    )
  )
  
  mnp_factors_df
}


