################################################################################
#
#'
#' Estimate screening coverage using binom.test
#'
#
################################################################################

estimate_screening_coverage <- function(urban_montserrado_screening_df,
                                        grand_bassa_screening_df) {
  gm_muac_screening <- binom.test(
    x = c(table(urban_montserrado_screening_df[["muac_screen"]])[2],
          table(urban_montserrado_screening_df[["muac_screen"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gm_oedema_screening <- binom.test(
    x = c(table(urban_montserrado_screening_df[["oedema_screen"]])[2],
          table(urban_montserrado_screening_df[["oedema_screen"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gb_muac_screening <- binom.test(
    x = c(table(grand_bassa_screening_df[["muac_screen"]])[2],
          table(grand_bassa_screening_df[["muac_screen"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gb_oedema_screening <- binom.test(
    x = c(table(grand_bassa_screening_df[["oedema_screen"]])[2],
          table(grand_bassa_screening_df[["oedema_screen"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  screen_results <- cbind(
    rbind(gm_muac_screening, gm_oedema_screening),
    rbind(gb_muac_screening, gb_oedema_screening)
  )
  
  screen_results <- data.frame(
    c("MUAC Screening", "Oedema Screening"),
    screen_results
  )
  
  row.names(screen_results) <- NULL
  names(screen_results) <- c("indicators", 
                             "estimate_gm", "lcl_gm", "ucl_gm",
                             "estimate_gb", "lcl_gb", "ucl_gb")
                          
  screen_results
}


################################################################################
#
#'
#' Estimate coverage indicators using binom.test
#'
#
################################################################################

estimate_cmam_coverage <- function(urban_montserrado_cmam_df,
                                   grand_bassa_cmam_df) {
  cov_data <- rbind(
    colSums(
      cbind(
        urban_montserrado_cmam_df$sam_in,
        urban_montserrado_cmam_df$sam_out,
        urban_montserrado_cmam_df$rec_in
      )
    ),
    colSums(
      cbind(
        grand_bassa_cmam_df$sam_in,
        grand_bassa_cmam_df$sam_out,
        grand_bassa_cmam_df$rec_in
      )
    )
  ) |>
    data.frame() |>
    (\(x) { names(x) <- c("sam_in", "sam_out", "rec_in"); x })()
  
  cov_data$rec_out <- calculate_rec_out(
    k = 3, 
    sam_in = cov_data$sam_in, 
    sam_out = cov_data$sam_out, 
    rec_in = cov_data$rec_in
  )
  
  
  gm_case_finding <- binom.test(
    x = c(cov_data$sam_in[1], cov_data$sam_in[1] + cov_data$sam_out[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gb_case_finding <- binom.test(
    x = c(cov_data$sam_in[2], cov_data$sam_in[2] + cov_data$sam_out[2])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gm_treatment <- binom.test(
    x = c(cov_data$sam_in[1] + cov_data$rec_in[1], 
          cov_data$sam_in[1] + cov_data$sam_out[1] + 
            cov_data$rec_in[1] + cov_data$rec_out[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gb_treatment <- binom.test(
    x = c(cov_data$sam_in[2] + cov_data$rec_in[2], 
          cov_data$sam_in[2] + cov_data$sam_out[2] + 
            cov_data$rec_in[2] + cov_data$rec_out[2])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  cmam_results <- rbind(
    c(gm_case_finding, gb_case_finding),
    c(gm_treatment, gb_treatment)
  )
  
  cmam_results <- data.frame(
    indicators = c("Case-finding effectiveness", "Treatment coverage"),
    cmam_results
  )
  
  #row.names(screen_results) <- NULL
  names(cmam_results) <- c("indicators", 
                           "estimate_gm", "lcl_gm", "ucl_gm",
                           "estimate_gb", "lcl_gb", "ucl_gb")
  
  cmam_results
}


################################################################################
#
#'
#' Estimate vitamin A coverage using binom.test
#'
#
################################################################################

estimate_vita_coverage <- function(urban_montserrado_vita_df,
                                   grand_bassa_vita_df) {
  gm_vita_coverage <- binom.test(
    x = c(table(urban_montserrado_vita_df[["vit1"]])[2],
          table(urban_montserrado_vita_df[["vit1"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gb_vita_coverage <- binom.test(
    x = c(table(grand_bassa_vita_df[["vit1"]])[2],
          table(grand_bassa_vita_df[["vit1"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  coverage_results <- data.frame(
    "Survey area" = c("Urban Montserrado", "Grand Bassa"),
    rbind(
      gm_vita_coverage,
      gb_vita_coverage
    )
  )
  
  
  row.names(coverage_results) <- NULL
  names(coverage_results) <- c("survey_area", "estimate", "lcl", "ucl")
  
  coverage_results
}


################################################################################
#
#'
#' Estimate mnp coverage using binom.test
#'
#
################################################################################

estimate_mnp_coverage <- function(urban_montserrado_mnp_df,
                                  grand_bassa_mnp_df) {
  gm_mnp1_coverage <- binom.test(
    x = c(table(urban_montserrado_mnp_df[["mnp1"]])[2],
          table(urban_montserrado_mnp_df[["mnp1"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gb_mnp1_coverage <- binom.test(
    x = c(table(grand_bassa_mnp_df[["mnp1"]])[2],
          table(grand_bassa_mnp_df[["mnp1"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gm_mnp2_coverage <- binom.test(
    x = c(table(urban_montserrado_mnp_df[["mnp2"]])[2],
          table(urban_montserrado_mnp_df[["mnp2"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gb_mnp2_coverage <- binom.test(
    x = c(table(grand_bassa_mnp_df[["mnp2"]])[2],
          table(grand_bassa_mnp_df[["mnp2"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gm_mnp3_coverage <- binom.test(
    x = c(table(urban_montserrado_mnp_df[["mnp3"]])[2],
          table(urban_montserrado_mnp_df[["mnp3"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gb_mnp3_coverage <- binom.test(
    x = c(table(grand_bassa_mnp_df[["mnp3"]])[2],
          table(grand_bassa_mnp_df[["mnp3"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gm_mnp4_coverage <- binom.test(
    x = c(table(urban_montserrado_mnp_df[["mnp4"]])[2],
          table(urban_montserrado_mnp_df[["mnp4"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gb_mnp4_coverage <- binom.test(
    x = c(table(grand_bassa_mnp_df[["mnp4"]])[2],
          table(grand_bassa_mnp_df[["mnp4"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  
  coverage_results <- data.frame(
    "Indicators" = c(
      "Heard of MNP", 
      "Received MNP", 
      "Gave MNP to child", 
      "Gave MNP at least 7 days in past week"
    ),
    rbind(
      c(gm_mnp1_coverage, gb_mnp1_coverage),
      c(gm_mnp2_coverage, gb_mnp2_coverage),
      c(gm_mnp3_coverage, gb_mnp3_coverage),
      c(gm_mnp4_coverage, gb_mnp4_coverage)
    )
  )
  
  row.names(coverage_results) <- NULL
  names(coverage_results) <- c("indicators", 
                               "gm_estimate", "gm_lcl", "gm_ucl", 
                               "gb_estimate", "gb_lcl", "gb_ucl")
  
  coverage_results
}


################################################################################
#
#'
#' Estimate ifa coverage using binom.test
#'
#
################################################################################

estimate_ifa_coverage <- function(urban_montserrado_ifa_df,
                                  grand_bassa_ifa_df) {
  gm_ifa1_coverage <- binom.test(
    x = c(table(urban_montserrado_ifa_df[["ifa1"]])[2],
          table(urban_montserrado_ifa_df[["ifa1"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gb_ifa1_coverage <- binom.test(
    x = c(table(grand_bassa_ifa_df[["ifa1"]])[2],
          table(grand_bassa_ifa_df[["ifa1"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gm_ifa2_coverage <- binom.test(
    x = c(table(urban_montserrado_ifa_df[["ifa2"]])[2],
          table(urban_montserrado_ifa_df[["ifa2"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gb_ifa2_coverage <- binom.test(
    x = c(table(grand_bassa_ifa_df[["ifa2"]])[2],
          table(grand_bassa_ifa_df[["ifa2"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gm_ifa3_coverage <- binom.test(
    x = c(table(urban_montserrado_ifa_df[["ifa3"]])[2],
          table(urban_montserrado_ifa_df[["ifa3"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gb_ifa3_coverage <- binom.test(
    x = c(table(grand_bassa_ifa_df[["ifa3"]])[2],
          table(grand_bassa_ifa_df[["ifa3"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gm_ifa4_coverage <- binom.test(
    x = c(table(urban_montserrado_ifa_df[["ifa4"]])[2],
          table(urban_montserrado_ifa_df[["ifa4"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gb_ifa4_coverage <- binom.test(
    x = c(table(grand_bassa_ifa_df[["ifa4"]])[2],
          table(grand_bassa_ifa_df[["ifa4"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gm_ifa5_coverage <- binom.test(
    x = c(table(urban_montserrado_ifa_df[["ifa5a"]])[2],
          table(urban_montserrado_ifa_df[["ifa5a"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gb_ifa5_coverage <- binom.test(
    x = c(table(grand_bassa_ifa_df[["ifa5a"]])[2],
          table(grand_bassa_ifa_df[["ifa5a"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  
  coverage_results <- data.frame(
    "Indicators" = c(
      "Attended ANC", 
      "Received information on IFA", 
      "Received IFA", 
      "Consumed IFA",
      "Consumed IFA at least 90 days"
    ),
    rbind(
      c(gm_ifa1_coverage, gb_ifa1_coverage),
      c(gm_ifa2_coverage, gb_ifa2_coverage),
      c(gm_ifa3_coverage, gb_ifa3_coverage),
      c(gm_ifa4_coverage, gb_ifa4_coverage),
      c(gm_ifa5_coverage, gb_ifa5_coverage)
    )
  )
  
  row.names(coverage_results) <- NULL
  names(coverage_results) <- c("indicators", 
                               "gm_estimate", "gm_lcl", "gm_ucl", 
                               "gb_estimate", "gb_lcl", "gb_ucl")
  
  coverage_results
}


################################################################################
#
#'
#' Estimate icf coverage using binom.test
#'
#
################################################################################

estimate_icf_coverage <- function(urban_montserrado_icf_df,
                                  grand_bassa_icf_df) {
  gm_icf1_coverage <- binom.test(
    x = c(table(urban_montserrado_icf_df[["icf1"]])[2],
          table(urban_montserrado_icf_df[["icf1"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gb_icf1_coverage <- binom.test(
    x = c(table(grand_bassa_icf_df[["icf1"]])[2],
          table(grand_bassa_icf_df[["icf1"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gm_icf2_coverage <- binom.test(
    x = c(table(urban_montserrado_icf_df[["icf2"]])[2],
          table(urban_montserrado_icf_df[["icf2"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  gb_icf2_coverage <- binom.test(
    x = c(table(grand_bassa_icf_df[["icf2"]])[2],
          table(grand_bassa_icf_df[["icf2"]])[1])
  ) |>
    (\(x) c(x$estimate, x$conf.int))()
  
  coverage_results <- data.frame(
    "Indicators" = c(
      "Heard about IYCF counselling", 
      "Attended IYCF counselling"
    ),
    rbind(
      c(gm_icf1_coverage, gb_icf1_coverage),
      c(gm_icf2_coverage, gb_icf2_coverage)
    )
  )
  
  row.names(coverage_results) <- NULL
  names(coverage_results) <- c("indicators", 
                               "gm_estimate", "gm_lcl", "gm_ucl", 
                               "gb_estimate", "gb_lcl", "gb_ucl")
  
  coverage_results
}
