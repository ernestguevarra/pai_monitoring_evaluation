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

