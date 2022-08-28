################################################################################
#
#'
#' Restructure CMAM estimates to long format
#'
#
################################################################################

lengthen_cmam_estimates <- function(cmam_estimates) {
  est <- cmam_estimates |>
    tidyr::pivot_longer(
      cols = c(estimate_gm, estimate_gb), 
      names_to = "survey_area", 
      names_prefix = "estimate_",
      values_to = "estimate"
    ) |>
    subset(select = c(indicators, survey_area, estimate)) |>
    dplyr::mutate(
      survey_area = ifelse(
        survey_area == "gm", "Urban Montserrado", "Grand Bassa"
      )
    )
  
  lcl <- cmam_estimates |>
    tidyr::pivot_longer(
      cols = c(lcl_gm, lcl_gb),
      names_to = "survey_area",
      names_prefix = "lcl_",
      values_to = "lcl"
    ) |>
    subset(select = c(indicators, survey_area, lcl)) |>
    dplyr::mutate(
      survey_area = ifelse(
        survey_area == "gm", "Urban Montserrado", "Grand Bassa"
      )
    )
  
  ucl <- cmam_estimates |>
    tidyr::pivot_longer(
      cols = c(ucl_gm, ucl_gb),
      names_to = "survey_area",
      names_prefix = "ucl_",
      values_to = "ucl"
    ) |>
    subset(select = c(indicators, survey_area, ucl)) |>
    dplyr::mutate(
      survey_area = ifelse(
        survey_area == "gm", "Urban Montserrado", "Grand Bassa"
      )
    )
  
  cmam_estimates_long <- merge(est, lcl) |> merge(ucl)
  
  cmam_estimates_long
}
