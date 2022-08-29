################################################################################
#
#'
#' Deploy survey progress report
#'
#
################################################################################

deploy_progress_report <- function(from = survey_progress_report[1],
                                   to = "docs/survey_progress_report.html") {
  file.copy(from = from, to = to, overwrite = TRUE)
  
  to
}


################################################################################
#
#'
#' Deploy survey progress report
#'
#
################################################################################

deploy_final_results <- function(from = final_results_report[1],
                                 to = "docs/final_results_report.html") {
  file.copy(from = from, to = to, overwrite = TRUE)
  
  to
}



################################################################################
#
#'
#' Deploy data quality report
#'
#
################################################################################

deploy_quality_report <- function(from = data_quality_report[1],
                                  to = "docs/data_quality.html") {
  file.copy(from = from, to = to, overwrite = TRUE)
  
  to
}



################################################################################
#
#'
#' Deploy CMAM dashboard
#'
#
################################################################################

deploy_cmam_dashboard <- function(from = c(cmam_coverage_dashboard[1],
                                           urban_montserrado_cmam_dashboard[1],
                                           grand_bassa_cmam_dashboard[1])) {
  to <- file.path("docs", basename(from))
  
  file.copy(from = from, to = to, overwrite = TRUE)
  
  if (!dir.exists("docs/fonts")) {
    dir.create("docs/fonts")
    
    file.copy(
      from = "outputs/fonts",
      to = "docs/fonts",
      recursive = TRUE
    )
  }
  
  to[1]
}

