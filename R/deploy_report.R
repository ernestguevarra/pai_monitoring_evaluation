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

deploy_cmam_dashboard <- function(from = cmam_coverage_dashboard[1],
                                   to = "docs/cmam_coverage_dashboard.html") {
  file.copy(from = from, to = to, overwrite = TRUE)
  
  to
}