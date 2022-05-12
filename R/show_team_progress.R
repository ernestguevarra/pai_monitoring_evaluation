################################################################################
#
#'
#' Create table for team progress
#'
#
################################################################################

create_table_team_progress <- function(raw_data, ea_assignment, survey_team) {
  x <- ea_assignment |>
    subset(team == paste("Team", survey_team))
  
  y <- raw_data |>
    #subset(team == as.character(survey_team))
    #dplyr::filter(team == as.character(survey_team))
    subset(to_character(team) == paste0("Survey team ", survey_team))

  if (nrow(y) != 0) {
    y <- data.frame(
      table(y$eid)
    ) |>
      (\(x) { names(x) <- c("EFEACODE", "n"); x })()
  
    xy <- merge(
      x, y,
      by = "EFEACODE",
      all.x = TRUE
    ) |>
      (\(x) { x$n <- ifelse(is.na(x$n), 0, x$n); x })()
  } else {
    xy <- x |>
      (\(x) { x$n <- 0; x })()
  }
  
  xy$n <- as.integer(xy$n)
  
  xy
}



################################################################################
#
#'
#' Get median survey time
#'
#
################################################################################

calculate_overall_median_survey_time <- function(raw_data) {
  raw_data |>
    dplyr::mutate(
      start = strptime(start, format = "%Y-%m-%dT%H:%M:%S"),
      end = strptime(end, format = "%Y-%m-%dT%H:%M:%S")
    ) |>
    (\(x) median(x$end - x$start, na.rm = TRUE))()
}


calculate_daily_median_survey_time <- function(raw_data) {
  raw_data |>
    subset(as.Date(today) == Sys.Date()) |>
    dplyr::mutate(
      start = strptime(start, format = "%Y-%m-%dT%H:%M:%S"),
      end = strptime(end, format = "%Y-%m-%dT%H:%M:%S")
    ) |>
    (\(x) median(x$end - x$start, na.rm = TRUE))()
}
