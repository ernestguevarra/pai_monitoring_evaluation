################################################################################
#
#'
#' Get enumeration area populations
#'
#
################################################################################

get_ea_population <- function(ea, raw_data) {
  ea |>
    subset(
      EFEACODE %in% raw_data$eid,
      select = c(EFEACODE, TOTAL)
    ) |>
    sf::st_drop_geometry() |>
    (\(x) { names(x) <- c("psu", "pop"); x })()
}