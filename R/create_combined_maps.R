################################################################################
#
#'
#' Create interpolation layers 
#'
#
################################################################################

create_int_maps <- function(int_grid, ea_map, screening_int, cmam_int) {
  int_grid <- sf::st_as_sf(int_grid)
  
  int_grid_centroid <- sf::st_centroid(int_grid)
  
  int_grid_within <- sf::st_join(
    int_grid_centroid, ea_map, join = sf::st_within
  )
  
  int_grid |>
    subset(select = -county) |>
    cbind(sf::st_drop_geometry(int_grid_within)) |>
    cbind(screening_int) |>
    cbind(cmam_int[ , c("case_finding", "treatment")])
}

