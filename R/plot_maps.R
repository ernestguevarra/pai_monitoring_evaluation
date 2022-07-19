################################################################################
#
#'
#' Load targets
#'
#
################################################################################

# targets::tar_load(
#   c(lbr_counties, lbr_districts, lbr_clans, 
#     grand_bassa_ea, grand_bassa_sample_map, grand_bassa_sample,
#     urban_montserrado, urban_montserrado_ea, 
#     urban_montserrado_sample_map, urban_montserrado_sample)
# )


################################################################################
#
#'
#' Create map objects
#'
#
################################################################################

# gb_districts <- lbr_districts |>
#   subset(admin1Name == "Grand Bassa")
# 
# gb_map_spid <- grand_bassa_sample_map |>
#   dplyr::left_join(grand_bassa_sample |> 
#                      subset(select = c(spid, EFEACODE)), 
#                    by = "EFEACODE")
#  
# gm_map_spid <- urban_montserrado_sample_map |>
#   dplyr::left_join(urban_montserrado_sample |> 
#                      subset(select = c(spid, EFEACODE)), 
#                    by = "EFEACODE")

################################################################################
#
#'
#' Plot Grand Bassa sampling area map
#'
#
################################################################################

# png(
#   filename = "outputs/grand_bassa_sample_map.png",
#   width = 10, height = 10, units = "in",
#   res = 200
# )
# par(mar = c(0,0,0,0))
# plot(sf::st_geometry(grand_bassa_ea), lwd = 0.5, border = "gray70")
# plot(sf::st_geometry(gb_districts), lwd = 3, border = "gray50", add = TRUE)
# plot(sf::st_geometry(gb_map_spid), col = "gray90", add = TRUE)
# text(
#   as_Spatial(sf::st_centroid(gb_map_spid)), 
#   labels = gb_map_spid$spid, cex = 0.75, col = "red"
# )
# text(
#   as_Spatial(sf::st_centroid(gb_districts)), 
#   labels = gb_districts$admin2Name, cex = 1, col = "gray70"
# )
# dev.off()
# 
# png(
#   filename = "outputs/urban_montserrado_sample_map.png",
#   width = 10, height = 10, units = "in",
#   res = 200
# )
# par(mar = c(0,0,0,0))
# plot(sf::st_geometry(urban_montserrado_ea), lwd = 0.5, border = "gray70")
# plot(sf::st_geometry(gm_map_spid), col = "gray90", add = TRUE)
# text(
#   as_Spatial(sf::st_centroid(gm_map_spid)), 
#   labels = gm_map_spid$spid, cex = 0.75, col = "red"
# )
# plot(sf::st_geometry(urban_montserrado), lwd = 1.5, add = TRUE)
# dev.off()



#0904009022 <- 0904009052
#0904006012 <- 0904009042


#0906011022 <- 0904008032
#0906011012 <- 0904008012


plot_interpolation <- function(interpolation, base_grid, base_map, var, col_palette) {
  sp::plot(
    base_grid, 
    col = col_palette[(interpolation[[var]] * 100) + 1], 
    border = col_palette[(interpolation[[var]] * 100) + 1]
  )
  plot(sf::st_geometry(base_map), add = TRUE)
}
