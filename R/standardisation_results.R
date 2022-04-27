# ## Read data
# std_data <- read.csv("data/standardisation_test_data.csv")
# 
# ## Calculate the summary metrics of measurements per child (i.e., summary
# ## measures of measurements made per child by each enumerator)
# std_data_summary <- std_data |>
#   dplyr::select(-round) |>
#   dplyr::group_by(child_id) |>
#   dplyr::summarise(
#     mean_height = mean(height),
#     median_height = median(height),
#     sd_height = sd(height),
#     iqr_height = IQR(height),
#     mean_muac = mean(muac),
#     median_muac = median(muac),
#     sd_muac = sd(muac),
#     iqr_muac = IQR(muac)
#   )
# 
# 
# ##
# ## Boxplots of the measurements per child
# ##
# boxplot(height ~ child_id, data = std_data)
# boxplot(muac ~ child_id, data = std_data)
# 
# 
# ## Get outliers per child for height and MUAC
# xx <- std_data |>
#   dplyr::group_by(child_id) |>
#   dplyr::summarise(
#     height_outliers = nipnTK::outliersUV(height),
#     muac_outliers = nipnTK::outliersUV(muac)
#   )
# 
# ## Height measurement outliers
# height_outliers_table <- std_data[xx$height_outliers, ] |>
#   (\(x) data.frame(table(x$enumerator_id)))() |>
#   (\(x) { names(x) <- c("enumerator_id", "n_outliers"); x })()
# 
# ## MUAC measurement outliers
# muac_outliers_table <- std_data[xx$muac_outliers, ] |>
#   (\(x) data.frame(table(x$enumerator_id)))() |>
#   (\(x) { names(x) <- c("enumerator_id", "n_outliers"); x })()
# 
# 
# ## Bias (against mean measurement)
# measurement_bias <- dplyr::left_join(
#   std_data, 
#   std_data_summary |> 
#     subset(select = c(child_id, mean_height, median_height, mean_muac, median_muac)), 
#   by = "child_id"
# ) |>
#   dplyr::mutate(
#     bias_height = abs(height - median_height),
#     bias_muac = abs(muac - median_muac)
#   ) |>
#   dplyr::group_by(enumerator_id) |>
#   dplyr::summarise(
#     mean_bias_height = mean(bias_height),
#     mean_bias_height_class = cut(
#       mean_bias_height, 
#       breaks = c(0, 0.4, 0.6, 1.4, Inf),
#       include.lowest = TRUE, right = FALSE, 
#       labels = c("good", "acceptable", "poor", "rejected")
#     ),
#     mean_bias_muac = mean(bias_muac),
#     mean_bias_muac_class = cut(
#       mean_bias_muac,
#       breaks = c(0, 0.1, 0.2, 0.3, Inf),
#       include.lowest = TRUE, right = FALSE,
#       labels = c("good", "acceptable", "poor", "rejected")
#     )
#   )
# 
# 
# ## Test height first
# height_by_enumerator <- std_data |>
#   dplyr::select(-round, -muac) |>
#   tidyr::pivot_wider(
#     names_from = enumerator_id,
#     values_from = height)
# 
# ## Calculate team TEM for height
# height_team_tem <- height_by_enumerator |>
#   dplyr::select(-child_id) |>
#   data.frame() |>
#   (\(x) anthrocheckr::calculate_team_tem(n = 10, k = 24, m = x))()
# 
# ##
# height_team_tem
# 
# 
# ## Test MUAC
# muac_by_enumerator <- std_data |>
#   dplyr::select(-round, -height) |>
#   tidyr::pivot_wider(
#     names_from = enumerator_id,
#     values_from = muac)
# 
# ## Calculate team TEM for height
# muac_team_tem <- muac_by_enumerator |>
#   dplyr::select(-child_id) |>
#   data.frame() |>
#   (\(x) anthrocheckr::calculate_team_tem(n = 10, k = 24, m = x))()
# 
# ##
# muac_team_tem
# 
#   