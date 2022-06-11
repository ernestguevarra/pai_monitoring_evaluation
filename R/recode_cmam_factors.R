recode_cmam_factors <- function(urban_montserrado_cmam_factors_df,
                                grand_bassa_cmam_factors_df) {
  ## Factor 1: Does not recognise that child is sick
  gm_factor1 <- ifelse(
    urban_montserrado_cmam_factors_df$q1 == 0, 1, 0
  ) |>
    sum(na.rm = TRUE)
  
  gb_factor1 <- ifelse(
    grand_bassa_cmam_factors_df$q1 == 0, 1, 0
  ) |>
    sum(na.rm = TRUE)
  
  ## Factor 2: Not aware of programme that treats condition
  gm_factor2 <- ifelse(
    urban_montserrado_cmam_factors_df$q3 == 0, 1, 0
  ) |>
    sum(na.rm = TRUE)
  
  gb_factor2 <- ifelse(
    grand_bassa_cmam_factors_df$q3 == 0, 1, 0
  ) |>
    sum(na.rm = TRUE)
  
  ## Factor 3: Child came to facility but not measured
  gm_factor3 <- sum(urban_montserrado_cmam_factors_df$q4a, na.rm = TRUE)
  
  gb_factor3 <- sum(grand_bassa_cmam_factors_df$q4a, na.rm = TRUE)
  
  ## Factor 4: Too far
  gm_factor4 <- sum(urban_montserrado_cmam_factors_df$q4b, na.rm = TRUE)
  
  gb_factor4 <- sum(grand_bassa_cmam_factors_df$q4b, na.rm = TRUE)

  ## Factor 5: Mother/carer has not time/too busy
  gm_factor5 <- sum(urban_montserrado_cmam_factors_df$q4c, na.rm = TRUE)

  gb_factor5 <- sum(grand_bassa_cmam_factors_df$q4c, na.rm = TRUE)
  
  ## Factor 6: Mother/carer is sick
  gm_factor6 <- sum(urban_montserrado_cmam_factors_df$q4d, na.rm = TRUE)
  
  gb_factor6 <- sum(grand_bassa_cmam_factors_df$q4d, na.rm = TRUE)
  
  ## Factor 7: No one else to take care of other siblings
  gm_factor7 <- sum(urban_montserrado_cmam_factors_df$q4h, na.rm = TRUE)
  
  gb_factor7 <- sum(grand_bassa_cmam_factors_df$q4h, na.rm = TRUE)

  ## Factor 8: Child rejected by the programme already
  gm_factor8 <- sum(urban_montserrado_cmam_factors_df$q4i, na.rm = TRUE)
  
  gb_factor8 <- sum(grand_bassa_cmam_factors_df$q4i, na.rm = TRUE)
  
  ## Factor 9: RUTF stock-out
  gm_factor9 <- sum(urban_montserrado_cmam_factors_df$q4p, na.rm = TRUE)
  
  gb_factor9 <- sum(grand_bassa_cmam_factors_df$q4p, na.rm = TRUE)
  
  ## Factor 10: Lack of money for transport to facility
  gm_factor10 <- sum(urban_montserrado_cmam_factors_df$q4q, na.rm = TRUE)
  
  gb_factor10 <- sum(grand_bassa_cmam_factors_df$q4q, na.rm = TRUE)
  
  ## Factor 11: Other
  gm_factor11 <- sum(urban_montserrado_cmam_factors_df$q4u, na.rm = TRUE)
  
  gb_factor11 <- sum(grand_bassa_cmam_factors_df$q4u, na.rm = TRUE)
  
  out_reasons <- c(
    "Mother/carer does not recognise child is sick",
    "Mother/carer not aware of programme",
    "Child brought to facility but not measured",
    "Facility too far",
    "Mother/carer has no time/too busy",
    "Mother/carer is sick",
    "No one else to take care of other siblings",
    "Child rejected by programme",
    "RUTF stock-out",
    "Lack of money for transport to facility",
    "Other"
  )
  
  gm_reasons <- data.frame(
    survey_area = "Urban Montserrado",
    n = c(
      gm_factor1, gm_factor2, gm_factor3, gm_factor4, gm_factor5,
      gm_factor6, gm_factor7, gm_factor8, gm_factor9, gm_factor10, gm_factor11
    )
  )
  
  gb_reasons <- data.frame(
    survey_area = "Grand Bassa",
    n = c(
      gb_factor1, gb_factor2, gb_factor3, gb_factor4, gb_factor5,
      gb_factor6, gb_factor7, gb_factor8, gb_factor9, gb_factor10, gb_factor11
    )
  )
  
  cmam_factors_df <- rbind(
    data.frame(
      out_reasons, gm_reasons
    ),
    data.frame(
      out_reasons, gb_reasons
    )
  )
  
  cmam_factors_df
}


plot_pareto_factors <- function(cmam_factors, main) {
  cmam_factors |>
    (\(x) x[rev(order(x[[2]])), ])() |>
    (\(x) 
      barplot(
        x[[2]], 
        horiz = TRUE, 
        names.arg = x$out_reasons, 
        cex.names = 0.75,
        las = 1,
        main = main,
        xlim = 60
      )
    )()
}


ggplot_pareto_factors <- function(cmam_factors) {
  cmam_factors |>
    mutate(
      survey_area = factor(
        x = survey_area, 
        levels = c("Urban Montserrado", "Grand Bassa")
      )
    ) |>
    ggplot(
      mapping = aes(
        x = n, 
        y = tidytext::reorder_within(out_reasons, -n, survey_area), 
        group = survey_area
      )
    ) +
    geom_col() +
    tidytext::scale_y_reordered() +
    ylab(label = "Reasons for non-coverage") +
    facet_wrap(. ~ survey_area, scales = "free_y") +
    theme_minimal()
}
