################################################################################
#
# Project build script
#
################################################################################

# Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)


# Set build options ------------------------------------------------------------



# Groups of targets ------------------------------------------------------------

## Sampling
spatial_sample <- tar_plan(
  lbr_counties = liberia::counties,
  lbr_districts = liberia::districts,
  lbr_clans = liberia::clans,
  urban_montserrado = lbr_districts |> 
    subset(admin2Name == "Greater Monrovia"),
  grand_bassa = lbr_counties |>
    subset(admin1name == "Grand Bassa"),
  urban_montserrado_ea = liberia::greaterMonroviaEA,
  grand_bassa_ea = liberia::grandBassaEA,
  urban_montserrado_ea_centroids = urban_montserrado_ea |>
    sf::st_centroid(),
  grand_bassa_ea_centroids = grand_bassa_ea |>
    sf::st_centroid(),
  urban_montserrado_sp = urban_montserrado |>
    sf::st_buffer(dist = 500) |>
    sf::as_Spatial() |>
    spatialsampler::create_sp_grid(
      n = 30, fixed = TRUE, n.factor = 10, country = "Liberia"
    ),
  grand_bassa_sp = grand_bassa |>
    sf::st_buffer(dist = 5000) |> 
    sf::as_Spatial() |>
    spatialsampler::create_sp_grid(
      n = 30, fixed = TRUE, n.factor = 10, country = "Liberia"
    ),
  urban_montserrado_grid = sp::HexPoints2SpatialPolygons(urban_montserrado_sp),
  grand_bassa_grid = sp::HexPoints2SpatialPolygons(grand_bassa_sp),
  urban_montserrado_sample = urban_montserrado_ea_centroids |> 
    (\(x) cbind(data.frame(x), sf::st_coordinates(x)))() |>
    spatialsampler::get_nearest_point(
      data.x = "X", data.y = "Y", query = urban_montserrado_sp, n = 3
    ),
  urban_montserrado_sample_map = urban_montserrado_ea |> 
    subset(EFEACODE %in% urban_montserrado_sample$EFEACODE),
  grand_bassa_sample = grand_bassa_ea_centroids |>
    (\(x) cbind(data.frame(x), sf::st_coordinates(x)))() |>
    spatialsampler::get_nearest_point(
      data.x = "X", data.y = "Y", query = grand_bassa_sp, n = 3
    ),
  grand_bassa_sample_map = grand_bassa_ea |> 
    subset(EFEACODE %in% grand_bassa_sample$EFEACODE),
  urban_montserrado_sample_reformat = urban_montserrado_sample |>
    dplyr::mutate(
      spid = paste0(
        CCODE1, stringr::str_pad(spid, width = 2, side = "left", pad = "0")
      ),
      EFEACODE = stringr::str_pad(
        EFEACODE, width = 10, side = "left", pad = "0"
      ),
      EFEACODE_key = EFEACODE
    ) |>
    dplyr::select(-d, -geometry) |>
    dplyr::rename(longitude = X, latitude = Y),
  grand_bassa_sample_reformat = grand_bassa_sample |>
    dplyr::mutate(
      spid = paste0(
        CCODE1, stringr::str_pad(spid, width = 2, side = "left", pad = "0")
      ),
      EFEACODE = stringr::str_pad(
        EFEACODE, width = 10, side = "left", pad = "0"
      ),
      EFEACODE_key = EFEACODE
    ) |>
    dplyr::select(-d, -geometry) |>
    dplyr::rename(longitude = X, latitude = Y),
  sample_list = rbind(
    urban_montserrado_sample_reformat, grand_bassa_sample_reformat
  ),
  sample_list_odk = sample_list |>
    dplyr::mutate(skip = floor(HH / 10))
)


## Read raw data
data_raw <- tar_plan(
  
)


## Data checks
data_checks <- tar_plan(
  ## Tallies for survey progress
  table_sp_total = tally_sp_total(raw_data, selected_ea_complete),
  table_sp_date_total = tally_sp_date_total(
    raw_data, selected_ea_complete
  ),
  table_team_total = tally_team_total(raw_data),
  table_team_date_total = tally_team_date_total(raw_data),
  check_ea_map = raw_data |>
    subset(!is.na(spid)) |>
    (\(x) x$ea_code)() |>
    unique() |>
    lapply(FUN = check_ea_geo, raw_data, complete_ea_sf),
  table_check_ea_map = raw_data |>
    subset(!is.na(spid)) |>
    (\(x) x$ea_code)() |>
    unique() |>
    lapply(FUN = check_ea, raw_data, complete_ea_sf) |>
    dplyr::bind_rows() |>
    (\(x) rbind(
      x, 
      data.frame(
        ea = "Total", 
        n_in = sum(x[ , 2]),
        n_out = sum(x[ , 3]),
        total = sum(x[ , 4])
      )
    ))(),
  table_check_ea_out = raw_data |>
    subset(!is.na(spid)) |>
    (\(x) x$ea_code)() |>
    unique() |>
    lapply(FUN = check_ea_table, raw_data, complete_ea_sf, check = "out") |>
    dplyr::bind_rows(),
  ## Detect univariate outliers for child anthropometric data
  outlier_weight = raw_data_clean|>
    subset(age_months >= 6 & age < 60) |>
    (\(x) x[outliersUV(x$cweight), ])(),
  outlier_height = raw_data_clean |>
    subset(age_months >= 6 & age_months < 60) |>
    (\(x) x[outliersUV(x$cheight), ])(),
  outlier_muac = raw_data_clean |>
    subset(age_months >= 6 & age_months < 60) |>
    (\(x) x[outliersUV(x$cmuac), ])(),
  outlier_weight_adj = raw_data_clean |>
    subset(age_months >= 6 & age_months < 60) |>
    (\(x) x[outliersUV(x$cweight1), ])(),
  outlier_height_adj = raw_data_clean |>
    subset(age_months >= 6 & age_months < 60) |>
    (\(x) x[outliersUV(x$cheight1), ])(),
  outlier_muac_adj = raw_data_clean |>
    subset(age_months >= 6 & age_months < 60) |>
    (\(x) x[outliersUV(x$cmuac1), ])(),
  outlier_summary_univariate = summarise_univariate_outliers(
    outlier_weight, outlier_height, outlier_muac
  ),
  outlier_table_univariate = tally_univariate_outliers(
    outlier_weight, outlier_height, outlier_muac
  ),
  outlier_unique_univariate_total = tally_unique_univariate_outliers(
    outlier_table_univariate, raw_data_clean
  ),
  outlier_summary_univariate_adj = summarise_univariate_outliers(
    outlier_weight_adj, outlier_height_adj, outlier_muac_adj
  ),
  outlier_table_univariate_adj = tally_univariate_outliers_adj(
    outlier_weight_adj, outlier_height_adj, outlier_muac_adj
  ),
  outlier_unique_univariate_total_adj = tally_unique_univariate_outliers(
    outlier_table_univariate_adj, raw_data_clean
  ),
  ## Detect bivariate outliers for child anthropometric data
  outlier_weight_height = raw_data_clean |>
    subset(age_months >= 6 & age_months < 60) |>
    (\(x) x[with(x, outliersMD(cweight, cheight)), ])() |>
    (\(x) x[!is.na(x$id), ])(),
  outlier_weight_muac = raw_data_clean |>
    subset(age_months >= 6 & age_months < 60) |>
    (\(x) x[with(x, outliersMD(cweight, cmuac)), ])() |>
    (\(x) x[!is.na(x$id), ])(),
  outlier_height_muac = raw_data_clean |>
    subset(age_months >= 6 & age_months < 60) |> 
    (\(x) x[with(x, outliersMD(cheight, cmuac)), ])() |>
    (\(x) x[!is.na(x$id), ])(),
  outlier_weight_age = raw_data_clean |>
    subset(age_months >= 6 & age_months < 60) |>
    (\(x) x[with(x, outliersMD(cweight, age_months)), ])() |>
    (\(x) x[!is.na(x$id), ])(),
  outlier_height_age = raw_data_clean |>
    subset(age_months >= 6 & age_months < 60) |>
    (\(x) x[with(x, outliersMD(cheight, age_months)), ])() |>
    (\(x) x[!is.na(x$id), ])(),
  outlier_muac_age = raw_data_clean |>
    subset(age_months >= 6 & age_months < 60) |>
    (\(x) x[with(x, outliersMD(cmuac, age_months)), ])() |>
    (\(x) x[!is.na(x$id), ])(),
  outlier_summary_bivariate = summarise_bivariate_outliers(
    outlier_weight_height, outlier_weight_muac, outlier_height_muac,
    outlier_weight_age, outlier_height_age, outlier_muac_age
  ),
  outlier_table_bivariate = tally_bivariate_outliers(
    outlier_weight_height, outlier_weight_muac, outlier_height_muac,
    outlier_weight_age, outlier_height_age, outlier_muac_age
  ),
  outlier_unique_bivariate_total = tally_unique_bivariate_outliers(
    outlier_table_bivariate, raw_data_clean
  ),  
  outlier_unique_total = tally_total_unique_outliers(
    outlier_table_bivariate,
    outlier_table_bivariate,
    raw_data_clean
  ),
  ## Flag child anthropometric zscores
  child_data_zscore = raw_data_clean |>
    subset(age_months < 60 & age_months >= 6) |>
    calculate_zscore(),
  child_data_zscore_adj = raw_data_clean |>
    subset(age_months < 60 & age_months >= 6) |>
    calculate_zscore_adj(),
  flag_zscore_total = child_data_zscore |>
    dplyr::filter(flag_zscore != 0) |>
    nrow(),
  flag_zscore_prop = (flag_zscore_total / nrow(child_data_zscore)) |>
    scales::percent(accuracy = 0.1),
  flag_zscore_adj_total = child_data_zscore_adj |>
    dplyr::filter(flag_zscore != 0) |>
    nrow(),
  flag_zscore_adj_prop = (flag_zscore_adj_total / nrow(child_data_zscore_adj)) |>
    scales::percent(accuracy = 0.1),
  ## Test for normality
  shapiro_wfaz = shapiro.test(x = child_data_zscore$wfaz),
  shapiro_hfaz = shapiro.test(x = child_data_zscore$hfaz),
  shapiro_wfhz = shapiro.test(x = child_data_zscore$wfhz),
  ## Test skewness and kurtosis of child anthropometric zscores
  skewKurt_wfaz = child_data_zscore |>
    (\(x) skewKurt(x$wfaz))(),
  skewKurt_hfaz = child_data_zscore |>
    (\(x) skewKurt(x$hfaz))(),
  skewKurt_wfhz = child_data_zscore |>
    (\(x) skewKurt(x$wfhz))(),
  whz_mad = with(
    child_data_zscore |>
      dplyr::filter(!flag_zscore %in% c(2, 3, 6, 7) | !is.na(flag_zscore), 
                    oedema == 2, !is.na(wfhz)),
    mad(wfhz)
  ),
  ## Assess digit preference score
  dp_weight = with(child_data_zscore, digitPreference(cweight)),
  dp_height = with(child_data_zscore, digitPreference(cheight)),
  dp_muac = with(child_data_zscore, digitPreference(cmuac)),
  ## Assess age heaping
  age_heaping = raw_data_clean |>
    (\(x) ageHeaping(x$age_months))(),
  age_heaping_class = classify_age_heaping(age_heaping),
  ## Assess sex ratio
  sex_ratio = raw_data_clean |> 
    (\(x) sexRatioTest(x$sex, codes = c(1, 2), pop = c(1.03, 1)))(),
  ## Assess age ratio
  age_ratio = raw_data_clean |>
    (\(x) x$age_months[x$age_months >= 6 & x$age_months < 60])() |>
    (\(x) x[!is.na(x)])() |>
    (\(x) ageRatioTest(x = x, ratio = 0.85))()
)


## Process data
data_processed <- tar_plan(
  ##
)


## Analysis
analysis <- tar_plan(
  ##
)


## Outputs
outputs <- tar_plan(
  urban_montserrado_sample_csv = write.csv(
    x = urban_montserrado_sample_reformat, 
    file = "outputs/urban_montserrado_sample.csv",
    row.names = FALSE
  ),
  urban_montserrado_sample_xlsx = openxlsx::write.xlsx(
    x = urban_montserrado_sample_reformat, 
    file = "outputs/urban_montserrado_sample.xlsx",
    overwrite = TRUE
  ),
  grand_bassa_sample_csv = write.csv(
    x = grand_bassa_sample_reformat,
    file = "outputs/grand_bassa_sample.csv",
    row.names = FALSE
  ),
  grand_bassa_sample_xlsx = openxlsx::write.xlsx(
    x = grand_bassa_sample_reformat,
    file = "outputs/grand_bassa_sample.xlsx",
    overwrite = TRUE
  ),
  sample_list_csv = write.csv(
    x = sample_list,
    file = "outputs/sample_list.csv",
    row.names = FALSE
  ),
  sample_list_xlsx = openxlsx::write.xlsx(
    x = sample_list,
    file = "outputs/sample_list_xlsx"
  ),
  sample_list_odk_csv = write.csv(
    x = sample_list_odk,
    file = "outputs/sample_list_odk.csv",
    row.names = FALSE
  )
)


## Reports
reports <- tar_plan(
  tar_render(
    name = study_design_report,
    path = "reports/study_design.Rmd",
    output_dir = "docs",
    knit_root_dir = here::here()
  )
)

## Deploy targets
deploy <- tar_plan(
  ##
)

## Set seed
set.seed(1977)

# Concatenate targets ----------------------------------------------------------
list(
  spatial_sample,
  data_raw,
  data_checks,
  data_processed,
  analysis,
  outputs,
  reports,
  deploy
)