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


## Survey training data
survey_training <- tar_plan(
  std_data_id = get_googlesheets_id(filename = "standardisation_test_data"), 
  std_data = get_googlesheets(id = std_data_id),
  std_outliers = std_data |>
    dplyr::group_by(child_id) |>
    dplyr::summarise(
      height_outliers = nipnTK::outliersUV(height),
      muac_outliers = nipnTK::outliersUV(muac)
    ),
  pilot_data_id = get_kobo_form_id(
    form_name = "Product Access Initiative Monitoring and Evaluation Survey Form"
  ),
  pilot_data = get_kobo_data(form_id = pilot_data_id),
  pilot_median_interview_time = pilot_data |>
    dplyr::mutate(
      start = strptime(start, format = "%Y-%m-%dT%H:%M:%S"),
      end = strptime(end, format = "%Y-%m-%dT%H:%M:%S")
    ) |>
    (\(x) median(x$end - x$start))(),
  ea_assignment_grand_bassa_file = download_googledrive(
    filename = "grand_bassa_sample.xlsx",
    overwrite = TRUE
  ),
  ea_assignment_grand_bassa = get_ea_assignment_grand_bassa(
    path = ea_assignment_grand_bassa_file$local_path
  ),
  ea_assignment_urban_montserrado_file = download_googledrive(
    filename = "urban_montserrado_sample.xlsx",
    overwrite = TRUE
  ),
  ea_assignment_urban_montserrado = get_ea_assignment_urban_montserrado(
    path = ea_assignment_urban_montserrado_file$local_path
  ),
  ea_assignment = rbind(
    ea_assignment_grand_bassa, ea_assignment_urban_montserrado
  )
)

## Read raw data
data_raw <- tar_plan(
  raw_data_id = get_kobo_form_id(
    form_name = "Product Access Initiative Monitoring and Evaluation Survey Form (MUAC only)"
  ),
  tar_target(
    name = raw_data,
    command = get_kobo_data(form_id = raw_data_id) |>
      subset(as.Date(today) >= as.Date("2022-05-10")) |>
      dplyr::mutate(
        eid = stringr::str_pad(eid, width = 10, side = "left", pad = "0")
      ) |>
      subset(!is.na(team)),
    cue = tar_cue(mode = "thorough")
  )#,
  #raw_data = get_kobo_data(form_id = raw_data_id) |>
  #  subset(as.Date(today) >= as.Date("2022-05-10"))
)


## Data checks
data_checks <- tar_plan(
  team1_progress_table = create_table_team_progress(
    raw_data, ea_assignment, survey_team = 1
  ),
  team2_progress_table = create_table_team_progress(
    raw_data, ea_assignment, survey_team = 2
  ),
  team3_progress_table = create_table_team_progress(
    raw_data, ea_assignment, survey_team = 3
  ),
  team4_progress_table = create_table_team_progress(
    raw_data, ea_assignment, survey_team = 4
  ),
  team5_progress_table = create_table_team_progress(
    raw_data, ea_assignment, survey_team = 5
  ),
  team6_progress_table = create_table_team_progress(
    raw_data, ea_assignment, survey_team = 6
  ),
  team_progress_table = rbind(
    team1_progress_table, team2_progress_table, team3_progress_table,
    team4_progress_table, team5_progress_table, team6_progress_table
  ),
  team1_overall_median_survey_time = calculate_overall_median_survey_time(
    raw_data |> subset(team == "1")
  ),
  team1_daily_median_survey_time = calculate_daily_median_survey_time(
    raw_data |> subset(team == "1")
  ),
  team2_overall_median_survey_time = calculate_overall_median_survey_time(
    raw_data |> subset(team == "2")
  ),
  team2_daily_median_survey_time = calculate_daily_median_survey_time(
    raw_data |> subset(team == "2")
  ),
  team3_overall_median_survey_time = calculate_overall_median_survey_time(
    raw_data |> subset(team == "3")
  ),
  team3_daily_median_survey_time = calculate_daily_median_survey_time(
    raw_data |> subset(team == "3")
  ),
  team4_overall_median_survey_time = calculate_overall_median_survey_time(
    raw_data |> subset(team == "4")
  ),
  team4_daily_median_survey_time = calculate_daily_median_survey_time(
    raw_data |> subset(team == "4")
  ),
  team5_overall_median_survey_time = calculate_overall_median_survey_time(
    raw_data |> subset(team == "5")
  ),
  team5_daily_median_survey_time = calculate_daily_median_survey_time(
    raw_data |> subset(team == "5")
  ),
  team6_overall_median_survey_time = calculate_overall_median_survey_time(
    raw_data |> subset(team == "6")
  ),
  team6_daily_median_survey_time = calculate_daily_median_survey_time(
    raw_data |> subset(team == "6")
  ),
  team_overall_median_survey_time = calculate_overall_median_survey_time(
    raw_data
  ),
  team_daily_median_survey_time = calculate_daily_median_survey_time(
    raw_data
  )
)


## Process data
data_processed <- tar_plan(
  ## Population datasets
  urban_montserrado_ea_population = get_ea_population(
    ea = urban_montserrado_ea, raw_data
  ),
  grand_bassa_ea_population = get_ea_population(
    ea = grand_bassa_ea, raw_data
  ),
  ##
  urban_montserrado_int_grid = rgdal::readOGR(
    dsn = "https://github.com/validmeasures/liberiaData/blob/master/data-raw/maps/gmHexGrid.gpkg?raw=true"
  ),
  urban_montserrado_int_points = create_points(urban_montserrado_int_grid),
  grand_bassa_int_grid = rgdal::readOGR(
    dsn = "https://github.com/validmeasures/liberiaData/blob/master/data-raw/maps/gbHexGrid.gpkg?raw=true"
  ),
  grand_bassa_int_points = create_points(grand_bassa_int_grid),
  urban_montserrado_anthro_df = recode_anthro(raw_data),
  urban_montserrado_anthro_sp = sp::SpatialPointsDataFrame(
    coords = urban_montserrado_anthro_df[ , c("longitude", "latitude")],
    data = urban_montserrado_anthro_df,
    proj4string = CRS(proj4string(urban_montserrado_int_grid))
  ),
  grand_bassa_anthro_df = recode_anthro(raw_data),
  grand_bassa_anthro_sp = sp::SpatialPointsDataFrame(
    coords = grand_bassa_anthro_df[ , c("longitude", "latitude")],
    data = grand_bassa_anthro_df,
    proj4string = CRS(proj4string(grand_bassa_int_grid))
  ),
  urban_montserrado_screening_df = recode_screening(raw_data) |>
    subset(cid == "30"),
  urban_montserrado_screening_sp = sp::SpatialPointsDataFrame(
    coords = urban_montserrado_screening_df[ , c("longitude", "latitude")],
    data = urban_montserrado_screening_df,
    proj4string = CRS(proj4string(urban_montserrado_int_grid))
  ),
  urban_montserrado_cmam_df = recode_cmam(raw_data) |>
    subset(cid == "30") |>
    aggregate_cmam(),
  urban_montserrado_cmam_factors_df = recode_cmam(raw_data) |>
    subset(cid == "30"),
  urban_montserrado_cmam_sp = sp::SpatialPointsDataFrame(
    coords = urban_montserrado_cmam_df[ , c("longitude", "latitude")],
    data = urban_montserrado_cmam_df,
    proj4string = CRS(proj4string(urban_montserrado_int_grid))
  ),
  urban_montserrado_vita_df = recode_vita(raw_data) |>
    subset(cid == "30"),
  urban_montserrado_vita_sp = sp::SpatialPointsDataFrame(
    coords = urban_montserrado_vita_df[ , c("longitude", "latitude")],
    data = urban_montserrado_vita_df,
    proj4string = CRS(proj4string(urban_montserrado_int_grid))
  ),
  urban_montserrado_mnp_df = recode_mnp(raw_data) |>
    subset(cid == "30"),
  urban_montserrado_mnp_sp = sp::SpatialPointsDataFrame(
    coords = urban_montserrado_mnp_df[ , c("longitude", "latitude")],
    data = urban_montserrado_mnp_df,
    proj4string = CRS(proj4string(urban_montserrado_int_grid))
  ),
  grand_bassa_screening_df = recode_screening(raw_data) |>
    subset(cid != "30"),
  grand_bassa_screening_sp = sp::SpatialPointsDataFrame(
    coords = grand_bassa_screening_df[ , c("longitude", "latitude")],
    data = grand_bassa_screening_df,
    proj4string = CRS(proj4string(grand_bassa_int_grid))
  ),
  grand_bassa_cmam_df = recode_cmam(raw_data) |>
    subset(cid != "30") |>
    aggregate_cmam(),
  grand_bassa_cmam_factors_df = recode_cmam(raw_data) |>
    subset(cid != "30"),
  grand_bassa_cmam_sp = sp::SpatialPointsDataFrame(
    coords = grand_bassa_cmam_df[ , c("longitude", "latitude")],
    data = grand_bassa_cmam_df,
    proj4string = CRS(proj4string(grand_bassa_int_grid))
  ),
  grand_bassa_vita_df = recode_vita(raw_data) |>
    subset(cid != "30"),
  grand_bassa_vita_sp = sp::SpatialPointsDataFrame(
    coords = grand_bassa_vita_df[ , c("longitude", "latitude")],
    data = grand_bassa_vita_df,
    proj4string = CRS(proj4string(grand_bassa_int_grid))
  ),
  grand_bassa_mnp_df = recode_mnp(raw_data) |>
    subset(cid != "30"),
  grand_bassa_mnp_sp = sp::SpatialPointsDataFrame(
    coords = grand_bassa_mnp_df[ , c("longitude", "latitude")],
    data = grand_bassa_mnp_df,
    proj4string = CRS(proj4string(grand_bassa_int_grid))
  ),
  urban_montserrado_ifa_df = recode_ifa(raw_data) |>
    subset(cid == "30"),
  urban_montserrado_ifa_sp = sp::SpatialPointsDataFrame(
    coords = urban_montserrado_ifa_df[ , c("longitude", "latitude")],
    data = urban_montserrado_ifa_df,
    proj4string = CRS(proj4string(urban_montserrado_int_grid))
  ),
  grand_bassa_ifa_df = recode_ifa(raw_data) |>
    subset(cid != "30"),
  grand_bassa_ifa_sp = sp::SpatialPointsDataFrame(
    coords = grand_bassa_ifa_df[ , c("longitude", "latitude")],
    data = grand_bassa_ifa_df,
    proj4string = CRS(proj4string(grand_bassa_int_grid))
  ),
  urban_montserrado_icf_df = recode_icf(raw_data) |>
    subset(cid == "30"),
  urban_montserrado_icf_sp = sp::SpatialPointsDataFrame(
    coords = urban_montserrado_icf_df[ , c("longitude", "latitude")],
    data = urban_montserrado_icf_df,
    proj4string = CRS(proj4string(urban_montserrado_int_grid))
  ),
  grand_bassa_icf_df = recode_icf(raw_data) |>
    subset(cid != "30"),
  grand_bassa_icf_sp = sp::SpatialPointsDataFrame(
    coords = grand_bassa_icf_df[ , c("longitude", "latitude")],
    data = grand_bassa_icf_df,
    proj4string = CRS(proj4string(grand_bassa_int_grid))
  )
)


## Analysis
analysis <- tar_plan(
  ## Wasting prevalence
  urban_montserrado_anthro_int = interpolate_anthro(
    anthro_sp = urban_montserrado_anthro_sp,
    point_grid = urban_montserrado_int_points,
    idp = 2
  ),
  grand_bassa_anthro_int = interpolate_anthro(
    anthro_sp = grand_bassa_anthro_sp,
    point_grid = grand_bassa_int_points,
    idp = 2
  ),
  wasting_bootstraps = boot_estimates(
    .data = list(urban_montserrado_anthro_df, grand_bassa_anthro_df),
    w = list(urban_montserrado_ea_population, grand_bassa_ea_population),
    vars = c("gam", "mam", "sam", "oedema"),
    labs = c("Global wasting", "Moderate wasting", "Severe wasting", "Oedema")
  ),
  ## Screening coverage
  urban_montserrado_screening_int = interpolate_screening(
    screening_sp = urban_montserrado_screening_sp,
    point_grid = urban_montserrado_int_points,
    idp = 2
  ),
  grand_bassa_screening_int = interpolate_screening(
    screening_sp = grand_bassa_screening_sp,
    point_grid = grand_bassa_int_points,
    idp = 2
  ),
  screening_estimates = estimate_screening_coverage(
    urban_montserrado_screening_df, grand_bassa_screening_df
  ),
  screening_bootstraps = boot_estimates(
    .data = list(urban_montserrado_screening_df, grand_bassa_screening_df),
    w = list(urban_montserrado_ea_population, grand_bassa_ea_population),
    vars = c("muac_screen", "oedema_screen"),
    labs = c("MUAC screening", "Oedema screening"),
    suffix = c("gm", "gb")
  ),
  ## CMAM coverage
  urban_montserrado_cmam_int = interpolate_cmam(
    cmam_sp = urban_montserrado_cmam_sp,
    point_grid = urban_montserrado_int_points,
    idp = 2
  ),
  grand_bassa_cmam_int = interpolate_cmam(
    cmam_sp = grand_bassa_cmam_sp,
    point_grid = grand_bassa_int_points,
    idp = 2
  ),
  cmam_estimates = estimate_cmam_coverage(
    urban_montserrado_cmam_df, grand_bassa_cmam_df
  ),
  cmam_estimates_long = lengthen_cmam_estimates(cmam_estimates),
  cmam_factors = recode_cmam_factors(
    urban_montserrado_cmam_factors_df, grand_bassa_cmam_factors_df
  ),
  ## Vitamin A
  urban_montserrado_vita_int = interpolate_vita(
    vita_sp = urban_montserrado_vita_sp,
    point_grid = urban_montserrado_int_points,
    idp = 2
  ),
  grand_bassa_vita_int = interpolate_vita(
    vita_sp = grand_bassa_vita_sp,
    point_grid = grand_bassa_int_points,
    idp = 2
  ),
  vita_estimates = estimate_vita_coverage(
    urban_montserrado_vita_df, grand_bassa_vita_df
  ),
  vita_bootstraps = boot_estimate(
    .data = urban_montserrado_vita_df,
    w = urban_montserrado_ea_population,
    vars = "vit1",
    labs = "Urban Montserrado",
    suffix = ""
  ) |>
    rbind(
      boot_estimate(
        .data = grand_bassa_vita_df,
        w = grand_bassa_ea_population,
        vars = "vit1",
        labs = "Grand Bassa",
        suffix = ""
      )
    ) |>
    (\(x) { names(x) <- c("survey_area", "estimate", "lcl", "ucl"); x })(),
  vita_factors = recode_vita_factors(
    urban_montserrado_vita_df, grand_bassa_vita_df
  ),
  ## MNP
  urban_montserrado_mnp_int = interpolate_mnp(
    mnp_sp = urban_montserrado_mnp_sp,
    point_grid = urban_montserrado_int_points,
    idp = 2
  ),
  grand_bassa_mnp_int = interpolate_mnp(
    mnp_sp = grand_bassa_mnp_sp,
    point_grid = grand_bassa_int_points,
    idp = 2
  ),
  mnp_estimates = estimate_mnp_coverage(
    urban_montserrado_mnp_df, grand_bassa_mnp_df
  ),
  mnp_bootstraps = boot_estimates(
    .data = list(urban_montserrado_mnp_df, grand_bassa_mnp_df),
    w = list(urban_montserrado_ea_population, grand_bassa_ea_population),
    vars = c("mnp1", "mnp2", "mnp3", "mnp4"),
    labs = c("Heard of MNP", "Received MNP", "Gave MNP to child", 
             "Gave MNP at least 7 days in past week")
  ),
  mnp_factors = recode_mnp_factors(
    urban_montserrado_mnp_df, grand_bassa_mnp_df
  ),
  ## IFA coverage
  urban_montserrado_ifa_int = interpolate_ifa(
    ifa_sp = urban_montserrado_ifa_sp,
    point_grid = urban_montserrado_int_points,
    idp = 2
  ),
  grand_bassa_ifa_int = interpolate_ifa(
    ifa_sp = grand_bassa_ifa_sp,
    point_grid = grand_bassa_int_points,
    idp = 2
  ),
  ifa_estimates = estimate_ifa_coverage(
    urban_montserrado_ifa_df, grand_bassa_ifa_df
  ),
  ifa_bootstraps = boot_estimates(
    .data = list(urban_montserrado_ifa_df, grand_bassa_ifa_df),
    w = list(urban_montserrado_ea_population, grand_bassa_ea_population),
    vars = c("ifa1", "ifa2", "ifa3", "ifa4", "ifa5a"),
    labs = c("Attended ANC", "Received information on IFA", "Received IFA", 
             "Consumed IFA", "Consumed IFA at least 90 days")
  ),
  ifa_factors = recode_ifa_factors(
    urban_montserrado_ifa_df, grand_bassa_ifa_df
  ),
  ## IYCF counselling coverage
  urban_montserrado_icf_int = interpolate_icf(
    icf_sp = urban_montserrado_icf_sp,
    point_grid = urban_montserrado_int_points,
    idp = 2
  ),
  grand_bassa_icf_int = interpolate_icf(
    icf_sp = grand_bassa_icf_sp,
    point_grid = grand_bassa_int_points,
    idp = 2
  ),
  icf_estimates = estimate_icf_coverage(
    urban_montserrado_icf_df, grand_bassa_icf_df
  ),
  icf_bootstraps = boot_estimates(
    .data = list(urban_montserrado_icf_df, grand_bassa_icf_df),
    w = list(urban_montserrado_ea_population, grand_bassa_ea_population),
    vars = c("icf1", "icf2"),
    labs = c("Heard about IYCF counselling", "Attended IYCF counselling")
  ),
  icf_factors = recode_icf_factors(
    urban_montserrado_icf_df, grand_bassa_icf_df
  )
)

mapping <- tar_plan(
  urban_montserrado_cmam_coverage = create_int_maps(
    urban_montserrado_int_grid, 
    urban_montserrado_ea,
    urban_montserrado_screening_int, 
    urban_montserrado_cmam_int
  ),
  grand_bassa_cmam_coverage = create_int_maps(
    grand_bassa_int_grid,
    grand_bassa_ea,
    grand_bassa_screening_int,
    grand_bassa_cmam_int
  )
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
  ),
  tar_render(
    name = standardisation_test_results,
    path = "reports/standardisation_test_results.Rmd",
    output_dir = "docs",
    knit_root_dir = here::here()
  ),
  tar_render(
    name = survey_progress_report,
    path = "reports/survey_progress_report.Rmd",
    output_dir = "outputs",
    knit_root_dir = here::here()
  ),
  ## Archive survey progress report
  survey_progress_report_archive = archive_progress_report(
    from = survey_progress_report[1]
  ),
  email_progress_message = blastula::render_email(
    input = "reports/email_progress_report.Rmd"
  ),
  tar_render(
    name = preliminary_results_report,
    path = "reports/preliminary_results_report.Rmd",
    output_dir = "outputs",
    knit_root_dir = here::here()
  ),
  tar_render(
    name = final_results_report,
    path = "reports/final_results_report.Rmd",
    output_dir = "outputs",
    knit_root_dir = here::here()
  ),
  tar_render(
    name = cmam_coverage_dashboard,
    path = "reports/cmam_coverage_dashboard.Rmd",
    output_dir = "outputs",
    knit_root_dir = here::here()
  ),
  tar_render(
    name = urban_montserrado_cmam_dashboard,
    path = "reports/urban_montserrado_cmam.Rmd",
    output_dir = "outputs",
    knit_root_dir = here::here()
  ),
  tar_render(
    name = grand_bassa_cmam_dashboard,
    path = "reports/grand_bassa_cmam.Rmd",
    output_dir = "outputs",
    knit_root_dir = here::here()
  ),
  tar_target(
    name = cmam_coverage_dashboard_zipped,
    command = {
      zip(
        zipfile = "outputs/cmam_coverage_dashboard.zip",
        files = c(
          cmam_coverage_dashboard[1], 
          urban_montserrado_cmam_dashboard[1], 
          grand_bassa_cmam_dashboard[1]
        ),
        flags = "-j"
      )
      "outputs/cmam_coverage_dashboard.zip"
    }
  ),
  email_dashboard_message = blastula::render_email(
    input = "reports/email_cmam_coverage_dashboard.Rmd",
    render_options = list(knit_root_dir = here::here())
  )
)

## Deploy targets
deploy <- tar_plan(
  ##
  survey_progress_deployed = deploy_progress_report(
    from = survey_progress_report[1],
    to = "docs/survey_progress_report.html"
  ),
  survey_progress_archive_deployed = archive_progress_report(
    from = survey_progress_deployed,
    to = paste0("docs/", Sys.Date(), "/progress/index.html")
  ),
  progress_report_emailed = email_progress_report(
    message = email_progress_message,
    attachment = survey_progress_report[1],
    sender = Sys.getenv("GMAIL_USERNAME"),
    recipient = eval(parse(text = Sys.getenv("REPORT_RECIPIENTS")))
  ),
  urban_montserrado_cmam_dashboard_deployed = deploy_cmam_dashboard(
    from = urban_montserrado_cmam_dashboard[1],
    to = "docs/urban_montserrado_cmam.html"
  ),
  grand_bassa_cmam_dashboard_deployed = deploy_cmam_dashboard(
    from = grand_bassa_cmam_dashboard[1],
    to = "docs/grand_bassa_cmam_dashboard.html"
  ),
  cmam_coverage_dashboard_deployed = deploy_cmam_dashboard(
    from = c(
      cmam_coverage_dashboard[1], 
      urban_montserrado_cmam_dashboard[1],
      grand_bassa_cmam_dashboard[1]
    )
  ),
  cmam_coverage_dashboard_gdrive_deployed = deploy_to_googledrive(
    media = cmam_coverage_dashboard_zipped,
    path = "PoN Project/Reports/",
    name = basename(cmam_coverage_dashboard_zipped)
  ),
  coverage_dashboard_emailed = email_coverage_dashboard(
    message = email_dashboard_message,
    sender = Sys.getenv("GMAIL_USERNAME"),
    recipient = eval(parse(text = Sys.getenv("REPORT_RECIPIENTS")))
  )
)

## Set seed
set.seed(1977)

# Concatenate targets ----------------------------------------------------------
list(
  spatial_sample,
  data_raw,
  survey_training,
  data_checks,
  data_processed,
  analysis,
  mapping,
  outputs,
  reports,
  deploy
)