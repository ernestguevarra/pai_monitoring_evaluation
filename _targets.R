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
  )
)

## Read raw data
raw_data <- tar_plan(
  
)


## Process data
processed_data <- tar_plan(
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
  raw_data,
  processed_data,
  analysis,
  outputs,
  reports,
  deploy
)