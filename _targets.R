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
  counties = liberia::counties,
  districts = liberia::districts,
  clans = liberia::clans,
  urban_montserrado = liberia::districts |> 
    subset(admin2Name == "Greater Monrovia"),
  grand_bassa = liberia::counties |>
    subset(admin1name == "Grand Bassa"),
  urban_montserrado_ea = liberia::greaterMonroviaEA,
  grand_bassa_ea = liberia::grandBassaEA,
  urban_montserrado_ea_centroids = urban_montserrado_ea |>
    sf::st_centroid(),
  grand_bassa_ea_centroids = grand_bassa_ea |>
    sf::st_centroid(),
  urban_montserrado_sp = sf::as_Spatial(urban_montserrado) |>
    spatialsampler::create_sp_grid(
      n = 30, fixed = TRUE, n.factor = 0, country = "Liberia"
    ),
  grand_bassa_sp = sf::as_Spatial(grand_bassa) |>
    spatialsampler::create_sp_grid(
      n = 30, fixed = TRUE, n.factor = 10, country = "Liberia",
    ),
  urban_montserrado_grid = sp::HexPoints2SpatialPolygons(urban_montserrado_sp),
  grand_bassa_grid = sp::HexPoints2SpatialPolygons(grand_bassa_sp),
  urban_montserrado_sample = urban_montserrado_ea_centroids |> 
    (\(x) cbind(data.frame(x), sf::st_coordinates(x)))() |>
    spatialsampler::get_nearest_point(
      data.x = "X", data.y = "Y", query = urban_montserrado_sp
    ),
  grand_bassa_sample = grand_bassa_ea_centroids |>
    (\(x) cbind(data.frame(x), sf::st_coordinates(x)))() |>
    spatialsampler::get_nearest_point(
      data.x = "X", data.y = "Y", query = grand_bassa_sp
    )
)

## Read raw data
raw_data <- tar_plan(
  ##
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
  ##
)


## Reports
reports <- tar_plan(
  ##
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