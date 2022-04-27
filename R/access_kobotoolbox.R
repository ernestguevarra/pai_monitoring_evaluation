################################################################################
#
#'
#' Get asset id
#' 
#' @param form_name Name of form of interest. This should be the full form
#'   name given to the form in the XLSForm. For PAI monitoring, this will either
#'   be "Product Access Initiative Monitoring and Evaluation Survey Form
#'   (MUAC only)" or "Product Access Initiative Monitoring and Evaluation
#'   Enumeration Area Form"
#'
#
################################################################################

get_kobo_form_id <- function(form_name) {
  robotoolbox::kobo_setup(
    url = Sys.getenv("KOBOTOOLBOX_URL"),
    token = Sys.getenv("KOBOTOOLBOX_TOKEN")
  )
  
  asset_list <- robotoolbox::kobo_asset_list()
  
  asset_id <- asset_list |>
    subset(name == form_name, select = uid) |>
    unlist()
  
  asset_id
}


################################################################################
#
#'
#' Access data from Kobotoolbox
#' 
#' @param form_id A character value for the unique form identifier provided to
#'   the form of interest. This can be retrieved using the `get_kobo_form_id()`
#'   function
#'
#
################################################################################

get_kobo_data <- function(form_id) {
  robotoolbox::kobo_setup(
    url = Sys.getenv("KOBOTOOLBOX_URL"),
    token = Sys.getenv("KOBOTOOLBOX_TOKEN")
  )

  asset <- robotoolbox::kobo_asset(x = form_id)

  .data <- robotoolbox::kobo_submissions(x = asset)

  .data <- .data$main |>
    dplyr::right_join(
      .data$child_repeat, 
      by = c("_index" = "_parent_index")
    )
  
  .data
}

