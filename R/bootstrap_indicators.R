################################################################################
#
#'
#' Apply blocked weighted bootstrap to indicator datasets
#'
#' A wrapper for \code{bootBW} function in \code{bbw} package.
#'
#' @param indicator A character vector of indicator data.frame names
#' @param county An integer indicating which county to interpolate; 1 for
#'   Greater Monrovia; 2 for Grand Bassa
#' @param w Population dataset
#' @param replicates Number of bootstrap replicates
#' @param core.columns A vector of variable names included in indicator
#'   data.frames
#'
#' @return A data.frame of indicator estimates with lower and upper confidence
#'   limits
#'
#' @examples
#' boot_estimate(indicator = "iycfDF", county = 1, w = psuDataGM, replicates = 9)
#'
#' @export
#'
#
################################################################################

boot_estimate <- function(.data,
                          w, 
                          vars,
                          labs,
                          suffix,
                          replicates = 399) {
  currentDF <- .data[c("eid", vars)]
  ##
  params <- vars
    
  ## Rename "eid" to psu
  colnames(currentDF)[1] <- "psu"
  
  ##
  outputColumns <- params
    
  ##
  temp <- bbw::bootBW(
    x = currentDF, 
    w = w,
    statistic = bbw::bootClassic,
    params = params,
    outputColumns = outputColumns,
    replicates = replicates
  )
    
  est <- apply(
    X = temp, 
    MARGIN = 2, 
    FUN = quantile,
    probs = c(0.5, 0.025, 0.975), na.rm = TRUE
  )
  
  est <- t(est)
    
  est <- data.frame(labs, est)
  
  row.names(est) <- 1:nrow(est)
    
  names(est) <- c(
    "indicators", 
    paste(c("estimate", "lcl", "ucl"), suffix, sep = "_")
  )
  
  est
}


boot_estimates <- function(.data, 
                           w, vars, labs, 
                           suffix = c("gm", "gb"), 
                           replicates = 399) {
  vars   <- rep(list(vars), 2)
  labs   <- rep(list(labs), 2)

  Map(
    f = boot_estimate,
    .data = .data,
    w = w,
    vars = vars,
    labs = labs,
    suffix = suffix,
    replicates = replicates
  ) |>
    (\(x) do.call(cbind, x))() |>
    (\(x) x[ , c(1:4, 6:8)])()
}

