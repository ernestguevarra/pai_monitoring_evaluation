################################################################################
#
#'
#' Create sp class versions of datasets
#'
#' @param df A data.frame of recoded indicator values for the Liberia Coverage
#'   Survey
#' @param coords A data.frame containing per enumeration area centroid
#'   coordinates
#' @param by.x A character value for variable in \code{df} to use for matching
#'   with \code{coords} for merging
#' @param by.y A character value for variable in \code{coords} to use for
#'   matching with \code{df} for merging
#' @param proj A projection string of class CRS
#'
#' @return An `sp` class version of datasets
#'
#'
#
################################################################################

create_sp <- function(df, coords,
                      by.x = "eid", by.y = "EFEACODE",
                      proj) {
  ##
  temp <- merge(df, coords, by.x = by.x, by.y = by.y, all.x = TRUE)
  ##
  tempSP <- sp::SpatialPointsDataFrame(coords = temp[ , c("lon", "lat")],
                                       data = temp, proj4string = proj)
  ##
  return(tempSP)
}


################################################################################
#
#' Create SpatialPoints from a hexagonal grid map
#'
#' @param hexgrid A SpatialPolygonsDataFrame hexagonal grid map
#'
#' @return A SpatialPoints class object of hexagonal grid centroids
#'
#'
#'
#
################################################################################

create_points <- function(hexgrid) {
  # sp::SpatialPoints(coords = sp::coordinates(hexgrid),
  #                   proj4string = sp::CRS(sp::proj4string(hexgrid)))
  x <- sp::SpatialPoints(coords = sp::coordinates(hexgrid))
  proj4string(x) <- proj4string(hexgrid)
  x
}




################################################################################
#
#'
#' Function to interpolate indicators from the Liberia Coverage Survey
#'
#' @param indicator A character vector of indicator data.frame names
#' @param county An integer indicating which county to interpolate; 1 for
#'   Greater Monrovia; 2 for Grand Bassa
#' @param core.columns A vector of variable names included in indicator
#'   data.frames
#' @param coords A data.frame containing per enumeration area centroid
#'   coordinates
#' @param hexgrid A SpatialPoints class object containing locations of
#'   interpolation
#' @param idp Inverse distance power. Default is 2.
#'
#' @return A data.frame as long as `hexgrid` containing
#'   interpolated indicator values at each location of `hexgrid`
#'
#'
#
################################################################################

interpolate_indicators <- function(indicator = c("ifaDF", "iycfDF",
                                                 "mnpDF", "vitDF",
                                                 "screenDF",
                                                 "anthroDF", "cmamDF"),
                                   county,
                                   core.columns = c("spid", "cid", "did", "eid",
                                                    "motherID", "m2"),
                                   coords, hexgrid,
                                   idp = 2) {
  ##
  interpolationResults <- vector(mode = "list", length = length(indicator))
  names(interpolationResults) <- indicator
  ##
  for(i in indicator) {
    ##
    currentDF <- get(i)
    ##
    currentDF <- currentDF[currentDF$cid == county, ]
    ##
    ## Check if i is anthroDF
    if(i == "anthroDF") {
      core.columns <- c(core.columns, "age", "sex", "position", "flag")
    }
    ## Check if i is cmamDF
    if(i == "cmamDF") {
      core.columns <- c("spid", "cin", "cout", "rin")
    }
    ##
    currentDFresults <- data.frame(matrix(nrow = length(hexgrid),
                                          ncol = length(names(currentDF)[!names(currentDF) %in% core.columns])))
    ##
    names(currentDFresults) <- names(currentDF)[!names(currentDF) %in% core.columns]
    ## check if i is cmamDF
    if(i == "cmamDF") {
      temp <- aggregate(x = coords[ , c("lon", "lat")],
                        by = list(coords$spid),
                        FUN = mean)
      names(temp) <- c("spid", "lon", "lat")
      currentSP <- create_sp(df = currentDF,
                             coords = temp,
                             by.x = "spid", by.y = "spid",
                             proj = sp::CRS(sp::proj4string(hexgrid)))
    } else {
      ##
      currentSP <- create_sp(df = currentDF,
                             coords = coords[ , c("EFEACODE", "lon", "lat")],
                             by.x = "eid", by.y = "EFEACODE",
                             proj = sp::CRS(sp::proj4string(hexgrid)))
    }
    ##
    for(j in names(currentDF)[!names(currentDF) %in% core.columns]) {
      currentIndicator <- currentSP[!is.na(currentSP[[j]]), ]
      if(length(currentIndicator) != 0 ) {
        temp <- gstat::idw(formula = eval(parse(text = paste(j, "~", 1, sep = " "))),
                           locations = currentIndicator,
                           newdata = create_points(hexgrid = hexgrid),
                           idp = idp)
        currentDFresults[[j]] <- temp$var1.pred
      }
    }
    ##
    interpolationResults[[i]] <- currentDFresults
  }
  return(interpolationResults)
}


################################################################################
#
#'
#' Interpolate CMAM
#'
#
################################################################################

interpolate_cmam <- function(cmam_sp, idp = 2, point_grid) {

  results_df <- data.frame(
    matrix(
      nrow = length(point_grid),
      ncol = 21
    )
  )
  
  ##
  names(results_df) <- names(cmam_sp)[9:29]
  
  ##
  for(i in names(results_df)) {
    currentIndicator <- cmam_sp[!is.na(cmam_sp[[i]]), ]
    if(length(currentIndicator) != 0 ) {
      temp <- gstat::idw(
        formula = eval(parse(text = paste(i, "~", 1, sep = " "))),
        locations = currentIndicator,
        newdata = point_grid,
        idp = idp
      )
      results_df[[i]] <- temp$var1.pred
    }
  }
  
  results_df
}


################################################################################
#
#'
#' Interpolate screening indicators
#'
#
################################################################################

interpolate_screening <- function(screening_sp, point_grid, idp = 2) {
  
  results_df <- data.frame(
    matrix(
      nrow = length(point_grid),
      ncol = 2
    )
  )
  
  ##
  names(results_df) <- names(screening_sp)[6:7]
  
  ##
  for(i in names(results_df)) {
    currentIndicator <- screening_sp[!is.na(screening_sp[[i]]), ]
    if(length(currentIndicator) != 0 ) {
      temp <- gstat::idw(
        formula = eval(parse(text = paste(i, "~", 1, sep = " "))),
        locations = currentIndicator,
        newdata = point_grid,
        idp = idp
      )
      results_df[[i]] <- temp$var1.pred
    }
  }
  
  results_df
}



################################################################################
#
#'
#' Interpolate vitamin A indicators
#'
#
################################################################################

interpolate_vita <- function(vita_sp, point_grid, idp = 2) {
  
  results_df <- data.frame(
    matrix(
      nrow = length(point_grid),
      ncol = 2
    )
  )
  
  ##
  names(results_df) <- names(vita_sp)[6:7]
  
  ##
  for(i in names(results_df)) {
    currentIndicator <- vita_sp[!is.na(vita_sp[[i]]), ]
    if(length(currentIndicator) != 0 ) {
      temp <- gstat::idw(
        formula = eval(parse(text = paste(i, "~", 1, sep = " "))),
        locations = currentIndicator,
        newdata = point_grid,
        idp = idp
      )
      results_df[[i]] <- temp$var1.pred
    }
  }
  
  results_df
}
