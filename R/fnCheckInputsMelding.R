#' This is a function to check the inputs of fnPredictMelding()
#' check they are sf objects
#' check CRS points and areas are the same.
#' @import sf
#' @param depoint dataset estimation point.sf obj. Columns value, geometry
#' @param dearea dataset estimation area. sf obj. Columns value, geometry
#' @param dppoint dataset prediction point. sf obj. Columns geometry
#' @param dparea dataset prediction area. sf obj. Columns geometry
#' @param boundaryregion The boundary region of the mesh. This is a sf object. The input data and the target should be within the boundary.
#' @return if the inputs of fn
fnCheckInputsMelding <- function(depoint, dearea, dppoint, dparea, boundaryregion) {

  # Need to provide point and/or areal data for estimation
  if (is.null(dearea) == T && is.null(depoint) == T) {
    stop("'Valid estimation data input required'")
  }

  # Need to provide point and/or areal data for prediction
  if (is.null(dparea) == T && is.null(dppoint) == T) {
    stop("'Valid preidction data input required'")
  }

  # Classes should be sf
  if (sum(c(class(depoint)[[1]], class(dppoint)[[1]], class(dparea)[[1]], class(dearea)[[1]]) %in% c("sf", "NULL")) != 4) {
    stop("'All input data should be 'sf' obj")
  }


  if (is.null(depoint) == F && sum(c("value", "geometry") %in% colnames(depoint)) != 2) {
    stop("'depoint' must have 'value','geometry' as column names")
  }
  if (is.null(dearea) == F && sum(c("value", "geometry") %in% colnames(dearea)) != 2) {
    stop("'dearea' must have 'value','geometry', as column names")
  }

  if (st_crs(depoint) != st_crs(dearea) && sum(c(is.null(depoint), is.null(dearea))) == 0) {
    stop("all the input data must have the same crs, use st_crs() to check your data")
  }
  if (is.null(dparea) == F && sum(c(st_crs(dparea) == st_crs(depoint), st_crs(dparea) == st_crs(dearea))) == 0) {
    stop("all the input data must have the same crs, use st_crs() to check your data")
  }
  if (is.null(dppoint) == F && sum(c(st_crs(dppoint) == st_crs(depoint), st_crs(dppoint) == st_crs(dearea))) == 0) {
    stop("all the input data must have the same crs, use st_crs() to check your data")
  }

  if (!class(boundaryregion)[1] %in% c("sf", "NULL")) {
    stop("boundary should be null or 'sf' obj")
  }
  if (is.null(dearea) == F && nrow(dparea) != nrow(st_join(dparea, boundaryregion))) {
    stop("predicted area should be within boundary")
  }

  if (is.null(dppoint) == F && nrow(dppoint) != nrow(st_join(dppoint, boundaryregion))) {
    stop("predicted area should be within boundary")
  }
}
