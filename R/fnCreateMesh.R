#' This is the function to create a fine mesh for spatial analysis.
#' @import methods
#' @import sf
#' @import INLA
#' @param de1 The input point data. This is a sf object, with column name 'value' and 'geometry'.
#' @param boundaryregion The boundary region of the mesh. This is a sf object. The input data and the target should be within the boundary.
#' @return a mesh object.
#' @export
fnCreateMesh <- function(de1, boundaryregion) {
  location <- NULL
  if (!is.null(de1)) {
    # de1 <- st_transform(de1, crs = 4326)
    location <- as.matrix(sf::st_coordinates(de1)[, c(1, 2)])
  }

  maxedge <- fnMaxEdgeMeshFromBoundary(boundaryregion)

  bdsp <- as(boundaryregion, "Spatial")
  mesh <- INLA::inla.mesh.2d(loc = location, boundary = bdsp, max.edge = c(maxedge / 10, maxedge), cutoff = maxedge / 25)

  return(mesh)
}

fnMaxEdgeMeshFromBoundary <- function(bd) {
  maxedge <- 0.33 * max(
    attributes(sf::st_geometry(bd))$bbox[3] - attributes(sf::st_geometry(bd))$bbox[1],
    attributes(sf::st_geometry(bd))$bbox[4] - attributes(sf::st_geometry(bd))$bbox[2]
  )
  return(maxedge)
}
