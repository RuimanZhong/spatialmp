% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnCreateMesh.R
\name{fnCreateMesh}
\alias{fnCreateMesh}
\title{This is the function to create a fine mesh for spatial analysis.}
\usage{
fnCreateMesh(de1, boundaryregion)
}
\arguments{
\item{de1}{The input point data. This is a sf object, with column name 'value' and 'geometry'.}

\item{boundaryregion}{The boundary region of the mesh. This is a sf object. The input data and the target should be within the boundary.}
}
\value{
a mesh object.
}
\description{
This is the function to create a fine mesh for spatial analysis.
}
