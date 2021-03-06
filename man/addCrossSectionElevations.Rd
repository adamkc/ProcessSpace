% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addCrossSectionElevations.R
\name{addCrossSectionElevations}
\alias{addCrossSectionElevations}
\title{addCrossSectionElevations}
\usage{
addCrossSectionElevations(
  transectObject,
  rasterDir = "GeoData/Raster/ChildsDEM_m.tif",
  ...
)
}
\arguments{
\item{transectObject}{Object created by the \code{generateCrossSections} function}

\item{rasterDir}{Directory containing the LiDAR raster dataset}
}
\value{
transectObject with the cross section elevations added.
}
\description{
addCrossSectionElevations
}
