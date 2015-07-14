\name{reduceDuplicated}
\alias{reduceDuplicated}
\title{Remove duplicated pollen sites from the same climate grid.}
\description{
This function reduce the pollen sites that fall in the same grid cell of the climate rasters. The result is a new community matrix where the latlong coordinates are shifted to the pixels centers in which there are a pollen site. When there are several pollen sites in the same grid cell their values are averaged.
}
\usage{
reduceDuplicated(commM, raster, weighted=FALSE)
}
\arguments{
  \item{commM}{Matrix object. Community matrix (species by sites) with 4 extra columns: "sites" (for sites names), "Longitude" (for the x coordinates), "Latitude" (for the y coordinates), and "mean.quality" (for quality data).}
  \item{raster}{Raster. This object is used to define the grid cells}
  \item{weighted}{Logical value. This argument indicate whether the average should be weighted by data quality. Default value is FALSE and the mean is unweighted.}
}
\value{This function return a matrix object. This matrix is a community matrix with the same taxa than the input and two extra columns (x and y) for longitude and latitude respectively. The coordinates for each row now correspond to the centroid of the grid cell in which the pollen sites where placed, and the pollen values are the average (weighted or not depending on the user).}
\author{Diego Nieto Lugilde}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
