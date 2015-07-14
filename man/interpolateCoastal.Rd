\name{interpolateCoastal}
\alias{interpolateCoastal}
\title{Interpolate climatic values in emerged areas.}
\description{
This function use an inverse distance weighted interpolation to estimate climate values in areas previously emerged that are now under the water. It use the values in the closest pixels to estimate the values in pixels specified by a shoreline raster.
}
\usage{interpolateCoastal(raster, shore)}
\arguments{
  \item{raster}{Raster object, with the variable to be interpolated}
  \item{shore}{Raster object with values 1 to areas emerged and NA otherwise. This raster should have the same extent and resolution as the climatic raster. }
}
\value{
The output is a new raster with the same extent and resolution than the input raster (raster). Pixel with value 1 in shore that have values already in raster are unchanged, whereas those whitout value in raster are interpolated by inverse distance weighted interpolation.}
\author{Diego Nieto Lugilde}
\note{Interpolation method is in gstat package, with nmax=7 and idp=0.5.}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(raster, shore)
{
  xy <- data.frame(xyFromCell(raster, 1:ncell(raster)))
  v <- getValues(raster)
  dat <- data.frame(xy, v)
  dat <- dat[complete.cases(dat),]
#  mg <- Tps(x=xy, Y=v) Esta funcion peta
  mg <- gstat(formula=v~1, locations=~x+y, data=dat, nmax=7, set=list(idp = .5))
  p <- raster(raster)
  z <- interpolate(p, mg)
  nz <- z * shore
  nnz <- cover(raster, nz)
  return(nnz)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
