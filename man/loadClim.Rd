\name{loadClim}
\alias{loadClim}
\title{Load climate data.}
\description{
This function load the climate data from a specified period of time and climate model. For this it takes the directory where the raster variables are and the pollen data that should with the latlong coordinates to extract the climate data.
}
\usage{
loadClim(period, clim_model, pollen, indir, vars)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{period}{Numeric. It should specify a period value (0-21000) to load the climatic data.}
  \item{clim_model}{String. It should specify the climatic model that will be loaded.}
  \item{pollen}{Matrix. Community matrix (species by sites) to be used to extract the climate data.}
  \item{indir}{String. Directory to look for the raster climate data.}
  \item{vars}{Character vector. It should specify the name of the variables that should be loaded.}
}
\details{This function load only the climate data corresponding for the same coordinates as the pollen sites. This way we don't have to work with the raster loaded all the time.}
\value{The output is a dataframe with a column for each climatic variable, and as many rows as pollen sites.}
\author{Diego Nieto Lugilde}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
