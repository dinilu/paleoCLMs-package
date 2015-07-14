\name{applyThreshold}
\alias{applyThreshold}
\title{Apply a threshold to a community matrix.}
\description{This function apply a species specific threshold or a single threshold to a community matrix, converting abundance data to presence/absence data.}
\usage{
applyThreshold(commMatrix, threshold)
}
\arguments{
  \item{commMatrix}{Matrix. Community matrix (species by sites) with abundance data (or predictions from a model).}
  \item{threshold}{Data.frame (or Matrix) or numeric value. If the threshold is a value it will apply the same threshold to all the species. If the threshold is a data.frame it should have the same number of rows as species in the community matrix and two columns. The first columns should indicate the name of the species and the second the threshold for each species.}
}
\value{Matrix. The output is a new community matrix with the same dimmensions as the input data (data). The values are now binary instead of abundances.}
\author{Diego Nieto Lugilde}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
