\name{pollenThreshold}
\alias{pollenThreshold}
\title{Create a dataframe to be used in the applyThreshold function.}
\description{This function creates a dataframe with a threshold for each taxon in a community matrix. The threshold could be based in a percentage value for all the taxa or in a specific value for each taxon.}
\usage{
pollenThreshold(commM, perc)
}
\arguments{
  \item{commM}{Matrix. Community matrix (species by sites) with pollen percentages.}
  \item{perc}{Numeric or vector. If numeric it applies the same percentage for all the taxa in the commM matrix. If vector, then it has to have the same length than the number of rows in the commM matrix.}
}
\value{Matrix. The output is a dataframe with two columns, the first column will list all the taxa in the commM and the second column will list the threshold for each taxon.}
\author{Diego Nieto Lugilde}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
