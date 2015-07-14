\name{vif}
\alias{vif}
\title{Calculate Variance Inflation Factor (VIF)}
\description{
This function calculate the Variance Inflation Factor for all the variables in a matrix.}
\usage{
vif(x, v=NULL)
}
\arguments{
  \item{x}{Dataframe. Each column in this dataframe that should be a variables to compute the VIF.}
  \item{v}{Optional character or numeric vector. It should specify a subset of variables by its name or its column number in the x matrix. If v is not provided vif are calculated for all the variables in x.}
}
\value{
The function return a numeric vector with a VIF value of each variable (column) in the x matrix.}
\author{Diego Nieto Lugilde}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
