\name{spPrevalence}
\alias{spPrevalence}
\title{Get the species' prevalence from a binary community matrix.}
\description{
Given a community matrix, the function give out a vector with the prevalence of each species in the data.
}
\usage{
spPrevalence(commMatrix, relative=TRUE)
}
\arguments{
  \item{commMatrix}{Matrix. Community matrix (species by sites) with binary (presence/absence) data.}
  \item{relative}{Logical value. This value indicate if the prevalence should be absolute (number of presences) or relative (percentage of presences).}
}
\value{
The output is a vector with one element for each species in the community matrix (commMatrix). It can be total prevalence or relative prevalence of each species.
}
\author{Diego Nieto Lugilde}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (commMatrix) 
{
    sP <- colSums(commMatrix)
    return(sP)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
