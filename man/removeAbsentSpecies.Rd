\name{removeAbsentSpecies}
\alias{removeAbsentSpecies}
\title{Remove species with low (or high) prevalence in a community matrix.}
\description{
This function takes a community matrix and remove all those species that have less than a specified number of presences in the dataset. It also provide an option to remove high prevalent species or both.
}
\usage{
removeAbsentSpecies(commMatrix, n, applyTo = c("low", "high", "both"))
}
\arguments{
  \item{commMatrix}{Matrix. Community matrix (species by sites) to be reduced.}
  \item{n}{Numeric. This value indicate the minimum number of presences for a species to be kept in the output community matrix.}
  \item{applyTo}{String. This argument specify whether the function will apply as minimum number of presences (remove low prevalent species with less presences than n) or as minimum number of absences (remove high prevalent species with less absences than n) or as minimum number of both presences and absences (remove high and low prevalent species).}
}
\value{Matrix. Community matrix with less species than the input but with the same number of sites.}
\details{The function use the function "greater than" and not "greater or equal than" so, to get species with 20 presences you should specify the value 19 in the argument n.}
\author{Diego Nieto Lugilde}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (commMatrix, n, applyTo = c("low", "high", "both")) 
{
    pL <- commMatrix > 0
    lowPrev <- which(colSums(pL) >= n)
    highPrev <- which(colSums(!pL) >= n)
    if (applyTo == "low") 
        staySpec <- lowPrev
    if (applyTo == "high") 
        staySpec <- highPrev
    if (applyTo == "both") 
        staySpec <- intersect(lowPrev, highPrev)
    pN <- commMatrix[, staySpec]
    return(pN)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
