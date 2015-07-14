\name{removeLowQualitySamples}
\alias{removeLowQualitySamples}
\title{Remove low quality pollen data.}
\description{
This function remove those pollen data below a quality threshold specified by the user.
}
\usage{
removeLowQualitySamples(p, threshold = 0.5)
}
\arguments{
  \item{p}{Matrix. Community matrix of pollen data. This matrix should have a column called "mean.quality" indicating a quality value for each data.}
  \item{threshold}{Value. This value should indicate the quality threshold to be keep in the output matrix. All the data with a quality below this threshold will be removed from the output.}
}
\value{Matrix. Community matrix with the same number of species than the input community matrix but with less sites, since those that doesn't reach the quality threshold are removed.}
\author{Diego Nieto Lugilde}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (p, threshold = 0.5) 
{
    pN <- p[which(p$mean.quality > threshold), ]
    return(pN)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
