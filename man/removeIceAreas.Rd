\name{removeIceAreas}
\alias{removeIceAreas}
\title{Remove Iced Areas from the climate raster/stacks}
\description{
This function remove the Iced Areas, according to a raster of glaciated areas and lakes, from the climatic variables stored in a raster o stack. The values in glaciated areas are substituted by NA.
}
\usage{
removeIceAreas(r, iceS)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{Raster or raster stack. They will be the climatic variables to be changed}
  \item{iceS}{Raster. This raster should have value 1 in all the glaciated areas. This raster should have the same extent and resolution as the climatic raster (r).
}
}
\value{
Raster of Stack. The function return a raster or stack (according to the input r) with value NA in the cels with value 1 in the glaciated raster (iceS).
}
\author{Diego Nieto Lugilde}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(r, iceS)
{
  r[iceS == 1] <- NA
  return(r)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
