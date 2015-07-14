\name{poolingData}
\alias{poolingData}
\title{Pool data across time periods.}
\description{
This function take a list of data (matrix or dataframes) and an index of which data should be pooled together. It return a new list of data with less elements, in which the previous data have been pooled.
}
\usage{
poolingData(index, Data)
}
\arguments{
  \item{index}{Numeric vector. The vector should have the same length as the list of data (Data). All the elements in Data that have the same index value will be pooled and will be ordered according to the index value.}
  \item{Data}{List. This list will store the matrices or dataframes to be pooled. It should have the same number of elements as index.
}
}
\value{List. The output will have the same output as Data, but as many elements as specified in index. The elements in the output will be bigger (greater number of sites) than Data.}
\author{Diego Nieto Lugilde}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (index, Data) 
{
    l <- Data[index]
    df <- do.call("rbind", l)
    return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
