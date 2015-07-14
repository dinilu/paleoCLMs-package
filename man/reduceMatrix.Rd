\name{reduceMatrix}
\alias{reduceMatrix}
\title{Reduce a matrix by the name of variables.}
\description{
This function reduce a square matrix keeping only those variables specified by the users. It also allow to keep the whole matrix or the lower triangle only.
}
\usage{
reduceMatrix(x, varNames=NULL, lowerTri=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Matrix. Matrix to be reduced.}
  \item{varNames}{String vector. It should contain variables name in x to be keeped in the output matrix.}
  \item{lowerTri}{Logical value, indicating if the output matrix should include all the values or only the lower triangle. Default value is returning the whole matrix.}
}
\value{Matrix. Which will be a submatrix of x, where only the variables specified (varNames) are keeped.}
\author{Diego Nieto Lugilde}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
