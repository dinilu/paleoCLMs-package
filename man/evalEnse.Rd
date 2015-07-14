\name{evalEnse}
\alias{evalEnse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaluate community assemblages predictions.
}
\description{
This function evaluate several measures of community assemblage for predictions. It take two community matrices (observed and predicted), a logical vector specifing test(training) datasets, and an option to specify if the data in the community matrices are presence/absence (binary=TRUE) or abundances (binary=FALSE).
}
\usage{
evalEnse(obse, pred, tS, binary=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obse}{Matrix. Community matrix with observed values. Thay can be binary data (presence/absence) or abundance data.}
  \item{pred}{Matrix. Community matrix with predicted values. It should have the same size (and with the same column and row order) than obse.}
  \item{tS}{Logical vector. This logical vector should indicate which sites will be used as training (TRUE) and testing (FALSE).}
  \item{binary}{Logical value. This specify if the data are binary (TRUE) or abundance (FALSE).}
}
\details{This function doesn't check for consistency between data and binary parameter. If there is a mismatch between them the result won't be reliable.}
\value{Vector. The output is a small vector(matrix) with 6 elements: mean Jaccard index, standard deviation of Jaccard index, discrimination (as correlation) and calibration (as model efficiency) bewteen pair of sites dissimilarity, species richness discrimination (as correlation) and calibration (as model efficiency).

Model efficiency has been defined as the Nash-Sutcliffe Efficiency.
}
\author{Diego Nieto Lugilde}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
                                             