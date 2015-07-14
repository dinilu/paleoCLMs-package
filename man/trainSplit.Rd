\name{trainSplit}
\alias{trainSplit}
\title{Generate random selections of training/testing datasets}
\description{
This function return a logical vector specifing which elements should be used for training or testing the dataset. It allow to specify several replicates, and then the output will be a matrix with as many columns as replicates have been specified. It also allow to change the proportion of training data which is 0.7 by default.
}
\usage{
trainSplit(m, nRep=1, perc=0.7, seed=NULL)
}
\arguments{
  \item{m}{Matrix. Community matrix that will be splitted in training and testing datasets.}
  \item{nRep}{Value. The number of replicates that will be run. Then the same number of random vector will be created and returned in a matrix.}
  \item{perc}{Value. Value in the range of 0 to 1 indicating the proportion of points to be used in training.}
  \item{seed}{Value. This value is used as initial seed for the first replicate, the next replicates will increase this seed by adding the number of the replicate.}
}
\value{
Matrix. The output is a matrix with one column for each replicate indicated (nRep). The number of rows in the matrix correspond with the number of sites in the community matrix (m).
}
\author{Diego Nieto Lugilde}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
