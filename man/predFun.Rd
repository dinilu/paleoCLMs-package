\name{predFun}
\alias{predFun}
\title{Inverse order in the predict function.}
\description{
This function is just a shorcut to change the order of the arguments for the predict function. I find this handy to use the predict function using the lapply function over several scenarios, instead of several models.
}
\usage{
predFun(cD, m, ...)
}
\arguments{
  \item{cD}{Dataframe. It should be the matrix with predictor variables specified in the model.}
  \item{m}{Model. It should be the model to predict with the predict function.}
  \item{\dots}{Other arguments passed to the predict function. It is usefull for specify response parameter for different models.}
}
\value{Matrix. Community matrix of predicted values from a distribution model and the corresponding predictor variables.}
\author{Diego Nieto Lugilde}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (cD, m, ...) 
{
    predict(m, cD, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
