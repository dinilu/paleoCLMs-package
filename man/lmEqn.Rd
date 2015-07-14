\name{lmEqn}
\alias{lmEqn}
\title{Get equation from a linear model.}
\description{
Given a linear model the function get back the equation and goodness of fit as a string.
}
\usage{
lmEqn(m)
}
\arguments{
  \item{m}{Linear model. It is a linear model to extract the formula, coefficients, and goodness of fit measures.
}
}
\value{The output is a character string with the formaula, with coefficients and a summary of the fit.}
\author{Diego Nieto Lugilde}
\note{
I took this from internet: \url{http://stackoverflow.com/questions/7549694/ggplot2-adding-regression-line-equation-and-r2-on-graph}.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (m) 
{
    l <- list(a = format(coef(m)[1], digits = 2), b = format(abs(coef(m)[2]), 
        digits = 2), r2 = format(summary(m)$r.squared, digits = 3))
    if (coef(m)[2] >= 0) {
        eq <- substitute(italic(y) == a + b \%.\% italic(x) * "," ~ 
            ~italic(r)^2 ~ "=" ~ r2, l)
    }
    else {
        eq <- substitute(italic(y) == a - b \%.\% italic(x) * "," ~ 
            ~italic(r)^2 ~ "=" ~ r2, l)
    }
    as.character(as.expression(eq))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
