\name{multiPlot}
\alias{multiPlot}
\title{Plot several ggplots in the same device.}
\description{
I borrowed this function from a website. I should look for it again and cite properly. This function allow to plot several ggplots in the same devide.
}
\usage{
multiPlot(..., plotlist = NULL, cols = 1, layout = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{Plot objects. They can be as many as necesary. They should be plot object that would draw something in a display device.}
  \item{plotlist}{List. List of plot objects. They can be as many as necesary. They should be plot object that would draw something in a display device.}
  \item{cols}{Numeric value. This should indicate the number of columns to be drawn in the multi plot.}
  \item{layout}{Matrix. That should be a matrix to pass to the layout function}
}
\value{This function doesn't get an object back. It only plot several plot objects in the same device.}
\references{
I took this function from the Cookbook for R website (\url{http://www.cookbook-r.com}): \url{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
}
}
\author{Diego Nieto Lugilde}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (..., plotlist = NULL, cols = 1, layout = NULL) 
{
    require(grid)
    plots <- c(list(...), plotlist)
    numPlots = length(plots)
    if (is.null(layout)) {
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), 
            ncol = cols, nrow = ceiling(numPlots/cols))
    }
    if (numPlots == 1) {
        print(plots[[1]])
    }
    else {
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), 
            ncol(layout))))
        for (i in 1:numPlots) {
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, 
                layout.pos.col = matchidx$col))
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
