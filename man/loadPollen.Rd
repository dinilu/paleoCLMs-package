\name{loadPollen}
\alias{loadPollen}
\title{Load pollen data.}
\description{
This function use the periods as used in the pollen file names and use the directory to look for the files.
}
\usage{
loadPollen(period, indir)
}
\arguments{
  \item{period}{Should be a vector with the values of the periods for which the pollen data should be loaded.}
  \item{indir}{Should be a directory with the path to look for the pollen files (.csv).}
}
\value{This function return a list in which each element is a dataframe with the pollen data (rows are pollen sites and columns are pollen taxon).
}
\author{Diego Nieto Lugilde}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (period, indir) 
{
    pSites <- read.csv(paste(indir, "Pollen/All.site.data-withagemodel-finalv2.txt", sep = ""), sep = "\t")
    colnames(pSites)[3] <- "sites"
    pSites <- pSites[, c("sites", "Longitude", "Latitude")]
    fName <- paste(indir, "Pollen/all data by time/PollenAbund_Q_", period, "bp.csv", sep = "")
    pConc <- read.csv(fName)
    pConc <- pConc[complete.cases(pConc), ]
    pData <- merge(pConc, pSites, by = "sites")
    return(pData)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
