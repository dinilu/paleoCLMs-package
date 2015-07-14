loadPollen <-
function(period, indir){

  pSites <- read.csv(paste(indir, "Pollen/All.site.data-withagemodel-finalv2.txt", sep=""), sep="\t")
  colnames(pSites)[3] <- "sites"
  pSites <- pSites[,c("sites","Longitude","Latitude")]

  fName <- paste(indir, "Pollen/all data by time/PollenAbund_Q_", period, "bp.csv", sep="")
  pConc <- read.csv(fName)
  pConc <- pConc[complete.cases(pConc),]

  pData <- merge(pConc, pSites, by="sites")

  return(pData)
}
