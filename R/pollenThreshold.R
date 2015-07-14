pollenThreshold <- function(commM, perc){
  maxCon <- apply(commM, MARGIN=2, FUN=max)
  thres <- as.vector(perc * maxCon)
  result <- data.frame(colnames(commM), thres)
  return(result)
}
