spPrevalence <-
function(commMatrix, relative=TRUE){
  sP <- colSums(commMatrix)
  if(relative == TRUE){ sP <- sP/nrow(commMatrix)}
  return(sP)
}
