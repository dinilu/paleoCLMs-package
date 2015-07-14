trainSplit <-
function(m, nRep=1, perc=0.7, seed=NULL){
  trainingSubset <- matrix(NA, nrow(m), nRep)
  
  for(i in 1:nRep){
    if(!is.null(seed)){
      set.seed(seed+i-1)
    }
    trainingSubset[,i] <- runif(nrow(m)) < perc
  }
  
  return(trainingSubset)
}
