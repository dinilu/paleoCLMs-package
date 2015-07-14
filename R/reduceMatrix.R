reduceMatrix <-
function(x, varNames=NULL, lowerTri=F){
  nX <- x[[1]]
  if(!is.null(varNames)){
    ind <- which(colnames(nX) %in% varNames)
    nX <- nX[ind,ind]
  }
  if(lowerTri){
    nX[upper.tri(nX, diag=T)] <- NA
  }
  return(nX)
}
