applyThreshold <-
function(commMatrix, threshold){
  if(class(threshold) == "data.frame" || class(threshold) == "matrix"){
    i <- match(colnames(commMatrix), threshold[,1])
    thres <- threshold[i,2]
  }
  if(class(threshold) == "numeric"){
    thres <- threshold
  }
  nComm <- apply(commMatrix, MARGIN=1, FUN=function(x){x > thres})
  rownames(nComm) <- colnames(commMatrix)
  t(nComm*1)
}
