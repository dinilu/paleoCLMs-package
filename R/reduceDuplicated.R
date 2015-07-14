reduceDuplicated <- function(commM, raster, weighted=FALSE){
  cels <- cellFromXY(raster, commM[,c("Longitude","Latitude")])
  uCels <- unique(cels)
  ind <- which(colnames(commM) %in% c("sites","Longitude","Latitude","mean.quality"))
  nComm <- commM[0,-ind]

  for(i in 1:length(uCels)){
    if(weighted == TRUE){
      nComm[i,] <- apply(commM[which(cels == uCels[i]), -ind], MARGIN=2, FUN=weighted.mean, wt=commM[which(cels == uCels[i]), "mean.quality"])
    }else{
      nComm[i,] <- apply(commM[which(cels == uCels[i]), -ind], MARGIN=2, FUN=mean)
    }
  }
  
  xy <- xyFromCell(raster, uCels)
  nComm <- cbind(nComm, xy)

  return(nComm)
}
