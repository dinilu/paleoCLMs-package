removeLowQualitySamples <-
function(p, threshold=0.5){
  pN <- p[which(p$mean.quality > threshold),]
  return(pN)
}
