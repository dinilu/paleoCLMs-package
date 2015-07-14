removeIceAreas <- function(r, iceS){
  r[iceS == 1] <- NA
  return(r)
}
