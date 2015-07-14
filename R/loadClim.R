loadClim <-
function(period, clim_model, pollen, indir, vars){
  directory <- paste(indir, "Climate/", clim_model, "/", period, "BP", sep="")
  fList <- paste(directory, "/", vars, ".tif", sep="")

  cStack <- stack(fList)
  cData <- extract(cStack, pollen[,c("x","y")])
  return(cData)
}
