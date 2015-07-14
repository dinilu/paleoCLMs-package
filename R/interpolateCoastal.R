interpolateCoastal <- function(raster, shore){
  xy <- data.frame(xyFromCell(raster, 1:ncell(raster)))
  v <- getValues(raster)
  dat <- data.frame(xy, v)
  dat <- dat[complete.cases(dat),]
#  mg <- Tps(x=xy, Y=v) Esta funcion peta
  mg <- gstat(formula=v~1, locations=~x+y, data=dat, nmax=7, set=list(idp = .5))
  p <- raster(raster)
  z <- interpolate(p, mg)
  nz <- z * shore
  nnz <- cover(raster, nz)
  return(nnz)
}

