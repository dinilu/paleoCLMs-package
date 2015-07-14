removeAbsentSpecies <-
function(commMatrix, n, applyTo=c("low","high","both")){
  if(class(applyTo) == "vector"){stop("You should specify only one value for the applyTo argument: low, high or both.")}

  pL <- commMatrix > 0

  # The nomenclature can be confusing. lowPrev actually refer to all taxon except low prevanlence ones. Similarly highPrev is an index of taxons when the high prevalents are removed.
  lowPrev <- which(colSums(pL) > n)
  highPrev <- which(colSums(!pL) > n)
  
  if(applyTo == "low") staySpec <- lowPrev
  if(applyTo == "high") staySpec <- highPrev
  if(applyTo == "both") staySpec <- intersect(lowPrev, highPrev)

  pN <- commMatrix[,staySpec]

  return(pN)
}
