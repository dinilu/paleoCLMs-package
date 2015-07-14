poolingData <-
function(index, Data){
  l <- Data[index]

  df <- do.call("rbind", l)

  return(df)
}
