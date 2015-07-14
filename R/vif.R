vif <-
function(x, v=NULL)
  {
  if(is.null(v)){ v <- colnames(x) }
  vif <- NULL
  for (i in 1:length(v))
    {
      linMod <- lm(as.formula(paste(v[i], " ~ ", paste(v[-i], collapse="+"), sep="")), data=x)
      rS <- summary(linMod)$r.squared
      vif[i] <- 1/(1-rS)
    }
  return(vif)
  }
