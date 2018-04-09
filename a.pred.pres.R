a.pred.pres <- function(inrast) {
  ### Calculate proportionate area predicted under each suitability
  classpixels <- freq(inrast)
  ### Remove the NAs from table
  if (is.na(classpixels[dim(classpixels)[1],1]) == TRUE)
  {
    classpixels <- classpixels[-dim(classpixels)[1],]
  }
  classpixels <- classpixels[order(nrow(classpixels):1),]
  totpixelperclass <- cumsum(classpixels[,2])
  percentpixels <- totpixelperclass / sum(classpixels[,2])
  
  classpixels <- cbind(classpixels, totpixelperclass, percentpixels)
  classpixels <- classpixels[order(nrow(classpixels):1),]
  return(classpixels)
}