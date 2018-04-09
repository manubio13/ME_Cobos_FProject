rast.bin <- function(rasterpre, threshold){
  rdf <- data.frame(rasterToPoints(rasterpre))
  names(rdf) <- c("x","y","binary")
  reclass <- sapply(rdf$binary, function(x){
    if(x >= threshold) return(1)
    else return(0)
  })
  rdf$binary <- reclass
  coordinates(rdf) <- ~ x + y
  # coerce to SpatialPixelsDataFrame
  gridded(rdf) <- TRUE
  # coerce to raster
  binraster <- raster(rdf)
  return(binraster)
}