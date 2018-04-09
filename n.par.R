n.par <- function(x) {
  lambdas <- x[1:(length(x) - 4)]
  countNonZeroParams <- function(x) {
    if (strsplit(x, split = ", ")[[1]][2] != "0.0") 
      1
  }
  no.params <- sum(unlist(sapply(lambdas, countNonZeroParams)))
  return(no.params)
}