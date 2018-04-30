#' Counting parameters of single Maxent models
#'
#' @description n.par counts the number of parameters of a single Maxent model.
#'
#' @param x a vector created from reading a lambdas file produced for maxent.
#'
#' @return A numeric value of the parameters of a single model created
#' in Maxent.
#'
#' @examples
#' lambdas <-
#' num_par <- n.par(lambdas)

n.par <- function(x) {
  lambdas <- x[1:(length(x) - 4)]
  countNonZeroParams <- function(x) {
    if (strsplit(x, split = ", ")[[1]][2] != "0.0")
      1
  }
  no.params <- sum(unlist(sapply(lambdas, countNonZeroParams)))
  return(no.params)
}
