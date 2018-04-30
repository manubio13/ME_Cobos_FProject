#' Get amount of RAM not occupied at the moment
#'
#' @description get.free.ream calculates the amount of RAM memory that is
#' not being occupied at the moment.
#'
#' @return The amount of free RAM at the moment as a numeric value.
#'
#' @details At the moment only works for windows. For its use in the
#' \code{\link{ku.enm.cal}} and \code{\link{ku.enm.mod}} functions for
#' in other operative systems returns a value of 4 GB.
#'
#' @examples
#' free_ram <- get.free.ram()

get.free.ram <- function(){
  if(Sys.info()[["sysname"]] == "Windows"){
    x <- system2("wmic", args =  "OS get FreePhysicalMemory /Value", stdout = TRUE)
    x <- x[grepl("FreePhysicalMemory", x)]
    x <- gsub("FreePhysicalMemory=", "", x, fixed = TRUE)
    x <- gsub("\r", "", x, fixed = TRUE)
    as.integer(x)
  } else {
    cat("\nOnly supported on Windows OS\n1.6 Gb of the memory will be used for java runnings\n")
    x <- 4000000
  }
}
