#' Partial ROC calculation of single models
#'
#' @description ku.enm.proc calculates the partial ROC of single models.
#'
#' @param occ a numeric matrix containing longitude and latitude of the
#' occurrences used to test the ecological niche model.
#' @param model a raster layer of the ecological niche model to be evaluated.
#' @param threshold (numeric) value from 0 to 100 that will be used as threshold,
#' default = 5.
#' @param ran.percent (numeric) value from 0 to 100 representing the amount of data
#' to be used for performing the bootstrap process for calculating the partial ROC,
#' default = 50.
#' @param iterations (numeric) number of bootstrapped iterations to be performed,
#' default = 1000.
#'
#' @return A Mobility-Oriented Parity raster layer.
#'
#' @details The MOP is calculated following Owens et al., 2013
#' \url{https://doi.org/10.1016/j.ecolmodel.2013.04.011}.
#'
#' @examples
#' occ <-
#' model <-
#' thres <- 5
#' rand_perc <- 50
#' iterac <- 100
#'
#' mop <- ku.enm.proc(occ = occ, model = model, threshold = thres,
#'                    ran.percent = rand_perc, iterations = iterac)

ku.enm.proc <- function(occ, model, threshold = 5, ran.percent = 50, iterations = 1000) {

  if(min(na.omit(getValues(model))) == max(na.omit(getValues(model)))) {
    warning("\nModel output with an only probability value, pROC
            will return NA.\n")
    p_roc <- rep(NA, 2)
    names(p_roc) <- c(paste("Mean_AUC_ratio_at_", threshold, "%", sep = ""), "Partial_ROC")
    return(p_roc)
  }else{
    inrastlog <- model

    ## Currently fixing the number of classes to 100. But later flexibility should be given in the parameter.
    inrast <- round((inrastlog / cellStats(inrastlog, max)) * 1000)

    ## This function should be called only once outside the loop. This function generates values for x-axis.
    ## As x-axis is not going to
    classpixels <- a.pred.pres(inrast)

    occur <- occ
    extrast <- extract(inrast, occur)

    ## Remove all the occurrences in the class NA. As these points are not used in the calibration.
    occurtbl <- cbind(occur, extrast)
    occurtbl <- occurtbl[which(is.na(occurtbl[, 3]) == FALSE), ]

    pointid <- seq(1:nrow(occurtbl))
    occurtbl <- cbind(pointid, occurtbl)
    names(occurtbl) <- c("PointID", "Longitude", "Latitude", "ClassID")

    ## Use option cl.cores to choose an appropriate cluster size.
    lapply(X = 1:iterations, FUN = function(x) {
      ll <- sample(nrow(occurtbl), round(ran.percent / 100 * nrow(occurtbl)), replace = TRUE)
      occurtbl1 <- occurtbl[ll, ]
      ## Generate the % points within each class in this table. Write SQL, using sqldf package
      occurinclass <- sqldf("Select count(*), ClassID from occurtbl1 group by ClassID order by ClassID desc")
      occurinclass <- cbind(occurinclass, cumsum(occurinclass[, 1]),
                            cumsum(occurinclass[, 1]) / nrow(occurtbl1))
      names(occurinclass) <- c("OccuCount", "ClassID", "OccuSumBelow", "Percent")

      #### Raster file will contain all the classes in ClassID column, while
      #### occurrences table may not have all the classes.
      xytable <- g.xy.tab(classpixels, occurinclass)
      arearow <- calc.auc(xytable, threshold / 100, x)
      names(arearow) <- c("IterationNo", paste("AUC_at_Value_", threshold, sep = ""), "AUC_at_0.5", "AUC_ratio")

      p_roc <- rep(NA, 2)
      names(p_roc) <- c(paste("Mean_AUC_ratio_at_", threshold, "%", sep = ""), "Partial_ROC")

      p_roc_res <- list(p_roc, arearow)

      return(p_roc_res)
    })
  }
}
