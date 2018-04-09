proc <- function(presencefile, predictionfile, omissionval = 0.05, randompercent = 50, noofiteration = 1000) {
  
  source("https://raw.githubusercontent.com/manubio13/ME_Cobos_FProject/master/a.pred.pres.R")
  source("https://raw.githubusercontent.com/manubio13/ME_Cobos_FProject/master/g.xy.tab.R")
  source("https://raw.githubusercontent.com/manubio13/ME_Cobos_FProject/master/calc.auc.R")
  
  inrastlog <- predictionfile
  
  ## Currently fixing the number of classes to 100. But later flexibility should be given in the parameter.
  inrast <- round((inrastlog/cellStats(inrastlog, max))*1000)
  
  ## This function should be called only once outside the loop. This function generates values for x-axis.
  ## As x-axis is not going to 
  classpixels <- a.pred.pres(inrast)
  
  occur <- read.csv(presencefile)
  occur <- occur[,-1]
  extrast <- extract(inrast, occur)
  
  ## Remove all the occurrences in the class NA. As these points are not used in the calibration.
  occurtbl <- cbind(occur, extrast)
  occurtbl <- occurtbl[which(is.na(occurtbl[,3]) == FALSE),]
  
  pointid <- seq(1:nrow(occurtbl))
  occurtbl <- cbind(pointid, occurtbl)
  names(occurtbl)= c("PointID", "Longitude", "Latitude", "ClassID")
  # # ## Generate the % points within each class in this table. Write SQL, using sqldf package
  # # occurINClass <- sqldf("Select count(*), ClassID from occurtbl group by ClassID order by ClassID desc")
  # # occurINClass <- cbind(occurINClass, cumsum(occurINClass[,1]), cumsum(occurINClass[,1]) / nrow(occurtbl))
  # # names(occurINClass) <- c("OccuCount", "ClassID", "OccuSumBelow", "Percent")
  
  ## Use option cl.cores to choose an appropriate cluster size.
  lapply(X = 1:noofiteration, FUN =  function(x){
    ll <- sample(nrow(occurtbl), round(randompercent/100 * nrow(occurtbl)), replace=TRUE)
    occurtbl1 <- occurtbl[ll,]
    ## Generate the % points within each class in this table. Write SQL, using sqldf package
    occurinclass <- sqldf("Select count(*), ClassID from occurtbl1 group by ClassID order by ClassID desc")
    occurinclass <- cbind(occurinclass, cumsum(occurinclass[,1]), cumsum(occurinclass[,1]) / nrow(occurtbl1))
    names(occurinclass) <- c("OccuCount", "ClassID", "OccuSumBelow", "Percent")
    
    #### Raster file will contain all the classes in ClassID column, while occurrences table may not have all the classes.
    #### Somehow we have to make the ClassID same as raster file ClassID. This could be done with SQL command update.
    #### but update is not working, not sure about the reason. So I am running the loop which is very very slow.
    xytable <- g.xy.tab(classpixels,occurinclass)
    #plot(xytable[,2], xytable[,3])
    arearow <- calc.auc(xytable, omissionval, x)
    names(arearow) <- c("IterationNo", paste("AUC_at_Value_", omissionval, sep = ""), "AUC_at_0.5", "AUC_ratio")
    #arearow[1] <- as.integer(arearow[1])
    return(arearow)
  })
}