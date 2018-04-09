g.xy.tab <- function(classpixels, occurinclass) {
  xytable <- classpixels[,c(1,4)]
  xytable <- cbind(xytable,rep(-1,nrow(xytable)))
  # names(xytable) <- c("ClassID", "XCoor", "YCoor")
  ## Set the previous value for 1-omission, i.e Y-axis as the value of last
  ## class id in Occurrence table. Last class id will always smallest
  ## area predicted presence.
  prevyval <- occurinclass[1,4]
  for (i in nrow(classpixels):1)
  {
    curclassid <- xytable[i,1]
    yval <- occurinclass[which(occurinclass[,2] == curclassid),4]
    ## print(paste("Length of yval :",length(yval), "Current Loop count :", i, "Current value of yval : ", yval, sep = " " ))
    
    if (length(yval) == 0 )
    {
      xytable[i,3] <- prevyval
    }
    else
    {
      xytable[i,3] <- yval
      prevyval <- yval
    }
  }
  ## Add A dummy class id in the xytable with coordinate as 0,0
  xytable <- rbind(xytable, c(xytable[nrow(xytable),1] + 1, 0, 0))
  xytable <- as.data.frame(xytable)
  names(xytable) <- c("ClassID", "XCoor", "YCoor")
  ### Now calculate the area using trapezoid method.
  return(xytable)
}