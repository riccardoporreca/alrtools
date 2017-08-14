# Author: Adam L. Rich
# Date:   May 15, 2012
# Description:
#
# FancyCut
#   A function that takes a vector of numbers,
#     a vector of intervals in interval notation, like [1,100] etc.
#     a vector of labels of the same length as the intervals
#
#   Like cut it returns a factor, but how it handles boundaries is more explicit
#
#   Example to use:
#
#     x <- round(runif(1000, 0, 10))
#     intervals <- c('(0,2]','(2,5)','[5,10]')
#     buckets <- c('Small','Medium','Large')
#
#     fc <- FancyCut(x, intervals, buckets)
#
#     df <- data.frame(x, fc)
#

FancyCut <- function(x, intervals, buckets, 
                     na.bucket = NA, unmatched.bucket = NA,
                     out.as.factor = TRUE) {
  
  # Make sure that intervals and buckets are the same length
  l <- length(intervals)
  if(l != length(buckets)) {
    stop('FancyCut requires a 1-1 map from intervals to buckets')
  }
  
  out <- rep(NA, length(x))
  for(index in 1:l) {
    i <- intervals[index]
    b <- buckets[index]
    n <- nchar(i[1])
    left <- substr(i, 1, 1)
    right <- substr(i, n, n)
    bounds <- strsplit(substr(i, 2, n - 1), ",")
    upper <- as.numeric(bounds[[1]][2])
    lower <- as.numeric(bounds[[1]][1])
    
    mask <- rep(FALSE, length(x))
    if(left == '[' & right == ']') {mask <- x >= lower & x <= upper}
    if(left == '[' & right == ')') {mask <- x >= lower & x <  upper}
    if(left == '(' & right == ']') {mask <- x >  lower & x <= upper}
    if(left == '(' & right == ')') {mask <- x >  lower & x <  upper}
    
    out[mask] <- b
  }
  
  out[is.na(out)]  <- unmatched.bucket
  out[is.na(x)]    <- na.bucket
  
  levels <- unique(c(buckets, na.bucket, unmatched.bucket))
  
  if(out.as.factor) {
    return(factor(
      out, 
      levels = levels,
      exclude = NULL
    ))
  } else {
    return(out)
  }
}

