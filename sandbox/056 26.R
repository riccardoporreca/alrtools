# Author: Adam L. Rich
# Date:   February 19, 2013
# Description:
#
#   Project Euler
#

# Problem 26
#
#   Number of form 1/d with longest repeating part
#

as.repeating <- function(d) {
  
  r <- 1
  r.prev <- NULL
  n <- 0
  answer <- NULL
  remainders <- r
  done <- FALSE
  
  while(!done) {
    
    r.prev <- r
    
    
    q <- r.prev * 10
    r <- q %% d
    a <- (q - r) / d
    
    n <- n + 1
    if(r == 0 | r %in% remainders[1:n]) {
      answer[n] <- a
      remainders[n + 1] <- r
      done <- TRUE
    } else {
      answer[n] <- a
      remainders[n + 1] <- r
    }
    
  }
  
  # Figure length of repeating part
  r.last <- remainders[n + 1]
  len <- ifelse(r.last == 0, 0, n + 1 - which(remainders[1:n] == r.last))
  
  
  list(answer = answer, remainders = remainders, len = len)
}


x <- 1:999


lens <- sapply(x, FUN = function(n){as.repeating(n)$len})


which(lens == max(lens))