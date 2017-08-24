# Author: Adam L. Rich
# Date:   July 19, 2012
# Description:
#
#   Project Euler
#

# Find the first triangle number that has over 500 divisors

source('Functions.R')

done <- FALSE
x <- 1
p <- GetPrimes(1e7)



while(!done) {
  
  # Increment x by 1
  x <- x + 1
  
  # Define y = x + 1
  y <- x + 1
  
  
  # xy/2 is a triangle number
  pfm.x <- PrimeFactorMask(x)
  pfm.y <- PrimeFactorMask(y)
  
  pfml.x <- length(pfm.x)
  pfml.y <- length(pfm.y)
  
  pfml.t <- max(pfml.x, pfml.y)
  
  pfm.t <- c(pfm.x, rep(0, pfml.t - pfml.x)) +
           c(pfm.y, rep(0, pfml.t - pfml.y)) -
           c(1, rep(0, pfml.t - 1))
  
  count <- prod(pfm.t[pfm.t != 0] + 1)
  
  print(c(x = x, t = x * (x + 1) / 2, count = count))
  
  if(count > 500) {
    done <- TRUE
  }  
  
}

print(c(x = x, t = x * (x + 1) / 2, count = count))

# > print(c(x = x, t = x * (x + 1) / 2, count = count))
# x        t    count 
# 12375 76576500      576 