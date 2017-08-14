# Name:   Deprecated.R
# Author: Hamid Raza
# Date:   28/06/2012
# Description:
#
#   A home for missing functions
#

# 1.
# rmnorm - this was removed from an earlier package.
# Its function is for normal distribution.
# used in RDS amongst other things

rmnorm <- function(n = 1, mean = rep(0, d), varcov) {
  d <- if (is.matrix(varcov)) 
    ncol(varcov)
  else 1
  z <- matrix(rnorm(n * d), n, d) %*% chol(varcov)
  y <- t(mean + t(z))
  return(y)
}


