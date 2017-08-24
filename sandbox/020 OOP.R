# Author: Adam L. Rich
# Date:   August 22, 2012
# Description:
#
#   To demonstrate OOP in R using Simon's GeneralRater4.R
#

source('w:/sl/actuarial/richad/r/SLPseudoPackage.r')
setwd('p:/desktop/OOP in R/')

# SimX has the following signature
#
#   SimX <- function(nclm, m, odf, nyr = 1000) {}
#     
#     nclm    number of claims above the minimum value
#     m       loss matrix
#     odf     overdispersion factor for Neative Binomial
#     nyr     number of years to simulate
#     return  returns an "object" of type SimX
#

pM <- data.frame(
  lo = c(0.0, 0.77545, 0.98204, 0.98204),
  hi = c(0.77545, 0.98204, 0.98204, 1.0),
  mu = c(9.9233, 9.8717, 11.6108, 11.6900),
  sigma = c(2.3580, 2.0828, 1.0279, 1.6),
  start = c(0, 1e5, 1e6, 1e6),
  end = c(1e5, 1e6, 1e6, 0e0)
)

#  pM
#
#    lo      hi           mu  sigma start   end
#  1 0.00000 0.77545  9.9233 2.3580 0e+00 1e+05
#  2 0.77545 0.98204  9.8717 2.0828 1e+05 1e+06
#  3 0.98204 0.98204 11.6108 1.0279 1e+06 1e+06
#  4 0.98204 1.00000 11.6900 1.6000 1e+06 0e+00
#

# Helper methods for Piecewise Lognormal Loss Matrix class
new_PiecewiseLognormal <- function(x = NULL, pLo = 0.0, pMu = 0.0, pSigma = 1.0, pStart = 0e0) {
  
  # This constructor has three uses:
  #   1. If x is passed, it will convert x to a PiecewiseLognormal data.frame, 
  #       provided it is well-formed
  #   2. If x is not passes, but everything else is, 
  #       will build an object from pLo, pMu, etc.
  #   3. If nothing is passed, will return an object with one row
  #
  
  if(is.null(x)) {
    x <- data.frame(
      lo = pLo,
      mu = pMu,
      sigma = pSigma,
      start = pStart
    )
  }
  
  return(as.PiecewiseLognormal(x))
  
}

is.PiecewiseLognormal <- function(x) {
  inherits(x, 'PiecewiseLognormal') & 
    can.PiecewiseLognormal(x)
}

can.PiecewiseLognormal <- function(x) {
  # This returns true is x can be coerced to a PiecewiseLognormal object
  
  # First check if x is or can be a data.frame
  x.df <- NULL
  x.df <- try(as.data.frame(x), silent = TRUE)
  if(is.null(x.df)) return(FALSE)
  
  # Does x have the proper columns?
  return(sum(names(x) %in% c('lo', 'mu', 'sigma', 'start')) == 4)
  
  # TODO: should other checks be added?
  #       Like whether the rows are in order?
  #       Or whether [1, 1] == 0?
  #       etc.
}

as.PiecewiseLognormal <- function(x) {
  if(!can.PiecewiseLognormal(x)) {
    stop('x cannot be coerced to a PiecewiseLognormal data.frame: bad object')
  }
  
  # Convert x to a data frame, since we can
  x.df <- as.data.frame(x)
  n <- nrow(x)
  
  if(n < 1) {
    stop('x cannot be coerced to a PiecewiseLognormal data.frame: must have at least one row')
  }
  
  # TODO: Simon's example uses 0e0 instead of Inf
  #       Does using Inf break anything?
  if(n > 1) {
    x.df$hi   <- c(x.df$lo[2:n], 1)
    x.df$end  <- c(x.df$start[2:n], Inf)  
  } else {
    x.df$hi   <- 1.0
    x.df$end  <- Inf
  }
  
  # Append the class name to the class attribute
  #   it should have class of data.frame and PiecewiseLognormal, 
  #   at the very least
  class(x.df) <- c('PiecewiseLognormal', class(x.df))
  
  # Return only the six columns needed in the proper order
  return(x.df[, c('lo', 'hi', 'mu', 'sigma', 'start', 'end')])
}


# Generic function and member methods
SimX2 <- function(m, nclm, odf = 2.5, nyr = 1000, ...) {
  UseMethod('SimX2')
}

SimX2.default <- function(m, nclm, odf, nyr, ...) {
  print('Using SimX2.default')
  SimX(nclm, m, odf, nyr)
}

SimX2.PiecewiseLognormal <- function(m, nclm, odf, nyr, ...) {
  print('Using SimX2.PiecewiseLognormal')
  SimX(nclm, m, odf, nyr)
}

print.PiecewiseLognormal <- function(x) {
  print("I'm piecewise!")
  print.data.frame(x)
}