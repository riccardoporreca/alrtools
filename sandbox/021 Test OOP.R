# Author: Adam L. Rich
# Date:   August 22, 2012
# Description:
#
#   To demonstrate OOP in R using Simon's GeneralRater4.R
#

source('w:/sl/actuarial/richad/r/SLPseudoPackage.r')
setwd('p:/desktop/OOP in R/')
library(testthat)

pM <- data.frame(
  lo = c(0.0, 0.77545, 0.98204, 0.98204),
  hi = c(0.77545, 0.98204, 0.98204, 1.0),
  mu = c(9.9233, 9.8717, 11.6108, 11.6900),
  sigma = c(2.3580, 2.0828, 1.0279, 1.6),
  start = c(0, 1e5, 1e6, 1e6),
  end = c(1e5, 1e6, 1e6, 0e0)
)

blank <- structure(
  list(lo = 0, hi = 1, mu = 0, sigma = 1, start = 0, end = Inf), 
  .Names = c("lo", "hi", "mu", "sigma", "start", "end"), 
  class = c("PiecewiseLognormal", "data.frame"), 
  row.names = c(NA, -1L)
)

blank.new <- new_PiecewiseLognormal()

expect_identical(blank.new, blank)
expect_is(blank.new, 'PiecewiseLognormal')
expect_is(blank.new, 'data.frame')
expect_is(blank, 'PiecewiseLognormal')
expect_is(blank, 'data.frame')

SimX(nclm = 10, m = pM, odf = 2.5, nyr = 2)
SimX2(nclm = 10, m = pM, odf = 2.5, nyr = 2)
SimX2(nclm = 10, m = new_PiecewiseLognormal(pM), odf = 2.5, nyr = 2)


