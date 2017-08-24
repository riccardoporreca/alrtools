# Author: Adam L. Rich
# Date:   February 19, 2013
# Description:
#
#   Project Euler
#

# Problem 21
#   Find the sum of all amicable numbers less than 10,000

source('Functions.R')

max <- 10000

d <- sapply(X = 1:max, FUN = function(n){sum(AllDivisors(n))})

i <- ifelse(d == 0 | d > max | d == 1:max, 1, d)

sum((1:max)[d[i] == (1:max)])
