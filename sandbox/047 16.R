# Author: Adam L. Rich
# Date:   July 23, 2012
# Description:
#
#   Project Euler
#

# Problem 16
#   What is the sum of the digits of 2^1000?

source('Digital.R')

d <- as.Digital(1)

for(x in 1:1000) {
  d <- d * 2
}

sum(d)
