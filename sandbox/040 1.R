# Author: Adam L. Rich
# Date:   July 17, 2012
# Description:
#
#   projecteuler.net
#

# Problem 1
#   If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
#   The sum of these multiples is 23.  Find the sum of all the multiples of 3 or 5 below 1000.

# Use result that 1 + 2 + . . . + n is n * (n+1) / 2

SumByN.Under1000 <- function(n) {
  m <- as.integer(999/n)
  m * (m + 1) / 2 * n
}

SumByN.Under1000(3) + SumByN.Under1000(5) - SumByN.Under1000(15)

