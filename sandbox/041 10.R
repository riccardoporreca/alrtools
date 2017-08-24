# Author: Adam L. Rich
# Date:   July 19, 2012
# Description:
#
#   Project Euler
#

# Calculate the sum of all the primes below 2m

source('Functions.R')

answer <- sum(as.numeric(GetPrimes(2e6)))


