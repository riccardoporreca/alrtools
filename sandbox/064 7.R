# Author: Adam L. Rich
# Date:   July 19, 2012
# Description:
#
#   Project Euler
#

# Find the 10001st prime


source('Functions.R')

GetPrimes(2e5)

answer <- primes[10001]

as.integer(sqrt(answer))

answer

# Check
sum(GCD(1:323, answer))