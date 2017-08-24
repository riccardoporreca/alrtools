# Author: Adam L. Rich
# Date:   July 17, 2012
# Description:
#
#   Speed tests
#

source('Functions.R')

ResetPrimes()
system.time(GetPrimes(1e5))
system.time(GetPrimes(1e6))


ResetPrimes()
system.time(GetPrimes(1e6))

ResetPrimes()
system.time(GetPrimes(2e6))

system.time(GetPrimes(1e5))

system.time(GetPrimes(1e6))

