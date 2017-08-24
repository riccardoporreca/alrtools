# Author: Adam L. Rich
# Date:   July 23, 2012
# Description:
#
#   Project Euler
#

# Problem 19
#   How many Sundays fell on the 1st of a month between Jan 1, 1901 and December 31, 2000?

# January 1, 1900 was a Monday
dates <- seq(from = as.Date('1900-1-1'), to = as.Date('2000-12-31'), by = 1)
serial <- 0:(length(dates) - 1)
mod7 <- serial %% 7

l <- dates >= as.Date('1901-1-1')

dates <- dates[l]
serial <- serial[l]
mod7 <- mod7[l]

m <- as.integer(as.character(dates, '%d')) == 1

sum(mod7 == 6 & m)