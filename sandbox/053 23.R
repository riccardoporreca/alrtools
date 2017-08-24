# Author: Adam L. Rich
# Date:   February 19, 2013
# Description:
#
#   Project Euler
#

# Problem 23
#  A perfect number is a number for which the sum of its 
#  proper divisors is exactly equal to the number. For example, 
#  the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, 
#  which means that 28 is a perfect number.
#  
#  A number n is called deficient if the sum of its proper divisors is 
#  less than n and it is called abundant if this sum exceeds n.
#  
#  As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, 
#  the smallest number that can be written as the sum of two abundant 
#  numbers is 24. By mathematical analysis, it can be shown that all 
#  integers greater than 28123 can be written as the sum of two abundant numbers. 
#  However, this upper limit cannot be reduced any further by analysis even 
#  though it is known that the greatest number that cannot be expressed as 
#  the sum of two abundant numbers is less than this limit.
#  
#  Find the sum of all the positive integers which cannot be written 
#  as the sum of two abundant numbers.

source('Functions.R')

max <- 28123

d <- sapply(X = 1:max, FUN = function(n){sum(AllDivisors(n))})

class <- ifelse(d == 1:max, 'Perfect', NA)
class <- ifelse(d < 1:max,  'Deficient', class)
class <- ifelse(d > 1:max,  'Abundant', class)

abundant   <- (1:max)[class == 'Abundant']
perfect    <- (1:max)[class == 'Perfect']
deficient  <- (1:max)[class == 'Deficient']

x <- rep(0, max)

a <- abundant
for (r in max:1) {
  a <- a[a < r]
  x[r] <- ifelse(any((r - a) %in% a), 1, 0)
}

sum((1:max)[x == 0])