# Author: Adam L. Rich
# Date:   July 17, 2012
# Description:
#
#   projecteuler.net
#

# Problem 3
#   A palindromic number reads the same both ways. 
#   The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 99.
#
#   Find the largest palindrome made from the product of two 3-digit numbers.

largest <- 999 * 999
smallest <- 111 * 111

stems <- 111:999

fives <- (function(){  
  first <- as.integer(stems / 100)
  second <- as.integer(stems / 10) %% 10
  stems * 100 + second * 10 + first
})()
  
sixes <- (function(){
  first <- as.integer(stems / 100)
  second <- as.integer(stems / 10) %% 10
  third <- stems %% 10
  stems * 1000 + third * 100 + second * 10 + first
})()

all <- c(fives, sixes)

all <- all[order(all, decreasing = TRUE)]

factors <- 111:999

done <- FALSE
x <- 0
while(!done) {
  x <- x + 1
  candidate <- all[x]
  done <- !!sum((candidate / factors[GCD(candidate, factors) == factors]) %in% factors)
}

candidate

# Answer is 993 * 913 = 906609
