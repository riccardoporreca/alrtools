# Author: Adam L. Rich
# Date:   July 17, 2012
# Description:
#
#   projecteuler.net
#

# Problem 3
#   The prime factors of 13195 are 5, 7, 13 and 29.
#
#   What is the largest prime factor of the number 600851475143 ?

Primes <- function(m) {
  
  if(m == 1) {
    return(c())
  }
  
  # Returns all primes less than or equal to n
  root.m <- as.integer(sqrt(m))
  
  # Build a mask to apply to seq(1:m)
  l <- rep(TRUE, m)
  
  # Exclude unity
  l[1] <- FALSE
  
  # Exclude all others
  for(x in Primes(root.m)) {
    l.head <- 1:x
    l.tail <- (x + 1):m
    l <- c(l[l.head], l[l.tail] & !!(l.tail %% x))
  }
  
  (1:m)[l]
}

n <- 600851475143
root.n <- as.integer(sqrt(n))

p <- Primes(root.n)

mods <- n %% p

max(mods)
