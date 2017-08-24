# Author: Adam L. Rich
# Date:   February 21, 2013
# Description:
#
#   Project Euler
# 

# Problem 27
#   Quadratic primes


a.candidates <- -999:999
b.candidates <- GetPrimes(999)

pairs <- merge(x = a.candidates, y = b.candidates, by = c(), all = TRUE)
primes <- GetPrimes(1000)


n <- -1

while(nrow(pairs) > 1) {
  
  # Increment n (starts at 0)
  n <- n + 1
  
  # Get the value p for all pairs that are left
  p <- n^2 + pairs$x * n + pairs$y
  
  # Make sure we have all the primes less than max(p)
  if (max(primes) < max(p)) primes <- GetPrimes(max(p))
  
  
  p.prime <- p %in% primes
  
  pairs <- pairs[p.prime, ]
   
}

pairs

pairs$x * pairs$y