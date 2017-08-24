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

ResetPrimes <- function() {
  primes <<- c(2L)
  attr(primes, 'm') <<- 2
}

GetPrimes <- function(x) {
  
  # primes
  #   a global object
  #   stores all primes <= m
  #   m is an attribute of primes
  m <- attr(primes, 'm')
  
  # Strategy
  #   return subset of primes if m >= x
  #   if x > m then create a bag of (m + 1):x
  #   delete stuff from the bag if they are not primes
  #   add what is left to primes
  
  # If it already exists, don't do the work again
  if(x <= m) {
    x.primes <- primes[primes <= x]
    attr(x.primes, 'm') <- x
    return(x.primes)
  }
  
  bag <- 2:x
  root <- as.integer(sqrt(x)) + 1L
  
  # Do the same for each value left in the bag that is not masked out
  # We only have to go up to the root
  i <- 1
  while(length(bag) > i & bag[i] <= root) {
    p <- bag[i]
    bag <- bag[!!(bag %% p) | bag == p]  
    i <- i + 1
  }
  
  # What remains in the bag is primes
  x.primes <- bag
  attr(x.primes, 'm') <- x
  primes <<- x.primes
  x.primes
}

ResetPrimes()

GCD <- function(x, y) {
  
  # Setup the recursion
  #   l: a vector to tell when we have got an answer for a particular pair
  #   d: the GCD
  r <- x
  n <- max(length(y), max(length(r)))
  l <- rep(FALSE, n)
  d <- rep(1, n)
  
  # The while statement works because 0 evaluates to FALSE
  #   and everything else evaluates to TRUE
  # So, if any l are FALSE, then there are TRUE !l 
  #   sum(!l) > 0 and this evals to TRUE
  # If all l are TRUE, then sum(!l) == 0 which is false
  while(sum(!l)) {
    x <- pmax(y, r)
    y <- pmin(y, r)
    r <- x %% y
    m <- r == 0 & l == FALSE
    l[m] <- TRUE
    d[m] <- y[m]
  }
  
  d
}

IsPrime <- function(x) {
  x <- x[1]
  if(x == 2) return(TRUE)
  root <- as.integer(sqrt(x)) + 1
  factors <- GetPrimes(root)
  !sum(GCD(factors, x) != 1)
}

IsDivisor <- function(n, d) {
  n <- n[1]
  (as.integer(n / d) * d) == n
}

PrimeFactorMask <- function(n) {
  
  if(n < 2) return(c(0))
  
  n <- n[1]
  all.primes <- GetPrimes(n)
  len.primes <- length(all.primes)
  mask <- rep(0, len.primes)
  log.n <- log(n)
  
  for(i in 1:len.primes) {
    p <- all.primes[i]
    max.power <- as.integer(log.n / log(p))
    p.series <- p ^ (1:max.power)
    mask[i] <- sum(IsDivisor(n, p.series))
  }
  
  mask
}

NumberOfDivisors <- function(n) {
  m <- PrimeFactorMask(n)
  m <- m[m != 0] + 1
  prod(m)
}

AllDivisors <- function(n, proper = TRUE, inc.unity = TRUE) {
  
  if (n < 0) return(c())
  if (n == 1 & !proper & inc.unity) return(1)
  if (n == 1) return(c())
  
  a <- ifelse(inc.unity[1], 1, 2)
  b <- ifelse(proper[1], n - 1, n)
  
  (a:b)[IsDivisor(n, a:b)]
}

PrimeFactors <- function(n) {
  p <- GetPrimes(n)
  m <- PrimeFactorMask(n)
  p[m > 0]
}