# Author: Adam L. Rich
# Date:   July 24, 2012
# Description:
#
#   Project Euler #14
#
#     Pick a number n to start a sequence
#     The next number in a sequence is
#   
#       n/2 if n is even
#       3n + 1 if n is odd
#
#     The sequence will stop once we get to 1
#
#     What value of n (less than 1m) 
#       gives the longest sequence?
#

require(memoise)

new_Sequence <- function() {
  x <- rep(NA, 100)
  attr(x, 'grow-by') <- 100
  attr(x, 'elements') <- 0
  class(x) <- 'Sequence'
  x
}

Push <- function(x, ...) UseMethod('Push')

Push.Sequence <- function(x, n) {

  # What is the physical length of x?
  l <- length(unclass(x))
  g <- attr(x, 'grow-by')
  
  # Increment the logical length by 1
  e <- attr(x, 'elements') + 1
  attr(x, 'elements') <- e
  
  # Before assignment, make sure we have enough physical storage space!
  while(l < e) {
    x[(l + 1):(l + g)] <- NA
    l <- length(unclass(x))
  }
  x[e] <- n
  x
}

print.Sequence <- function(x) {
  print(x[1:attr(x, 'elements')])
}

length.Sequence <- function(x) {
  attr(x, 'elements')
}

NextTerm <- function(n) {
  if( n == 1)         return()
  if((n %% 2) == 0)   return(n/2)
  if((n %% 2) == 1)   return(3 * n + 1)
}

CollatzSequence <- function(n) {
  if(n == 1) {
    s <- new_Sequence()
    return(Push(s, 1))
  }
  Push(m.CollatzSequence(NextTerm(n)), n)
}

m.CollatzSequence <- memoise(CollatzSequence)

m.CollatzSequence(200)
system.time(m.CollatzSequence(1e6))

results <- rep(NA, 1e6 - 1)
names(results) <- 1:(1e6 - 1)
for(x in 1:(1e6 - 1)) {
  results[x] <- length(m.CollatzSequence(x))
}

