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


cache <- c(1)


NextTerm <- function(n) {
  if( n == 1)         return()
  if((n %% 2) == 0)   return(n/2)
  if((n %% 2) == 1)   return(3 * n + 1)
}


CollatzLength <- function(n) {
  
  if (n == 1) return(1)
  
  n.cache <- cache[n]
  
  if (!is.na(n.cache)) return(n.cache)
  
  p <- NextTerm(n)
  
  cache[n] <<- CollatzLength(p) + 1
  return(cache[n])
  
}

for (x in 1:1e6) {
  if (is.na(cache[x])) l <- CollatzLength(x)
}
