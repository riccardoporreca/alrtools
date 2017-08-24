# Author: Adam L. Rich
# Date:   February 19, 2013
# Description:
#
#   Project Euler
#

# Problem 24
#   Find the 1e6th lexical permutation of the digits 0:9

# There are factorial(10) permuations
n <- factorial(10)

# There are n/10 permutations that start with each digit
# Once we've isolated the correct band, 
#   we can split the second band into 9 bands of
#   width n/10/9, etc.
band <- rep(NA, 10)
bag <- 0:9

remainder <- 1e6L
f <- 10:1
for (d in 1:10) {

  n <- n / f[d]
  quot <- as.integer((remainder - 1) / n)
  pos <- quot + 1
  band[d] <- bag[pos]
  bag <- bag[!(bag %in% band[d])]
  remainder <- remainder - quot * n
  
}

band
