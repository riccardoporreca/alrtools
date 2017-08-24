# Author: Adam L. Rich
# Date:   July 19, 2012
# Description:
#
#   Project Euler
#

# Find the product of the Pythagorean triple that sums to 1000

# a^2 + b^2 = c^2
#
# Assume that a > b
#   We know that a cannot equal b because sqrt(2) is irrational
#
# If b = 1 then c is just barely bigger than a so a has an upper bound at 500
# If b = a - 1 then c is approx sqrt(2) * a
# This gives a lower upper bound, so the first case is more restrictive
#
# Therefore we can say that a, b, and c are all less than 500

nums <- 1:500
squares <- nums ^ 2

m <- matrix(data = 0L, nrow = 500, ncol = 500)
r <- matrix(data = 0L, nrow = 500, ncol = 3)
count <- 0

for(x in nums) {
  m[, x] <- squares[x] + squares
}

c <- sqrt(m)

as.integer(c) == c

for(x in 1:500) {
  for(y in x:500) {
    if(as.integer(c[x, y]) == c[x,y]) {
      count <- count + 1
      r[count, ] <- c(x, y, c[x, y])
    }
  }
}

r <- r[1:count, ]

triple <- r[rowSums(r) == 1000, ]

answer <- prod(triple)