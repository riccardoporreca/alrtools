# Author: Adam L. Rich
# Date:   February 21, 2013
# Description:
#
#   Project Euler #11
#     Largest product in a grid
#



m <- read.csv('11.matrix.csv', header = FALSE)
m <- as.matrix(m)


down <- function(i) {
  a <- (i[1] - 1) %% 20 + 1
  b <- (i[1] - a) / 20 + 1
  
  if (a > 17) return(NA)
  
  prod(m[a:(a+3), b])
}

right <- function(i) {
  a <- (i[1] - 1) %% 20 + 1
  b <- (i[1] - a) / 20 + 1
  
  if (b > 17) return(NA)
  
  prod(m[a, b:(b+3)])
}

diagonal.right <- function(i) {
  a <- (i[1] - 1) %% 20 + 1
  b <- (i[1] - a) / 20 + 1
  
  if (a > 17 | b > 17) return(NA)
  
  m[a, b] * m[a+1, b+1] * m[a+2, b+2] * m[a+3, b+3]
}

diagonal.left <- function(i) {
  a <- (i[1] - 1) %% 20 + 1
  b <- (i[1] - a) / 20 + 1
  
  if (a > 17 | b < 4) return(NA)
  
  m[a, b] * m[a+1, b-1] * m[a+2, b-2] * m[a+3, b-3]
}

x <- 1:400

max(
  sapply(x, FUN = down),
  sapply(x, FUN = right),
  sapply(x, FUN = diagonal.right),
  sapply(x, FUN = diagonal.left),
  na.rm = TRUE
)
