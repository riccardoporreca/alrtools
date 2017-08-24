# Author: Adam L. Rich
# Date:   February 19, 2013
# Description:
#
#   Project Euler
#

# Problem 25
#   What is the first Fibonacci number with 1000 digits

source('Digital.R')

F1 <- as.Digital(1)
F2 <- F1


prior2 <- F1
prior1 <- F2
current <- F2 + F1
count <- length(current)
n <- 3


while (count < 1000) {
  n <- n + 1
  prior2 <- prior1
  prior1 <- current
  current <- as.Digital(prior1 + prior2)
  count <- length(current)
}

n
