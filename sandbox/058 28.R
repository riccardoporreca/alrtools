# Author: Adam L. Rich
# Date:   February 21, 2013
# Description:
#
#   Project Euler
#

# Problem 28
#   Number spiral diagonals


i <- 1:500
x <- i * 2 + 1
s <- i * 2

sum(4 * x^2 - 6 * s) + 1