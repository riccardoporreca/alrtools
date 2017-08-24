# Author: Adam L. Rich
# Date:   July 19, 2012
# Description:
#
#   Project Euler
#

# What is the smallest number divisible by all the numbers from 1 to 20

# Prime factorization:
#   1   = 1
#   2   = 2
#   3   = 3
#   4   = 2 * 2
#   5   = 5
#   6   = 2 * 3
#   7   = 7
#   8   = 2 * 2 * 2
#   9   = 3 * 3
#   10  = 5 * 2
#   11  = 11
#   12  = 2 * 2 * 3 
#   13  = 13
#   14  = 2 * 7
#   15  = 3 * 5
#   16  = 2 * 2 * 2 * 2
#   17  = 17
#   18  = 2 * 3 * 3
#   19  = 19
#   20  = 2 * 2 * 5

# Max power of each prime
#   2:  4
#   3:  2
#   5:  1
#   7:  1
#   11: 1
#   13: 1
#   17: 1
#   19: 1

answer <- 2^4 * 3^2 * 5 * 7 * 11 * 13 * 17 * 19

sum(GCD(answer, 1:20) != 1:20)

