# Author: Adam L. Rich
# Date:   July 19, 2012
# Description:
#
#   Project Euler
#

# What is the difference between the sum of squares and
#   squares of sums of the first 100 natural numbers?

nums <- 1:100

answer <- sum(nums) ^ 2 - sum(nums ^ 2)

answer

