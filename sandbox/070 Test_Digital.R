# Author: Adam L. Rich
# Date:   July 21, 2012
# Description:
#
#   To test Class:Digital
#

require(testthat)
source('Digital.R')

expect_true(is.Digital(new_Digital()))
expect_false(is.Digital(0))
expect_false(is.Digital(list()))

expect_error(Digitize(as.numeric(0)))

expect_equivalent(unclass(Digitize(100L, 10)), c(0, 0, 1))


