# Author: Adam Rich
# Date:   2020-10-12
# Description:
#
#   Test for function
#

# Running this test by itself is fine
# When you run it via devtools it fails with this weird error
# "use of NULL environment is defunct"
# See
# https://community.rstudio.com/t/confusing-error-in-as-environment-where-using-as-environment-null-is-defunct-with-using-devtools-document/12804


# # Clear environment before running tests
# rm(list = ls(all = TRUE))
# require(alrtools)
# require(testthat)
#
#
# context("curry")
#
# test_that('curry works', {
#
#   add5 <- curry(sum, 5)
#   expect_equal(add5(10), 15)
#   add4 <- curry(sum, 1, 1, 1, 1)
#   expect_equal(add4(0), 4)
#
#   left5 <- curry(left, n = 5)
#   expect_equal(
#     left5(c('1234567890', 'abcdefg')),
#     c('12345', 'abcde'))
#
#   pn <- curry(paste, sep = '\n')
#
#   expect_equal(pn('a', 'b', 'c'), 'a\nb\nc')
#
# })
#
