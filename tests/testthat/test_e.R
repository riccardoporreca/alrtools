# Author: richad
# Date:   2020-05-15
# Description:
#
#   Test for function
#

# Run for interactive testing
#
#   require(testthat)
#   require(alrtools)
#

# Clear environment before running tests
rm(list = ls(all = TRUE))


context("e")

test_that('e works', {
  expect_equal(alrtools::e, exp(1L))
})

