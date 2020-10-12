# Author: Adam Rich
# Date:   2020-10-12
# Description:
#
#   Test for function
#

# Clear environment before running tests
rm(list = ls(all = TRUE))
require(alrtools)
require(testthat)


context("year")

test_that('year works', {
  expect_equal(year(as.Date('2020-10-14')), 2020)
  expect_equal(year(ISOdate(1923, 12, 2)), 1923)
})

