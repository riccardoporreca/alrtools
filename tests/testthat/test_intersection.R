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


context("intersection")

test_that('intersection works', {
  v1 <- 1:5
  v2 <- 2:10
  v3 <- c("1", "2", "3")
  expect_equal(intersection(v1, v2), 2:5)
  expect_equal(intersection(v2, v3), 2:3)
  expect_equal(intersection(v3, v2), as.character(2:3))
  expect_equal(intersection(v1, v2, v3), 2:3)
})

