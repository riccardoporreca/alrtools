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


context("execute_in")

test_that('execute_in works', {
  e1 <- new.env()
  e1$a <- 5
  e1$b <- 3

  a <- -1
  b <- 10

  ab <- function() {a + b}

  expect_equal(ab(), 9)
  expect_equal(execute_in(ab, e1), 8)

})

