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


context("vlookup")

test_that('vlookup works', {
  ref <- data.frame(
    state = c('UT', 'FL', 'NY', 'CA', 'SD'),
    category = c(1, 2, 3, 3, 1)
  )

  l1 <- vlookup('CA', ref, 1, 0)
  l2 <- vlookup('CA', ref, 2, 0)
  l3 <- vlookup(c('CA', 'FL', 'KY'), ref, 2, 0)

  r1 <- 'CA'
  r2 <- 3
  r3 <- c(3, 2, NA)

  expect_equal(as.character(l1), r1)
  expect_equal(l2, r2)
  expect_equal(l3, r3)

  ref[2, 2] <- 4
  l4 <- vlookup('FL', ref, 2, 0)
  r4 <- 4
  expect_equal(l4, r4)

})


