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


context("pmean")

test_that('pmean works', {
  l1 <- pmean(1:10, 5, 20:11)
  l2 <- pmean(1:10, 5, 11:20)
  l3 <- pmean(1:10, 5, c(11:19, NA))
  l4 <- pmean(1:10, 5, c(11:19, NA), na.rm = FALSE)

  r1 <- c(
    mean(1, 5, 20),
    mean(2, 5, 19),
    mean(3, 5, 18),
    mean(4, 5, 17),
    mean(5, 5, 16),
    mean(6, 5, 15),
    mean(7, 5, 14),
    mean(8, 5, 13),
    mean(9, 5, 12),
    mean(10, 5, 11))

  r2 <- c(
    mean(1, 5, 11),
    mean(2, 5, 12),
    mean(3, 5, 13),
    mean(4, 5, 14),
    mean(5, 5, 15),
    mean(6, 5, 16),
    mean(7, 5, 17),
    mean(8, 5, 18),
    mean(9, 5, 19),
    mean(10, 5, 20))

  r3 <- c(
    r2[1:9], mean(10, 5))

  r4 <- c(
    r2[1:9], NA)

  expect_equal(l1, r1)
  expect_equal(l2, r2)
  expect_equal(l3, r3)
  expect_equal(l4, r4)

})

