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


context("quarter")

test_that('quarter works', {
  expect_equal(quarter(as.Date('2020-10-14')), 4)
  expect_equal(quarter(ISOdate(1923, 12, 2)), 4)

  expect_equal(quarter(ISOdate(1923, 1, 2)), 1)
  expect_equal(quarter(ISOdate(1923, 2, 2)), 1)
  expect_equal(quarter(ISOdate(1923, 3, 2)), 1)
  expect_equal(quarter(ISOdate(1923, 4, 2)), 2)
  expect_equal(quarter(ISOdate(1923, 5, 2)), 2)
  expect_equal(quarter(ISOdate(1923, 6, 2)), 2)
  expect_equal(quarter(ISOdate(1923, 7, 2)), 3)
  expect_equal(quarter(ISOdate(1923, 8, 2)), 3)
  expect_equal(quarter(ISOdate(1923, 9, 2)), 3)
  expect_equal(quarter(ISOdate(1923, 10, 2)), 4)
  expect_equal(quarter(ISOdate(1923, 11, 2)), 4)
  expect_equal(quarter(ISOdate(1923, 12, 2)), 4)
})

