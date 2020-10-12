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


context("left")

test_that('left works', {

  s1 <- 'abcdefghijklmnopqrstuvwxyz'
  s2 <- paste0(unlist(strsplit(s1, split = ''))[26:1], collapse = '')

  l1 <- left(s2, c(1, 2, 3, 4, 5, 6, 7))
  r1 <- c("z", "zy", "zyx", "zyxw", "zyxwv", "zyxwvu", "zyxwvut")

  l2 <- left(c(s2, s1), 1)
  r2 <- c('z', 'a')

  l3 <- left(c(s2, s1), c(2, 3))
  r3 <- c('zy', 'abc')

  expect_equal(l1, r1)
  expect_equal(l2, r2)
  expect_equal(l3, r3)

})

