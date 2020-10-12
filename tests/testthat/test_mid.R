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


context("mid")

test_that('mid works', {
  s1 <- 'abcdefghijklmnopqrstuvwxyz'
  s2 <- paste0(unlist(strsplit(s1, split = ''))[26:1], collapse = '')

  l1 <- mid(s1, c(1, 2, 3, 4, 5, 6, 7), 2)
  r1 <- c("ab", "bc", "cd", "de", "ef", "fg", "gh")

  l2 <- mid(c(s2, s1), 10, 5)
  r2 <- c('qponm', 'jklmn')

  l3 <- mid(c(s2, s1), c(25, 26), 2)
  r3 <- c('ba', 'z')

  expect_equal(l1, r1)
  expect_equal(l2, r2)
  expect_equal(l3, r3)

})

