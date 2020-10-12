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


context("source_env")

test_that('source_env works', {

  kode <- "
  a <- 1
  b <- 2
  print(a + b)
  d <- a * b - 15
  "

  kode_lines <- strsplit(kode, split = '\n')[[1]]

  temp_R <- tempfile(fileext = ".R")
  writeLines(kode, temp_R)
  expect_true(file.exists(temp_R))

  kode_in <- readLines(temp_R)
  expect_equal(kode_lines, kode_in)

  expect_output(env1 <- source_env(temp_R), regexp = '^\\[1\\] 3$')

  expect_equal(env1$a, 1)
  expect_equal(env1$b, 2)

  expect_false('a' %in% ls())
  expect_false('b' %in% ls())

  unlink(temp_R)

})

