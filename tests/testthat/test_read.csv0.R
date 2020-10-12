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


context("read.csv0")

test_that('read.csv0 works', {
  temp_CSV <- tempfile(fileext = ".csv")
  write.csv(iris, file = temp_CSV, row.names = FALSE)
  expect_true(file.exists(temp_CSV))

  with_factors <- read.csv(temp_CSV, stringsAsFactors = TRUE)
  without_factors <- read.csv0(temp_CSV)

  expect_equal(class(with_factors$Species), 'factor')
  expect_equal(class(without_factors$Species), 'character')

  unlink(temp_CSV)

})

