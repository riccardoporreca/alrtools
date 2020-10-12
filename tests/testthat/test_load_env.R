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


context("load_env")

test_that('load_env works', {

  pre_names <- ls(all.names = TRUE)
  n <- length(pre_names)
  new_names <- paste0('obj', 1:(n+1))
  new_obj <- new_names[!new_names %in% pre_names][1]
  x <- data.frame(letters, LETTERS, ID = 1:26)

  nenv1 <- new.env()
  assign(new_obj, x, nenv1)
  temp_RData <- tempfile(fileext = '.RData')
  save(list = new_obj, envir = nenv1, file = temp_RData)

  expect_true(file.exists(temp_RData))
  expect_false(new_obj %in% ls(all.names = TRUE))

  nenv2 <- load_env(temp_RData)

  expect_true(new_obj %in% ls(envir = nenv2))
  expect_false(new_obj %in% ls(all.names = TRUE))

  expect_equal(
    get(new_obj, envir = nenv2), x)

  unlink(temp_RData)

})

