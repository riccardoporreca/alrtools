# Author: Adam Rich
# Date:   2020-05-15
# Description:
#
#   Test for function `trim`
#

# Clear environment before running tests
rm(list = ls(all = TRUE))
require(alrtools)
require(testthat)


context("trim")

test_that('trim works', {

  expect_equal(trim('   '), '')

  expect_equal(
    trim('\n\nThis is \n not something \tto remove\n'),
    'This is \n not something \tto remove')

  expect_equal(
    trim(' a lot of   spaces are here '),
    'a lot of   spaces are here')

})




