# Author: Adam Rich
# Date:   2020-05-15
# Description:
#
#   Test for function `ifelse`
#   The problem with base::ifelse is that it
#   has a hard time when the branches are factors
#

# Clear environment before running tests
rm(list = ls(all = TRUE))
require(alrtools)
require(testthat)


context("ifelse")


true <- c("b", "c", "a", "b", "c", "c", "b", "c", "b", "a", "a", "a", "a", "b", "b")
truef <- factor(true, levels = c('a', 'b', 'c'))
false <- as.integer(truef)
test <- c(rep(TRUE, 7), rep(FALSE, 8))


test_that('base::ifelse has a problem when branches are factors', {

  expect_false(
    any(true == false),
    'Our branch vectors are not the same')

  expect_false(
    any(truef == false),
    'Still not the same even though true branch is now a factor')

  expect_is(base::ifelse(test, true, false), 'character')
  expect_is(base::ifelse(test, truef, false), 'integer')


  expect_equal(
    base::ifelse(test, true, false) == base::ifelse(test, truef, false),
    !test)

  expect_is(alrtools:::ifelse(test, true, false), 'character')
  expect_is(alrtools:::ifelse(test, truef, false), 'character')

  expect_equal(
    alrtools:::ifelse(test, true, false),
    alrtools:::ifelse(test, truef, false))

  expect_equal(
    alrtools:::ifelse(!test, true, false),
    alrtools:::ifelse(!test, truef, false))


  for (i in 1:10) {
    rnd <- sample(c(TRUE, FALSE), 15, replace = TRUE)
    expect_equal(
      alrtools:::ifelse(rnd, true, false),
      alrtools:::ifelse(rnd, truef, false))
  }

})


