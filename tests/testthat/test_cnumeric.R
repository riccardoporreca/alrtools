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

context("cnumeric")


test_that('cnumeric works', {

  in1 <- c(
    '1,000,000', '2,000.03',
    '1,0,0', '2,0000.03',
    '(1,000.92)', '(4)',
    '28.3%', '-1.3%', '(15%)',
    '3e7', '5e-1')

  ex1 <- c(
    1e+06, 2000.03,
    100, 20000.03,
    -1000.92, -4,
    0.283, -0.013, -0.15,
    3e+07, 0.5)

  in2 <- factor(c(
    '1,000', '1', '-234',
    '-14%', '(12.3%)', '1,000',
    '1', '-14%', NA,
    '-234', '(234)'
  ))

  ex2 <- c(
    1000, 1, -234,
    -0.14, -0.123, 1000,
    1, -0.14, NA,
    -234, -234)

  in3 <- c('USD 0.10', '$14.34')
  ex3 <- c(NA_real_, NA_real_)

  in4 <- c(
    ' 1,000,000   ', '\n2,000.03\t',
    '1,0,0\t\t\t\t', '\t\t\t  2,0000.03 ',
    '\r\n(1,000.92)\n\r', '(  4) ',
    '\n28.3%\r', '-1.3%', '  (  15%  )  ',
    '  3e7\t', '  5e-1 ')

  ex4 <- c(
    1e+06, 2000.03,
    100, 20000.03,
    -1000.92, -4,
    0.283, -0.013, -0.15,
    3e+07, 0.5)



  expect_equal(cnumeric(in1), ex1)
  expect_equal(cnumeric(in2), ex2)
  expect_equal(
    suppressWarnings(cnumeric(in3)), ex3)
  expect_warning(cnumeric(in3[1]))
  expect_warning(cnumeric(in3[2]))
  expect_equal(cnumeric(in4), ex4)

})

