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


context("matrix2latex")

test_that('matrix2latex works', {
  m <- datasets::iris[1:5, 1:4]
  expected_many <- "\\begin{bmatrix}
5.1 & 3.5 & 1.4 & 0.2 \\\\
4.9 & 3 & 1.4 & 0.2 \\\\
4.7 & 3.2 & 1.3 & 0.2 \\\\
4.6 & 3.1 & 1.5 & 0.2 \\\\
5 & 3.6 & 1.4 & 0.2
\\end{bmatrix}"
  expected_one <- gsub('\n', ' ', expected_many)

  tex <- matrix2latex(m, print = FALSE)
  expect_equal(expected_one, tex)
  tex <- matrix2latex(m, print = FALSE, one_line = FALSE)
  expect_equal(expected_many, tex)
})

