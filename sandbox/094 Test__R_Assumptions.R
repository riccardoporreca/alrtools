# Author: Adam L. Rich
# Date:   August 18, 2011
# Description:
#
#   An example test
#   Also, a playground for figuring out the quirkiness of R
#

require('testthat');

# Define a context at the beginning of your tests
# This groups several tests together into a logical set
context('Assumptions About General R Behavior');

# Check equality
test_that('T is True and 1', {
  
  # 'Natural language' format
  expect_that(T, is_true());
  expect_that(as.numeric(T), equals(1));
  
  # More concise format
  expect_equal(T, TRUE);
  expect_equal(as.numeric(T), 1);
  
});

test_that('cbind returns matrix or dataframe', {

  v1 <- c( 1,   2,   3,   4,   5 );
  v2 <- c('a', 'b', 'c', 'd', 'e');
  m1 <- cbind(v1, v2);
  
  expect_equal(mode(m1), 'character');
  expect_equal(length(m1), 10);
  expect_equal(dim(m1), c(5, 2));
  
  m2 <- cbind(data.frame(v1), v2);
  
  expect_equal(class(m2), 'data.frame');
  
});

