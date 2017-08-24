# Author: Adam L. Rich
# Date:   July 23, 2012
# Description:
#
#   Project Euler
#

# Problem 17
#   How many letters are required to write out all numbers from 1 through 1000

source('Digital.R')

as.English <- function(x, ...) UseMethod('as.English')

as.English.integer <- function(x) {
  
  if(x < 1 | x > 1000) stop('x must be between 1 and 1000 inclusive')
  if(x == 1000) return('one thousand')
  
  singles <- c('', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine')
  ties <- c('', '', 'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety')
  teens <- c('ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen')
  
  d <- new_Digital(length = 3)
  d[1] <- 1
  d <- d * x
  
  has.hundreds  <- (d[3] > 0)
  has.tens      <- (d[2] > 0)
  has.ones      <- (d[1] > 0)
  
  # one 
  # hundred
  # and
  # eighty
  # five
  
  s <- rep(NA, 5)
  
  if(has.hundreds)                    s[1] <- singles[d[3] + 1]
  if(has.hundreds)                    s[2] <- 'hundred'
  if(has.hundreds & sum(d[1:2]) > 0)  s[3] <- 'and'
  if(d[2] == 1)                       s[4] <- teens[d[1] + 1]
  if(d[2] > 1)                        s[4] <- ties[d[2] + 1]
  if(has.ones & d[2] != 1)            s[5] <- singles[d[1] + 1]
  
  s <- s[!is.na(s)]
  
  paste(s, collapse = ' ')
}

# Get the answer
letter.count <- 0
for(x in 1L:1000L) {
  s <- as.English(x)
  s <- strsplit(s, ' ')
  letter.count <- letter.count + sum(nchar(s[[1]]))
}

letter.count