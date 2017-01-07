quarter_name <- function(d){
  m <- rep(1:4, each = 3)[as.integer(format(d, '%m'))]
  paste(format(d, '%Y'), 'q', m, sep = '')
}

year <- function(d){
  as.integer(format(d, '%Y'))
}

month <- function(d) {
  as.integer(format(d, '%m'))
}

day <- function(d){
  as.integer(format(d, '%d'))
}

quarter <- function(d){
  rep(1:4, each = 3)[Month(d)]
}

