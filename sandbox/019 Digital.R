# Author: Adam L. Rich
# Date:   July 20, 2012
# Description:
#
#   Class:Digital
#
#     Express a number as a vector in base
#     Has a base and a sign
#     Only can store integers
#     
#     If the vector has n elements
#     The ith element is the coefficient of 
#
#       base^(i - 1)
#
#     So, ten in base 2 will look like
#
#       c(0, 1, 0, 1) = 0 * 2^0 + 
#                       1 * 2^1 +
#                       0 * 2^2 +
#                       1 * 2^3

# Helper methods
new_Digital <- function(x = NULL, base = 10, length = 1) {
  # This constructor has a dual purpose
  #   1. You can use it to create a Digital from an integer
  #   2. Or you can return an empty Digital with the correct attributes
  #
  # The second purpose is for use by as.Digital.*
  #   to get the right class structure
  
  if(!is.null(x)) return(as.Digital(x, base))
  
  # If x is NULL return an empty object
  x <- rep(0, length)
  attr(x, 'sign') <- 'Positive'
  attr(x, 'base') <- base
  class(x) <- 'Digital'
  x
}

is.Digital <- function(x) {
  inherits(x, 'Digital')
}


# Generic functions
as.Digital <- function(x, base = 10, ...) {
  if(as.integer(base) != base) stop("Non-integer base encountered")
  if(base < 2) stop('Base cannot be less than 2')
  
  UseMethod('as.Digital')
}
factorial <- function(x, ...) {
  UseMethod('factorial')
}


# Specific functions
factorial.default <- base::factorial

factorial.integer <- function(x) {
  d <- as.Digital(x)
  
  if(x < 2) return(d)
  
  for(i in (x - 1):1) {
    d <- d * i
  }
  
  d
}


as.integer.Digital <- function(digital) {
  
  base <- attr(digital, 'base')
  sign <- attr(digital, 'sign')
  elements <- length(digital)
  result <- 0L
  
  for(e in 1:elements) {
    coef <- digital[e]
    result <- result + coef*base^(e - 1)
  }
  
  if(sign == 'Positive') {
    result  
  } else {
    result * -1
  }
}


# as.Digital.Digital <- function(d, base = NULL) {
#   
#   # If base is NULL, take it from d
#   og.base  <- attr(d, 'base')
#   if(is.null(base)) {
#     new.base <- og.base
#   } else {
#     new.base <- base
#   }
#   
#   # For each element in d, as.integer and rebase
#   new.d <- c(0)
#   for(i in 1:length(d)) {
#     n <- as.Digital(d[i] * og.base^(i - 1), base = new.base)
#     max.length <- max(length(n), length(new.d))
#     new.d <- c(new.d, rep(0, max.length - length(new.d)))
#     n <- c(n, rep(0, max.length - length(n)))
#     new.d <- new.d + n
#   }
#   
#   out <- new_Digital(base = new.base)
#   out[1:length(new.d)] <- new.d
#   out
# }

as.Digital.Digital <- function(d) {
  
  # Does not rebase, simply makes sure that each coefficient is less than base
  base <- attr(d, 'base')
  
  i <- 0
  while(i < length(d)) {
    
    i <- i + 1
    
    # 5 3 11 (base = 10)
    #
    # 5 3 0   +
    # 5 3 1 1 =
    #
    # 5 3 1 1
    
    if(d[i] >= base) {
      n <- as.Digital(d[i])
      len.n <- length(n)
      if(len.n + i - 1 > length(d)) {
        d <- c(d, rep(0, len.n + i - 1 - length(d)))
      }
      d[i] <- 0
      d[i:(i + len.n - 1)] <- d[i:(i + len.n - 1)] + n
    } 
  }
  out <- new_Digital(base = base)
  out[1:length(d)] <- d
  out
}

as.Digital.integer <- function(num, base = 10) {
  
  # TODO: fix all functions to handle negatives
  if(num < 0) stop('as.Digital does not support negative numbers at this time')
  
  if(length(num) > 1) {
    num <- num[1]
    warning('Vector arguments to as.Digital can only have length 1')
  }
  
  my.num <- abs(num)
  
  # If num is zero, just finish
  if(num == 0) return(new_Digital(base = base))
  
  # How long should the Digital be?
  elements <- as.integer(log(my.num) / log(base) + 1)
  
  # Get an empty object with the correct structure
  digital <- new_Digital(base = base, length = elements)
  
  
  # Get the right sign
  if(num < 0) {
    attr(digital, 'sign') <- 'Negative'  
  } else {
    attr(digital, 'sign') <- 'Positive'
  }
  
  for(e in elements:1) {
    power <- base^(e - 1)
    divisor <- as.integer(my.num / power)  
    my.num <- my.num - power * divisor
    digital[e] <- divisor  
  }
  
  digital
}

as.Digital.numeric <- function(num, base = 10) {
  num.int <- as.integer(num)
  if(num.int != num) warning("as.Digital: numeric value precision loss")
  as.Digital(num.int, base)
}

`*.Digital` <- function(x, y) {
  
  # Figure out which is which
  x.digital <- is.Digital(x)
  y.digital <- is.Digital(y)
  
  if(x.digital & y.digital) {
    stop('*.Digital only supports multiplication by scalars')
  }
  
  if(!x.digital & !y.digital) {
    stop('This should never happen')
  }
  
  if(x.digital) {
    d <- x
    s <- y
  } else {
    d <- y
    s <- x
  }
  
  if(length(s) > 1) {
    warning('*.Digital only supports multiplication by scalars')
    s <- s[1]
  }
  
  if(s < 0) {
    stop('class:Digital does not support negatives at this time')
  }
  
  base <- attr(d, 'base')
  sign <- attr(d, 'sign')
  
  d[1:length(d)] <- unclass(d) * s
  
  as.Digital(d)
  
}

# as.character.Digital <- function(d) {
#   
# }