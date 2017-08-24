# adam.rich@beazley.com
# TODO Show how to use .RProfile file

add5 <- function(n) {
  n + 5
}



add5 <- function(n) {
  return(n + 5)
}


# Bad Example
add5special <- function(n) {
  if(n == 7) return(-3)

  return(n + 5)
}



add5special <- function(n) {
  return(ifelse(n == 7, -3, n + 5))
}



round(4.3, 0)



devmask <- function(size, x, weight = 1) {
  
  weight <- rep(weight, length.out = max(x, length(weight)))
  
  m <- matrix(0, nrow = size, ncol = size)
  
  for(i in 1:size) {
    
    start <- size + 1 - i
    end <- max(size + 2 - i - x, 0)
    
    for(j in start:end) {
      m[i, j] <- weight[start - j + 1]
    }
  }
  
  return(m)
}

