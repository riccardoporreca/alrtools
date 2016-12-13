copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)
}

paste.table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  return(df)
}

left <- function(string, n) {
  substring(
    text = string,
    first = 1,
    last = n
  )
}

right <- function(string, n) {

  out.len <- max(length(string), length(n))
  string <- rep(string, length.out = out.len)
  n <- rep(n, length.out = out.len)

  string.nchar <- nchar(string)

  substring(
    text = string,
    first = string.nchar - n + 1,
    last = nchar(string)
  )
}

mid <- function(string, start, end) {
  substring(string, start, end)
}

trim <- function(x) {
  gsub("(^ +)|( +$)", "", x);
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
  rep(1:4, each = 3)[month(d)]
}



