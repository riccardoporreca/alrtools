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

