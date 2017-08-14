
safe_ifelse <- function (test, yes, no) {

  # This is to prepare for calling the base package function


  # Unfactor yes and no if they are factors
  if (is.factor(yes))  yes <- levels(yes)[yes]
  if (is.factor(no))   no  <- levels(no)[no]


  # The original function follows
  base::ifelse(test, yes, no)

}

safe_merge <- function(
  x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by,
  all = FALSE, all.x = all, all.y = all,
  sort = TRUE, suffixes = c(".x",".y"),
  incomparables = NULL, ...){

  # Checks to see if by.x and by.y are unique keys

  l.x <- nrow(x)
  l.y <- nrow(y)

  k.x <- nrow(as.data.frame(unique(x[,by.x])))
  k.y <- nrow(as.data.frame(unique(y[,by.y])))

  u.x <- (k.x == l.x)
  u.y <- (k.y == l.y)

  # Warn if both are not unique
  if(!u.x & !u.y){
    warning('Merge is not safe: by is not a unique key')
  }

  base::merge(x, y, by, by.x, by.y, all, all.x, all.y, sort, suffixes, incomparables, ...)

}

pmean <- function(..., na.rm = TRUE) {
  m <- cbind(...)
  apply(m, 1, mean, na.rm = na.rm)
}

pswitch <- function(EXPR, ...) {
  m <- cbind(...)
  n <- nrow(m)
  m[n * (EXPR - 1) + 1:n]
}

# read.csv0 <- Curry(read.csv, stringsAsFactors = FALSE)

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
