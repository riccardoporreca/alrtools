curry2 <- function(FUN, ...){
  orig = as.list(match.call())
  noms = names(orig)
  if (length(orig) < 3)
    return(FUN)

  ff <- formals(FUN)
  for (i in 3:length(orig)) {
    # "a value of NULL deletes the corresponding item of the list.
    # To set entries to NULL, you need x[i] <- list(NULL)."
    if (is.null(orig[[i]])) {
      ff[noms[i]] <- list(NULL)
    } else {
      ff[[noms[i]]] <- orig[[i]]
    }
  }
  formals(FUN) <- ff
  return(FUN)
}

