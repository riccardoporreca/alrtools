# Comments by:  Adam L. Rich
# Date:         September 13, 2011
# Overview:
#
#   match.fun returns a function object 
#

match.fun <- function (FUN, descend = TRUE) {

  # If FUN is already a function, just return it
  if (is.function(FUN)) 
      return(FUN)
  
  # Is FUN a character vector of length one?
  # Or is it a symbol?
  # If either, then skip
  # This includes stuff that is included in back ticks
  #
  #   f <- function()
  #   is.symbol(`f`)    # false!
  #   is.character(`f`) # false!
  #
  if (!(is.character(FUN) && length(FUN) == 1L || is.symbol(FUN))) {
    # I am not sure what substitute(substitute()) does
    FUN <- eval.parent(substitute(substitute(FUN)))
    # If FUN is not a symbol, then break
    if (!is.symbol(FUN)) {
      stop(gettextf("'%s' is not a function, character or symbol", deparse(FUN)), domain = NA)
  }
  
  # parent.frame(2) returns the grandparent of the environment for this execution
  # from ?match.fun
  #   ‘match.fun’ is not intended to be used at the top level since it
  #   will perform matching in the _parent_ of the caller.
  envir <- parent.frame(2)
  if (descend) {
    # This piece of the code throws its own error
    # Hence the reason there is no error raised here
    FUN <- get(as.character(FUN), mode = "function", envir = envir)
  } else {
    FUN <- get(as.character(FUN), mode = "any", envir = envir)
    if (!is.function(FUN)) {
      stop(gettextf("found non-function '%s'", FUN), domain = NA)
    }
  }
  return(FUN)
}
