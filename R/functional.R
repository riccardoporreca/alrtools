Curry <- function(FUN, ...) {

  # Curry
  #   To curry a function means to pre-evaluate some of the arguments
  #   So, if you have a function
  #
  #     sum <- function(a, b) {a + b}
  #
  #   And you always want b to be 1, you could define
  #
  #     add.one <- Curry(sum, b = 1)
  #
  #   Which is the same as
  #
  #     function(a) {a + 1}
  #

  # Curry works because list evaluates its arguments
  .orig = list(...);

  # The ... referenced here is the remainder
  #   of the args passed to the curried function
  #   when it is called
  function(...) do.call(FUN, c(.orig, list(...)))
}

HFactory  <- function(name, ignore.case = TRUE){
  is.function <- base::is.function(name)

  # Previously, name had to be a charater string representing the name of an object (how annoying!)
  # Because of the next line, it can now be an object!
  # Based on code of "rm" function
  name <- as.character(match.call(expand.dots = FALSE)$name)

  # If name is a function object, trail with "()"
  if(is.function) name <- paste(name, '()', sep = '')

  # Also, now it allows for a case-sensitive function
  if(ignore.case) ic = 'TRUE' else ic = 'FALSE'

  s <- paste('
             function(pattern = \'\'){
             names(', name, ')[grep(pattern, names(', name, '), ignore.case = ',ic,')]
}', sep = '')

  return(eval(parse(text = s)))
}


