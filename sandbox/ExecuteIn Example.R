ExecuteIn <- function(f, env, ...) {
  if (!'environment' %in% class(env))
    env <- as.environment(env)
  if (identical(parent.env(env), emptyenv()))
    parent.env(env) <- globalenv()
  gox <- f
  environment(gox) <- env
  gox(...)
}

e <- new.env()
e$b <- 1

a <- function(){b}
a()
ExecuteIn(a, e)
ExecuteIn(a, list(b = 1))



