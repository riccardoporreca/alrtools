a <- 'Global'
my_env <- new.env()
my_env$a <- 'my env'

b_global <- function() a

eval(b_global(), my_env)
eval(b_global, my_env)
eval(b_global, my_env)()
eval(quote(b_global), my_env)()

environment(b_global)

b_my_env <- b_global

environment(b_my_env)
environment(b_my_env) <- my_env
environment(b_my_env)

b_my_env()
