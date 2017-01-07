load_env <- function(RData, env = new.env()){
  # con <- gzfile(RData)
  # on.exit(close(con))
  base::load(RData, env)
  return(env)
}

env_name <- function(env) {
  n <- base::environmentName(env)
  if (n == '') {
    return(capture.output(env))
  } else {
    return(n)
  }
}

clear_env <- function(pos = .GlobalEnv){
  rm(list = ls(pos), pos = pos)
}

source_env <-function(RScript, env = new.env()){
  with(env, {
    base::source(RScript, local = TRUE)
    env
  })
}
