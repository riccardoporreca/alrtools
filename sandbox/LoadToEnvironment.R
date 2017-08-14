LoadToEnvironment <- function (RData, env = new.env()) 
{
    con <- gzfile(RData)
    on.exit(close(con))
    load(RData, env)
    return(env)
}
