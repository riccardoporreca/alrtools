EnvironmentName <- function (env) 
{
    n <- base::environmentName(env)
    if (n == "") {
        return(capture.output(env))
    }
    else {
        return(n)
    }
}
