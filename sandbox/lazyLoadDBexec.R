lazyLoadDBexec <- function (filebase, fun, filter) 
{
    glue <- function(..., sep = " ", collapse = NULL) .Internal(paste(list(...), 
        sep, collapse))
    readRDS <- function(file) {
        halt <- function(message) .Internal(stop(TRUE, message))
        gzfile <- function(description, open) .Internal(gzfile(description, 
            open, "", 6))
        close <- function(con) .Internal(close(con, "rw"))
        if (!is.character(file)) 
            halt("bad file name")
        con <- gzfile(file, "rb")
        on.exit(close(con))
        .Internal(unserializeFromConn(con, baseenv()))
    }
    `parent.env<-` <- function(env, value) .Internal(`parent.env<-`(env, 
        value))
    existsInFrame <- function(x, env) .Internal(exists(x, env, 
        "any", FALSE))
    getFromFrame <- function(x, env) .Internal(get(x, env, "any", 
        FALSE))
    set <- function(x, value, env) .Internal(assign(x, value, 
        env, FALSE))
    environment <- function() .Internal(environment(NULL))
    mkenv <- function() .Internal(new.env(TRUE, baseenv(), 29L))
    mapfile <- glue(filebase, "rdx", sep = ".")
    datafile <- glue(filebase, "rdb", sep = ".")
    env <- mkenv()
    map <- readRDS(mapfile)
    vars <- names(map$variables)
    rvars <- names(map$references)
    compressed <- map$compressed
    for (i in seq_along(rvars)) set(rvars[i], map$references[[i]], 
        env)
    envenv <- mkenv()
    envhook <- function(n) {
        if (existsInFrame(n, envenv)) 
            getFromFrame(n, envenv)
        else {
            e <- mkenv()
            set(n, e, envenv)
            key <- getFromFrame(n, env)
            data <- lazyLoadDBfetch(key, datafile, compressed, 
                envhook)
            if (is.null(data$enclos)) 
                parent.env(e) <- emptyenv()
            else parent.env(e) <- data$enclos
            vars <- names(data$bindings)
            for (i in seq_along(vars)) set(vars[i], data$bindings[[i]], 
                e)
            if (!is.null(data$attributes)) 
                attributes(e) <- data$attributes
            if (!is.null(data$isS4) && data$isS4) 
                .Call("R_setS4Object", e, TRUE, TRUE, PACKAGE = "base")
            if (!is.null(data$locked) && data$locked) 
                .Internal(lockEnvironment(e, FALSE))
            e
        }
    }
    if (!missing(filter)) {
        use <- filter(vars)
        vars <- vars[use]
        vals <- map$variables[use]
        use <- NULL
    }
    else vals <- map$variables
    res <- fun(environment())
    map <- NULL
    vars <- NULL
    vals <- NULL
    rvars <- NULL
    mapfile <- NULL
    readRDS <- NULL
    res
}
<bytecode: 0x088ace9c>
<environment: namespace:base>
