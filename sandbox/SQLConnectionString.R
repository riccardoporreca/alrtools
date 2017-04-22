SQLConnectionString <- function (server, database) 
{
    switch(.Platform$OS.type, windows = {
        driver <- "SQL Server"
    }, unix = {
        driver <- "ODBC Driver 13 for SQL Server"
    }, stop("Operating System not recognised"))
    conn.string <- paste0("Driver={", driver, "};\n                       Server={", 
        server, "};\n                       Database={", database, 
        "};\n                       trusted_connection=Yes")
    return(conn.string)
}
