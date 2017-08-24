# Author: Adam L. Rich
# Date:   October 10, 2011
# Description:
# 
#   Tools for SQL Server ODBC
#

require(RODBC)

GetConnectionString <- function(server = '.', db = 'tempdb') {
  paste('Driver={SQL Server};Server={',server,'};Database={',db,'};TrustedConnection=Yes', sep='')
}

GetConnection <- function(server = '.', db = 'tempdb') {
  odbcDriverConnect(connection = GetConnectionString(server, db))
}

RunSQL <- function(sql, server = '.', db = 'tempdb', stringsAsFactors = FALSE){
  conn <- GetConnection(server, db)
  result <- sqlQuery(conn, sql, stringsAsFactors)
  close(conn)
  return(result)
}

