# Author: Adam L. Rich
# Date:   August 1, 2011
# Description:
#
#   Some examples of how to use RODBC
#



# This will attach to the server 'SERVERGROUP\SERVER'
channel <- odbcDriverConnect(
  connection='Driver={SQL Server};Server={SERVERGROUP\\SERVER};Database={tempdb};TrustedConnection=Yes'
)



# An example of how to get data into R
sqlQuery(channel, 'select name from sys.tables;')



# This function will remember how to write the connection string for you
SQLServerConnection <- function(server, database) { 
  c <- paste('Driver={SQL Server};Server={',
             server,
             '};Database={',
             database,
             '};TrustedConnection=Yes', sep='')
  odbcDriverConnect(connection=c)
}



# it is called like this
channel <- SQLServerConnection('.','tempdb')
sqlQuery(channel, 'select name from sys.servers;')



# To clear all connections.  Important!
odbcCloseAll()
