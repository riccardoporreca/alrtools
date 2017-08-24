# Author: Adam L. Rich
# Date:   August 1, 2011
# Description:
#   Some examples of how to use RODBC


require(RODBC)



# An Example of how to connect to an Excel file
path <- 'P:/desktop/Test.xlsx'

channel <- odbcConnectExcel2007(xls.file = path)
whole.sheet <- sqlFetch(channel, 'Sheet1$')
named.range <- sqlFetch(channel, 'tblPhoneNumbers')
close(channel)





# This will attach to the server 'LONLIVDB05\rating'
channel <- odbcDriverConnect(connection='Driver={SQL Server};Server={LONLIVDB05\\rating};Database={tempdb};TrustedConnection=Yes')

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

# OLAP Query
channel <- odbcDriverConnection(connection='Provider=MSOLAP.3;Integrated Security=SSPI;Persist Security Info=True;Initial Catalog=BeazleyRatingMI;Data Source=lonlivdb05\rating;MDX Compatibility=1;Safety Options=2;MDX Missing Member Mode=Error')

# To clear all connections.  Important!
odbcCloseAll()



