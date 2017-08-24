#
# R Workshop
# 
#   Author: Adam L. Rich
#   Date:   December 5, 2014
#   Description:
#  
#     Getting Data In and Out
#  
#   Copyright Notice:
#     THE FOLLOWING APPLIES TO THIS ENTIRE FILE
#       AND/OR ANY PORTION THEREOF
#     COPYRIGHT ADAM L. RICH, 2014
#     THIS HAS BEEN DISTRIBUTED AS PART OF THE 2014 CAS LIMITED ATTENDANCE SEMINAR: INTRO TO R
#     YOU CAN USE CODE HERE IN YOUR OWN WORK 
#       PROVIDED YOU DO NOT TAKE CREDIT FOR IT
#     YOU AGREE TO NOT POST THIS IN ANY WAY ON THE INTERNET
#     YOU CAN DISTRIBUTE TO THOSE YOU TRUST TO FOLLOW THESE RULES
#     THIS MESSAGE MUST STAY WITH THIS CODE
#     


# Setting the working directory allows you to keep paths short
setwd('P:/Desktop/LAS Orlando/')
library(RODBC)



source('./LoadFunctions.R')
source('./Functions.R')



# RData files are the native way of saving data
# They are basically just gzipped binary data
my.iris <- iris
save(my.iris, file = 'iris.RData')


# Will now remove to show that loading really works
rm(my.iris)
ls()  # It is missing


# Load the data into the .GlobalEnv
load('iris.RData')
ls()  # Back again



# Suppose we did an analysis in two consecutive quarters
# And we want to compare results
# In 2012q4 we stored my.df in '2012q4.RData'
# In 2013q1 we stored my.df in '2013q1.RData'

# 2012q4 Processing
my.df <- data.frame(
  USD = 1.6,
  CAD = 1.05,
  EUR = 1.2,
  GBP = 1.0
)
save(my.df, file = '2012q4.RData')
rm(my.df)

# 2013q1 Processing
my.df <- data.frame(
  USD = 1.58,
  CAD = 1.06,
  EUR = 1.3,
  GBP = 1.0
)
save(my.df, file = '2013q1.RData')
rm(my.df)

# Loading using load now causes a problem
# We cannot compare!
load('2012q4.RData')
load('2013q1.RData')
# The second load statement overwrites the object created by the first

# Of course, there is a way around this
load('2012q4.RData')
my.df.old <- my.df
load('2013q1.RData')
my.df.new <- my.df

# But that is a little messy
# Plus, the RData files could have any object 
#   from the old working environment in them
rm(list = ls(all = T))  # Just to clean up


# Instead use something like LoadToEnvironment
my.df.old <- LoadToEnvironment('2012q4.RData')$my.df
my.df.new <- LoadToEnvironment('2013q1.RData')$my.df






# CSV files are another very convenient way of getting data into and out of R
# Although, they can be bulky because they do not have any native compression
#   like RData and XLSX files do
AutoBI <- read.csv('AutoBI.csv')
str(AutoBI)


# You have to be careful with the formatting of 
#   data in CSV files that are saved from Excel
AutoBI.US <- read.csv('AutoBI (US Dates).csv')
str(AutoBI.US)

# I've added a fake date to this file
# read.csv() automatically treats this as a factor on load
# You can supress that by using
AutoBI.US <- read.csv('AutoBI (US Dates).csv', stringsAsFactors = FALSE)
str(AutoBI.US)

# But say you want to convert the field to a date now
as.Date(AutoBI.US$DATE)

# You have to pass a format string
as.Date(AutoBI.US$DATE, format = '%d/%b/%Y')

# That wasn't the right one, try again
as.Date(AutoBI.US$DATE, format = '%m/%d/%Y')
# Much better!


# So much easier to just use an unambiguous format
AutoBI <- read.csv('AutoBI (yyyy-mm-dd).csv', stringsAsFactors = FALSE)
str(AutoBI)
AutoBI$DATE <- as.Date(AutoBI$DATE)


# In addition to dates you have to watch
#   Accounting style numbers
#   Trailing negatives
#   Loss of precision due to rounding
#
# I get around these issues by removing all formatting
#   and then formatting date files with the custom picture
#
#   yyyy-mm-dd
#



# Another option is reading data directly from Excel using ODBC
#
#   # Remember to do this if you haven't already!
#   # It doesn't hurt to call it twice
#   library(RODBC)
#

channel <- odbcConnectExcel2007('RatingTables.xlsx')
base.rates <- sqlFetch(channel, sqtable = 'Base Rates')
agg.ILFs <- sqlFetch(channel, sqtable = 'ILFs')

# That didn't work
agg.ILFs <- sqlFetch(channel, sqtable = 'tblAggregateILFs')

# Always remember to close!
close(channel)



# I've created a couple of functions to make this cleaner
#   Don't have to create a channel
#   Don't have to close the channel
#   Everything is in one line
GetTablesExcel2007('RatingTables.xlsx')
ret.fact <- SheetToDataFrame('RatingTables.xlsx', 'tblLiabilityRetention')
SheetToDataFrame('RatingTables.xlsx', 'ILFs')
SheetToDataFrame('RatingTables.xlsx', "Base Rates")




# Say you do not have authority to copy or alter data
#   Maybe it is a general ledger report
#   And your company doens't want you to make copies
GetTablesExcel2007('Accounting Data.xlsx')
acc <- SheetToDataFrame('Accounting Data.xlsx', 'Ledger$')

acc.clean <- acc[(max(which(is.na(acc[, 1]))) + 1):nrow(acc), ]
names(acc.clean) <- c("AccountingDate", 
                      "Level", "Measure", "LOB", "Amount")

as.numeric(acc.clean$Amount)
acc.clean$Amount <- CNumeric(acc.clean$Amount)




# Other ways to get data in and out
# In interactive mode (like this), 
#   use the clipboard!
# 
#   copy.table
#   paste.table
#
# Not good for production level or non-interactive work
# But great for quick and dirty pushing and pulling of data
#



# Working with data sets with lots of columns
huge <- read.csv('HugeData.csv')
ls(huge)  # Number of columns
# Huge has a lot of columns
# Searching can be a pain
# Create a search function to help with that
hh <- function(pattern = ''){
  names(huge)[grep(pattern, names(huge), ignore.case = TRUE)]
}

hh('internet')

# HFactory creates this function for you
hh <- HFactory(huge)




# In reserving later we will see an example of pulling data directly from the web
# Also good to know how to use dput, which we go over then, too

# Thanks!




