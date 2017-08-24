# Author: Adam L. Rich
# Date:   August 20, 2012
# Description:
#
#   SLPseudoPackage
#     Enhanced
#     FancyCut
#     charFunctions/StringFunctions
#
#

source('w:/sl/actuarial/richad/r/SLPseudoPackage.R')
setwd('p:/desktop')

# Enhanced.R
#   e
#     Euler's Number
e

# Enhanced.R
#   copy.csv, copy.table
#     Copies stuff to the clipboard
#     Like read.*, write.*, copy.*
head(iris)
copy.table(iris)

# Enhanced.R
#   Day, Month, Year, Quarter, QuarterName
#     Works on anything that can be coerced to Date
d <- as.Date('2012-12-31')
Day(d)
Month(d)
Year(d)
Quarter(d)
QuarterName(d)
save(d, file = "Tuesday.RData")
d <- c(d, as.Date('2011-7-1'))

# Enhanced.R
#   empty.data.frame
empty.df <- empty.data.frame(c('Field1', 'Field2', 'Field3'))
str(empty.df)
my.df <- rbind(empty.df, data.frame(
  Field1 = 1:5,
  Field2 = letters[1:5],
  Field3 = e^(1:5)
))

# Enhanced.R
#   EnvironmentName, LoadToEnvironment, SourceToEnvironment
my.env <- new.env()
my.env
base::environmentName(my.env)
EnvironmentName(my.env)

d2 <- LoadToEnvironment('Tuesday.RData')$d
LoadToEnvironment('Tuesday.RData', my.env)
my.env$d

# Enhanced.R
#   fix.header, HFactory
sClaimCat <- LoadToEnvironment('w:/sl/actuarial/ClaimCategorisation_ALR/Quarters/2012q2/sClaimCat.RData')$sClaimCat
hs <- HFactory(sClaimCat)
str(hs)
hs
hs('ref')
hs('REF')
hs('gbp')

fix.header(my.df, map = list(
  Field2 = 'Second',
  Field1 = 'First'
))

# Enhanced.R
#   NormalizePath
NormalizePath('.')
NormalizePath('p:/desktop')
NormalizePath('P:\\.\\desktop')

# FancyCut.R
#   FancyCut
x <- round(runif(1000, 0, 10))
intervals <- c('[0,0]', '(0,2]','(2,5)','[5,10]')
buckets <- c('Zero', 'Small','Medium','Large')

fc <- FancyCut(x, intervals, buckets)

df <- data.frame(x, fc)

# charFunctions.R
#   trim
my.string <- '  Leading  and  trailing  spaces   '
trim(my.string, 'Leading')
trim(my.string, 'Trailing')
trim(my.string)

# StringFunctions.R
#   right, left
my.string <- 'A longish string'
left(my.string, 1)
right(my.string, 6)

# DateHandler.R
#   is.Date, is.POSIXt
my.date <- as.Date('2012-12-31')
is.Date(my.date)
my.Date <- ISOdate(2012, 12, 31)
is.POSIXt(my.date)

# EarnedPremium3.R
#   EarnedPremium
#   r12
