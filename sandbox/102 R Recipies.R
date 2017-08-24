# Author: Adam L. Rich
# Date:   August 12, 2011
# Description:
#   
#   Notes made while reading the first few chapters of 
#     R Cookbook by Paul Teetor (O'Reilly 2011)
#
#   Just some notes about how to do specific things in R
#   Also includes some things that I want to remember about R and how it works
#

# Hierarchy of type coercion
#   raw < logical < integer < real < complex < character < list 

# To show all locations where packages are installed
unique(installed.packages()[,"LibPath"])

# See the default packages installed on a machine
#   My Vanilla R shows
#   [1] "datasets"  "utils"     "grDevices" "graphics"  "stats"     "methods" 
getOption("defaultPackages")

# typeof(fun) is in {"closure","builtin","special"}
f <- function() return(0);
typeof(str)           # [1] "closure"
typeof(length)        # [1] "builtin"
typeof(c)             # [1] "special"
typeof(Sys.getenv)    # [1] "closure"
typeof(f)             # [1] "closure"
typeof(as.character)  # [1] "builtin"
typeof(return)        # [1] "special"

# Symbols to study:
#   max()
#   min()
#   q()
#   help.start()
#   args()
#   example()
#   mean()
#   sd()
#   help.search()
#   vignette()
#   data()
#   RSiteSearch()
#   pi
#   sqrt()
#   print()
#   matrix()
#   c()
#   list()
#   cat()
#   ls()
#   str()
#   ls.str()
#   rm()
#   mode()
#   mean()
#   median()
#   sd()
#   var()
#   cor()
#   cov()
#   seq()
#   rep()
#   names()
#   log()
#   sin()
#   lapply()
#   read.csv()
#   read.fwf()
#   read.table()
#   readHTMLTable()
#   readLines()
#   scan()
#   order()
#   save()
#   load()
#   dput()
#   dump()
#   length()
#   class()
#   write.csv()
#   truehist()
#   library()
#   require()
#   getwd()
#   setwd()
#   save.image()
#   history()
#   .Last.value
#   search()
#   detach()
#   head()
#   install.packages()
#   chooseCRANMirror()
#   options()
#   getOption()
#   source()
#   Sys.getenv()
#   Sys.setenv()
#   Sys.putenv()
#   data.frame()
#   edit()
#   format()
#   pnorm()
#   sink()
#   file()
#   close()
#   list.files()
#   rbind()
#   cbind()

# R Startup sequence
#   1. Execute the Rprofile.site script (R_HOME/etc/Rprofile.site)
#   2. Execute .Rprofile in working directory or
#   3. Execute .Rprofile in home directory
#   4. Loads workspace in .RData
#   5. Execute .First if it is defined
#   6. Execute .First.sys if it is defined

# Enviroment variables defining R configuration:
#   R_HOME


# Websites to use for help
#   http://rseek.org/
#   http://stats.stackexchange.com
#   http://search.r-project.org/nmz.html

# Make a table of all objects
out <- data.frame()
for(x in search()) {
  
}