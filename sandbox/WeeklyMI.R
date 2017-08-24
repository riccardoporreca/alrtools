# Author: Adam L. Rich
# Date:   March 4, 2013
# Description:
#
#   Check for changes in WeeklyMI file
#


source('w:/sl/actuarial/R-Software/Utilities/Prod/SLPseudoPackage.r')
require(RODBC)



copy.bigtable <- function(obj) {
  f <- file(description = 'clipboard-5000', open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)
}



# Get the two most recent WeeklyMI files loaded
WeeklyMI.folder <- '\\\\w2kfileserver/Groups/CrossBusiness/BeazleyPro Reports/Weekly MI Report'
WeeklyMI.file.new <- sort(dir(WeeklyMI.folder), decreasing = TRUE)[1]
WeeklyMI.file.old <- sort(dir(WeeklyMI.folder), decreasing = TRUE)[2]
WeeklyMI.path.new <- file.path(WeeklyMI.folder, WeeklyMI.file.new)
WeeklyMI.path.old <- file.path(WeeklyMI.folder, WeeklyMI.file.old)


WeeklyMI.new <- read.csv(WeeklyMI.path.new, stringsAsFactors = FALSE)
WeeklyMI.old <- read.csv(WeeklyMI.path.old, stringsAsFactors = FALSE)

hn <- HFactory(WeeklyMI.new)
ho <- HFactory(WeeklyMI.old)



# Fix formatting of dates in the WeeklyMI 
dates.tofix <- c('PeriodOfInsuranceFrom', 'PeriodofInsuranceTo', 'PolicyBindDate', 'DateCreated', 'LastModifiedDate')
for (f in dates.tofix) {
  WeeklyMI.new[, f] <- as.Date(WeeklyMI.new[, f], format = '%d-%b-%y')
  WeeklyMI.old[, f] <- as.Date(WeeklyMI.old[, f], format = '%d-%b-%y')
}
rm(f)




setdiff(hn(), ho())
setdiff(ho(), hn())