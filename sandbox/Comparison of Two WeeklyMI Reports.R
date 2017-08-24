# Author: Adam L. Rich
# Date:   January 18, 2013
# Description:
#
#   Comparison of two WeeklyMI reports
#

setwd('\\\\w2kfileserver/Groups/CrossBusiness/BeazleyPro Reports/Weekly MI Report')
source('w:/sl/actuarial/R-Software/Utilities/Prod/SLPseudoPackage.r')


# Get a list of files
files <- dir()


# Get the most recent file and one from approx one quarter ago
MI.new <- read.csv(files[length(files)], stringsAsFactors = FALSE)
MI.old <- read.csv(files[length(files) - 13], stringsAsFactors = FALSE)


hn <- HFactory(MI.new)
ho <- HFactory(MI.old)


copy.table(unique(MI.new[, hn('product|class')]))
copy.table(unique(MI.old[, ho('product|class')]))

products <- c(
  'Beazley Breach Response', 
  'Beazley USA Miscellaneous Professional Liability', 
  'Beazley USA Tech, Media & Professional Liability')


uwcodes.old <- unique(MI.old[MI.old$`ï..ProductType` %in% products, 'UnderwriterProductCode'])
unique(MI.old[MI.old$UnderwriterProductCode %in% uwcodes.old, 'ï..ProductType'])


uwcodes.new <- unique(MI.new[MI.new$`ï..ProductType` %in% products, 'UnderwriterProductCode'])
uwcodes.new <- unique(c(uwcodes.new, uwcodes.old))
unique(MI.new[MI.new$UnderwriterProductCode %in% uwcodes.new, 'ï..ProductType'])

copy.table(uwcodes.new)