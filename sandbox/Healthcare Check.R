# Author: Adam L. Rich
# Date:   April 13, 2012
# Description:
#
#   Healthcare analysis
#
# Dependencies:
#
#   ClaimCat
#   sClaimCat
#

require(RODBC)

# Last diagonals
max.date <- max(ClaimCat$PeriodEnding)
ClaimCat.last <- ClaimCat[ClaimCat$PeriodEnding == max.date,]
sClaimCat.last <- sClaimCat[sClaimCat$PeriodEnding == max.date,]


# Are there any duplicates?
sum(duplicated(ClaimCat.last$BCClaimRef))
sum(duplicated(sClaimCat.last$BCClaimRef))

# What are the RevFocusAreas with duplicates
sClaimCat.dupes <- table(sClaimCat.last$RevFocusArea[duplicated(sClaimCat.last$BCClaimRef)])
sClaimCat.dupes[sClaimCat.dupes != 0]

# Buildup File
#   Get the counts by YOA for Healthcare
path <- 'c:/home'
combined.file <- 'Combined Overall data build up 2012 Q1 - V4 - sl and pcg - with correct clash treatment.xlsm'
combined.channel <- odbcConnectExcel2007(file.path(path, combined.file))
combined <- sqlQuery(combined.channel, "
  select [Claim Ref] as [ClaimRef], 
         [YOA]
    from [Data$]
   where [Focus area] in ('Healthcare - Misc Med','Healthcare - Hospitals, LTC & Managed Care');")
odbcClose(combined.channel)
