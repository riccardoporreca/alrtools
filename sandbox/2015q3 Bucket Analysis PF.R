# load source file ====
source('w:/sl/actuarial/r-software/utilities/Prod/SLPseudoPackage.R')
source('w:/sl/actuarial/claimcategorisation_alr/projects/FrequencyReporting/Dev/FrequencyChartFunctionsDev.R')

#load required libraries
library(plyr)
library(gridExtra)
library(ggplot2)
library(reshape2)
library(data.table)
library(lubridate)
library(BZLYUtil)

UKsql<-paste0('SELECT * from [current].vw_UKBIData')
ukclms <- LoadFromDatabase(sourcedata="PrePeer",sqlstring=UKsql)

USsql<-paste0('SELECT * from [current].vw_USBIData')
usclms <- LoadFromDatabase(sourcedata="PrePeer",sqlstring=USsql)

clms <- rbind(ukclms, usclms)

delclmsql <- 'select * from [Current].vw_DeletedClaim'
delclms <- LoadFromDatabase(sourcedata="PrePeer",sqlstring=delclmsql)

clms <- clms[ !clms$ClaimReference %in% delclms$ClaimReference,]

clms <- clms[,c('PolicyReference','AccountName','Claimant','BeazleyClaimReference','YOA',
                'UneditedFocusArea','UneditedStatus','UneditedMostLikelyCCY',
                'UneditedPessimisticCCY','UneditedBlendWeighting','ClaimsCategory','CCY','FlaggedClaimIndicator',
                'BeazleyPaidCCY','BeazleyOSCCY')]

mapsql <- 'select ClaimReference, PolicyReference, OverallFocusArea, CombinedFocusArea from [Current].vw_OverallBuildUp'
clmmap <- usclms <- LoadFromDatabase(sourcedata="PrePeer",sqlstring=mapsql)



# how many claims are categorised
cattab <- table(clms$YOA, clms$ClaimsCategory, useNA='always')


# assign buckets
# sort out NA
clms[ is.na(clms$ClaimsCategory),'ClaimsCategory'] <- 4
table(clms$ClaimsCategory)

lawyers <- c('PI-Lawyers exBLPT','Mid Market Lawyers','BICI Lawyers MM','BUSA Lawyers MM')

#sort out non-lawyers buckets
clms[ clms$ClaimsCategory == 1 
      & !clms$UneditedFocusArea %in% lawyers,"Bucket"] <- 'A'   
clms[ !clms$ClaimsCategory == 1
      & !clms$UneditedFocusArea %in% lawyers,"Bucket"] <- 'B'
table(clms$Bucket, useNA='always')

clms$incurredCCY <- clms$BeazleyOSCCY + clms$BeazleyPaidCCY

#sort out lawyers buckets
clms[ (!is.na(clms$FlaggedClaimIndicator) | clms$incurredCCY > 2.5e6)
      & clms$UneditedFocusArea %in% lawyers,"Bucket"] <- 'A'   # everything that's Cat1 and Flagged is Bucket A for lawyers
clms[ is.na(clms$Bucket),'Bucket'] <- 'B'  # Everything else is Bucket B
table(clms$UneditedFocusArea, clms$Bucket, useNA='always')

# join clms with clmmap to get pre-peer focus areas for aggregation

CLMS <- as.data.table(clms)
CLMMAP <- as.data.table(clmmap)

setkey(CLMS, BeazleyClaimReference, PolicyReference)
setkey(CLMMAP, ClaimReference, PolicyReference)

DATA <- CLMMAP[CLMS]

DATA$Count <- 1

# Point 3 - Claim Count by Focus Area By Bucket
copy.table(aggregate(Count ~ YOA + CombinedFocusArea + Bucket + UneditedStatus, DATA, length))

#sort out pre-peer fields to numeric
DATA$UneditedMostLikelyCCY <- as.numeric(DATA$UneditedMostLikelyCCY)
DATA$UneditedPessimisticCCY <- as.numeric(DATA$UneditedPessimisticCCY)

DATA$HasMeasure <- 'Yes' # set all to yes but then set to No is either ML or Pess NA
DATA[ is.na(DATA$UneditedMostLikelyCCY) & is.na(DATA$UneditedPessimisticCCY),
      "HasMeasure"] <- 'No'

# now we know what has measure from the base data we can overwrite NA with zero to calc
# the volatility, assuming that if pess > 0 and ML = NA then ML = 0

DATA[ is.na(DATA$UneditedMostLikelyCCY), "UneditedMostLikelyCCY"] <- 0
DATA[ is.na(DATA$UneditedPessimisticCCY), "UneditedPessimisticCCY"] <- 0

DATA$Volatility <- 0
setDT(DATA)[HasMeasure=='Yes', 
            Volatility:= UneditedPessimisticCCY - 
                             (UneditedMostLikelyCCY + UneditedBlendWeighting * 
                                (UneditedPessimisticCCY - UneditedMostLikelyCCY))]

# Point 3 - Claim Count by Focus Area By Bucket
copy.table(aggregate(Count ~ YOA + UneditedFocusArea + Bucket + UneditedStatus + HasMeasure, DATA, length),buffer.size=999)
copy.table(aggregate(Volatility ~ YOA + UneditedFocusArea + Bucket + UneditedStatus + HasMeasure, DATA, sum),buffer.size=999)
