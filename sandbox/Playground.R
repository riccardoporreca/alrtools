# Author: Adam L. Rich
# Date:   March 26, 2015
# Description:
#
#   Preping A&E sClaimCat data for investigation in Excel
#



# Setup
source('c:/home/Prototype/Utilities/Dev/SLPseudoPackage.R')
load('C:/home/Prototype/2014q4 sClaimCat.Rdata')
library(ChainLadder)




# Get A&E data only
sClaimCat.ae <- sClaimCat[sClaimCat$CombinedFocusArea == 'A&E MM & PE', ]
sClaimCat.ae$HasMeasures <- 
  !(abs(sClaimCat.ae$MostLikelyUSD - sClaimCat.ae$IncurredUSD) < 5 &
  abs(sClaimCat.ae$BlendUSD - sClaimCat.ae$IncurredUSD) < 5 & 
  abs(sClaimCat.ae$PessimisticUSD - sClaimCat.ae$IncurredUSD) < 5)



# Are there claims with more than one YOA?
a <- unique(sClaimCat.ae[, c('BCClaimRef', 'YOA')])
b <- a$BCClaimRef[duplicated(a$BCClaimRef)]
sClaimCat.ae[sClaimCat.ae$BCClaimRef %in% b, ]




# Delete records with bad YOA
sClaimCat.ae2 <- sClaimCat.ae[
  !((sClaimCat.ae$BCClaimRef == 'V100YF06PNPMC0001' & sClaimCat.ae$YOA == 2005) | 
      (sClaimCat.ae$BCClaimRef == 'W15KW309PNPAC0001' & sClaimCat.ae$YOA == 2008)), ]




# Get the first development point by BCClaimRef
first.dev <- aggregate(
  x = sClaimCat.ae2[, 'dev', drop = FALSE], 
  by = sClaimCat.ae2[, c('BCClaimRef'), drop = FALSE], 
  FUN = min
)

names(first.dev) <- c('BCClaimRef', 'FirstDev')




# Merge and shorten name
# Shorten column set
ae <- merge(sClaimCat.ae2, first.dev)

n <- c("BCClaimRef", "Account name", "Claimant", "YOA", "Quarter", "dev", "FirstDev", 
       "ClaimSeverityInd", "ClaimStatus", "HasMeasures", 
       "IncurredUSD", "MostLikelyUSD", "BlendUSD", "PessimisticUSD")


ae <- ae[, n]
names(ae) <- c('BCClaimRef', 'AccountName', 'Claimant', 'YOA', 'Quarter', 'Dev', 'FirstDev', 
               'ClaimSeverity', 'ClaimStatus', 'HasMeasures',
               'Incurred', 'ML', 'Blend', 'Pess')
ae$Count <- 1




# Define "last diagonal" and buckets 
ae.ld <- ae[ae$Quarter == '2014q4', ]

ae.ld$Bucket.ClaimSeverity <- ifelse(ae.ld$ClaimSeverity == 1, 'A', 'B')
ae.ld$Bucket.Incurred <- ifelse(ae.ld$Incurred > 100000, 'A', 'B')




# Merge bucket info back
ae <- merge(ae, ae.ld[, c('BCClaimRef', 'Bucket.ClaimSeverity', 'Bucket.Incurred')])





# Aggregate and copy
ae.agg <- aggregate(
  x = ae[, c('Incurred', 'ML', 'Blend', 'Pess', 'Count')], 
  by = ae[, c('YOA', 'Quarter', 'Dev', 'FirstDev', 'ClaimSeverity', 'ClaimStatus', 'Bucket.ClaimSeverity', 'Bucket.Incurred')], 
  FUN = sum,
  na.rm = FALSE
)



copy.table(ae.agg, buffer.size = 5000)
copy.table(ae.ld, buffer.size = 5000)




# Bucket B Triangle
ae.b <- ae[ae$Bucket.ClaimSeverity == 'B' &
             ae$Dev %% 4 == 0 &
             ae$Dev > 0, ]

triangle.b <- as.triangle(
  Triangle = ae.b, 
  origin = 'YOA', 
  dev = 'Dev', 
  value = 'Incurred'
)
triangle.b[1,1] <- 1


MackChainLadder(triangle.b, tail = 1)




