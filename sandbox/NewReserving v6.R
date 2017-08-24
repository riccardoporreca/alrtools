# Author: Adam L. Rich
# Date:   July 22, 2015
# Description:
#
#   Preping A&E sClaimCat data for investigation in Excel
#




# Parameters
selectedFG <- c('US Programmes', 'US Programmes PE')
quarter <- '2015q3'
last_year <- 2013



# Setup
source('w:/sl/actuarial/r-software/utilities/Dev/Enhanced.R')
library(ChainLadder)




# General
years_display <- data.frame(YOA = (last_year - 9):last_year)
years_dev <- data.frame(YOA = (last_year - 9):last_year + 1)



# Load in sClaimCat
if (!exists('sClaimCat.env')) {
  sClaimCat.env <- LoadToEnvironment(
    paste0('W:/SL/Actuarial/ClaimCategorisation_ALR/Quarters/', quarter, '/', quarter, ' sClaimCat.Rdata'))
}

sClaimCat <- sClaimCat.env$sClaimCat
hs <- HFactory(sClaimCat)




# Add columns to sClaimCat
sClaimCat$HasMeasures <- 
  !(abs(sClaimCat$MostLikelyUSD - sClaimCat$IncurredUSD) < 5 &
      abs(sClaimCat$BlendUSD - sClaimCat$IncurredUSD) < 5 & 
      abs(sClaimCat$PessimisticUSD - sClaimCat$IncurredUSD) < 5)

sClaimCat$Count <- 1

sClaimCat$Bucket <- ifelse(sClaimCat$ClaimSeverity == 1, 'A', 'B')

sClaimCat$Key <- paste(sClaimCat$BIExposureReference, sClaimCat$BISectionReference, sClaimCat$Quarter, sep = '|')




# Field renaming
used_fields <- c("Key", 
                 "BIExposureReference",
                 "BISectionReference", 
                 "Account name", 
                 "Claimant", 
                 "YOA", 
                 "Quarter", 
                 "dev", 
                 "ClaimSeverityInd", 
                 "ClaimStatus", 
                 "HasMeasures", 
                 "IncurredUSD", 
                 "MostLikelyUSD", 
                 "BlendUSD", 
                 "PessimisticUSD", 
                 "Count",
                 "Bucket")

new_names <- c('Key', 
               "BIExposureReference",
               'BISectionReference', 
               'AccountName', 
               'Claimant', 
               'YOA', 
               'Quarter', 
               'Dev', 
               'ClaimSeverity', 
               'ClaimStatus', 
               'HasMeasures',
               'Incurred', 
               'ML', 
               'Blend', 
               'Pess', 
               'Count',
               "Bucket")




# Checks
stopifnot(
  !is.null(sClaimCat['MostLikelyUSD']),
  all(used_fields %in% names(sClaimCat)),
  length(used_fields) == length(new_names),
  all(!duplicated(sClaimCat$Key))
)




# Get Selected Focus Group data only
sClaimCat.fg <- sClaimCat[sClaimCat$CombinedFocusArea %in% selectedFG, used_fields]

names(sClaimCat.fg) <- new_names 




# Define "last diagonal" and buckets 
last_diagonal <- sClaimCat.fg[sClaimCat.fg$Quarter == quarter, ]




# Merge bucket info back
lookup <- last_diagonal[, c('BIExposureReference', 'BISectionReference', 'Bucket')]
names(lookup)[3] <- 'LastBucket'
sc <- merge(lookup, sClaimCat.fg)




# Aggregate and copy
sc.agg <- aggregate(
  x = sc[, c('Incurred', 'ML', 'Blend', 'Pess', 'Count')], 
  by = sc[, c('YOA', 'Quarter', 'Dev', 'ClaimSeverity', 'ClaimStatus', 'LastBucket')], 
  FUN = sum,
  na.rm = FALSE
)




# Bucket B Triangle
# TODO Make this work with years.dev to guarantee we get certain years in the study
sc.b <- sc[sc$LastBucket == 'B' &
             (sc$Dev %% 4) == (as.integer(substring(quarter, 6, 6)) %% 4) &
             sc$Dev > 0 &
             sc$YOA >= (last_year - 9), ]

triangle.b <- as.triangle(
  Triangle = sc.b, 
  origin = 'YOA', 
  dev = 'Dev', 
  value = 'Incurred'
)

triangle.b[is.na(triangle.b) & 
  (nrow(triangle.b) - row(triangle.b)) >= (col(triangle.b) - 1)] <- 1

triangle.b[triangle.b == 0 & 
             (nrow(triangle.b) - row(triangle.b)) >= (col(triangle.b) - 1)] <- 1

output <- MackChainLadder(triangle.b, tail = 1)




# Prepare Mack data for output
s <- summary(output)
ss <- s$ByOrigin
ss$YOA <- as.integer(rownames(ss))
ss <- ss[,c("YOA","Latest","Dev.To.Date","Ultimate","IBNR","Mack.S.E","CV(IBNR)")]




# Copy all output
copy.table(merge(years_display, ss, all.x = T))
copy.table(last_diagonal, buffer.size = 5000)
copy.table(sc.agg, buffer.size = 5000)



