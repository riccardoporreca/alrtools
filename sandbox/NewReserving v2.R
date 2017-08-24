# Author: Adam L. Rich
# Date:   July 22, 2015
# Description:
#
#   Preping A&E sClaimCat data for investigation in Excel
#




# Parameters
selectedFG <- 'EPL'
quarter <- '2015q2'




# Setup
source('w:/sl/actuarial/r-software/utilities/Dev/Enhanced.R')
library(ChainLadder)




# Load in sClaimCat
sClaimCat.env <- LoadToEnvironment(paste0('W:/SL/Actuarial/ClaimCategorisation_ALR/Quarters/', quarter, '/', quarter, ' sClaimCat.Rdata'))
sClaimCat <- sClaimCat.env$sClaimCat




# Add columns to sClaimCat
sClaimCat$HasMeasures <- 
  !(abs(sClaimCat$MostlikelyUSD - sClaimCat$IncurredUSD) < 5 &
      abs(sClaimCat$BlendUSD - sClaimCat$IncurredUSD) < 5 & 
      abs(sClaimCat$PessimisticUSD - sClaimCat$IncurredUSD) < 5)

sClaimCat$Count <- 1

sClaimCat$Bucket <- ifelse(sClaimCat$ClaimSeverity == 1, 'A', 'B')

sClaimCat$Key <- paste(sClaimCat$BIExposureReference, sClaimCat$BISectionReference, sClaimCat$Quarter, sep = '|')




# Field renaming
used_fields <- c("Key", 
                 "BCClaimRef", 
                 "Account name", 
                 "Claimant", 
                 "YOA", 
                 "Quarter", 
                 "dev", 
                 "ClaimSeverityInd", 
                 "ClaimStatus", 
                 "HasMeasures", 
                 "IncurredUSD", 
                 "MostlikelyUSD", 
                 "BlendUSD", 
                 "PessimisticUSD", 
                 "Count",
                 "Bucket")

new_names <- c('Key', 
               'BCClaimRef', 
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
  !is.null(sClaimCat['MostlikelyUSD']),
  all(used_fields %in% names(sClaimCat)),
  length(used_fields) == length(new_names),
  all(!duplicated(sClaimCat$Key))
)




# Get Selected Focus Group data only
sClaimCat.fg <- sClaimCat[sClaimCat$CombinedFocusArea == selectedFG, used_fields]

names(sClaimCat.fg) <- new_names 




# Define "last diagonal" and buckets 
last_diagonal <- sClaimCat.fg[sClaimCat.fg$Quarter == quarter, ]




# Merge bucket info back
last_diagonal





# Aggregate and copy
sc.agg <- aggregate(
  x = sc[, c('Incurred', 'ML', 'Blend', 'Pess', 'Count')], 
  by = sc[, c('YOA', 'Quarter', 'Dev', 'FirstDev', 'ClaimSeverity', 'ClaimStatus', 'Bucket.ClaimSeverity', 'Bucket.Incurred')], 
  FUN = sum,
  na.rm = FALSE
)



# Bucket B Triangle
sc.b <- sc[sc$Bucket.ClaimSeverity == 'B' &
             sc$Dev %% 4 == 0 &
             sc$Dev > 0, ]

triangle.b <- as.triangle(
  Triangle = sc.b, 
  origin = 'YOA', 
  dev = 'Dev', 
  value = 'Incurred'
)

triangle.b[1,1] <- 1


output <- MackChainLadder(triangle.b, tail = 1)

plot(output)



## Get Mack Output ## OUTPUT1
s <- summary(output)
ss <- s$ByOrigin
ss$YOA <- 2003:2014
ss <- ss[,c("YOA","Latest","Dev.To.Date","Ultimate","IBNR","Mack.S.E","CV(IBNR)")]
copy.table(ss)
copy.table(ss)


## Get All Output ## OUTPUT2

copy.table(sc.agg, buffer.size = 5000)

## Get Last Output
copy.table(sc.ld, buffer.size = 5000)




## Play ground

Mortgage
m <- MackChainLadder(Mortgage)
round(summary(m)$Totals["CV(IBNR)",], 2) ## 26% in Table 6 of paper

summary(m)$Totals["CV(IBNR)",]
s <- summary(m)
ss <- s$ByOrigin
ss
ss$YOA <- 2003:2011
ss <- ss[,c("YOA","Latest","Dev.To.Date","Ultimate","IBNR","Mack.S.E","CV(IBNR)")]
copy.table(ss)


output
Latest <- as.vector(getLatestCumulative(output$Triangle, na.values = NULL))
Ultimate <- as.vector(getLatestCumulative(output$FullTriangle, na.values = NULL))
IBNR <- Ultimate - Latest
MackSE <- as.vector(getLatestCumulative(output$Mack.S.E, na.values = NULL))

cbind(as.numeric(row.names(output$Triangle)), Latest, Ultimate, IBNR, MackSE)

tri <- output$Triangle
