#
# R Workshop
# 
#   Author: Adam L. Rich
#   Date:   December 5, 2014
#   Description:
#
#     Reserving in R
#   
#   Adapted from content found at
#     http://opensourcesoftware.casact.org/chain-ladder
#




# Set-up environment
setwd('P:/Desktop/LAS Orlando/')
source('LoadFunctions.R')
library(ChainLadder)




# Loss Reserving data sets 
#   http://www.casact.org/research/index.cfm?fa=loss_reserves_data
# asl17.group <- read.csv('http://www.casact.org/research/reserve_data/othliab_pos.csv')
asl17.group <- read.csv('othliab_pos.csv')




# Fields in asl17.group
#
#   GRCODE              NAIC company code (including insurer groups and single insurers)
#   GRNAME              NAIC company name (including insurer groups and single insurers)
#   AccidentYear        Accident year(1988 to 1997)
#   DevelopmentYear     Development year (1988 to 1997)
#   DevelopmentLag      Development year (AY-1987 + DY-1987 - 1)
#   IncurLoss_h1        Incurred losses and allocated expenses reported at year end 
#   CumPaidLoss_h1      Cumulative paid losses and allocated expenses at year end 
#   BulkLoss_h1         Bulk and IBNR reserves on net losses and defense and cost 
#                       containment expenses reported at year end 
#   PostedReserve97_h1  Posted reserves in year 1997 taken from the 
#                       Underwriting and Investment Exhibit - Part 2A, 
#                       including net losses unpaid and unpaid loss adjustment expenses 
#   EarnedPremDIR_h1    Premiums earned at incurral year - direct and assumed 
#   EarnedPremCeded_h1  Premiums earned at incurral year - ceded 
#   EarnedPremNet_h1    Premiums earned at incurral year - net 
#   Single              1 indicates a single entity, 0 indicates a group insurer
#




# What GROUPs are here?
sort(unique(asl17.group$GRNAME))




# Analayze and aggregate data
str(asl17.group)

asl17 <- aggregate(
  x = asl17.group[, c("IncurLoss_h1", "CumPaidLoss_h1", 
                      "BulkLoss_h1", "EarnedPremDIR_h1")], 
  by = asl17.group[, c('AccidentYear', 'DevelopmentYear', 'DevelopmentLag')], 
  FUN = sum
)




# Did the aggregation work?
# What does the data look like?
nrow(asl17.group)
head(asl17.group)

nrow(asl17)
head(asl17)

copy.table(asl17[, c('AccidentYear', 'DevelopmentYear', 'DevelopmentLag')])
table(asl17[, c('AccidentYear', 'DevelopmentYear')])
table(asl17[, c('AccidentYear', 'DevelopmentLag')])




# Note: The incurred loss data in the Excel file is the total loss reserve, 
#   and the bulk loss data is the IBNR data.
#   Therefore, to get the reported incurred data subtract the 
#   incurred loss data by the bulk loss data.
# Rename columns, too
head(asl17)

names.original <- names(asl17)
names.expected <- c("AccidentYear", "DevelopmentYear", "DevelopmentLag", "IncurLoss_h1", 
                    "CumPaidLoss_h1", "BulkLoss_h1", "EarnedPremDIR_h1")

stopifnot(all(names.expected == names.original))

names(asl17) <- c("AY", "DY", "Dev", "UltLoss", 
                  "PdLoss", "IBNR", "GPE")

asl17$IncLoss <- asl17$UltLoss - asl17$IBNR




# Turn into a triangle
#   Origin is the row names of the triangle
#   Dev is the column names of the triangle

tri.inc <- as.triangle(
  asl17[asl17$DY <= 1997, ], 
  origin = 'AY', 
  dev = 'Dev', 
  value = 'IncLoss'
)

tri.pd <- as.triangle(
  asl17[asl17$DY <= 1997, ], 
  origin = 'AY', 
  dev = 'Dev', 
  value = 'PdLoss'
)

tri.gpe <- as.triangle(
  asl17[asl17$DY <= 1997, ], 
  origin = 'AY', 
  dev = 'Dev', 
  value = 'GPE'
)




# TEST Does as.triangle aggregate for us?
tri.group <- as.triangle(
  asl17.group[asl17.group$DevelopmentYear <= 1997, ], 
  origin = 'AccidentYear', 
  dev = 'DevelopmentLag', 
  value = 'CumPaidLoss_h1'
)

# This will be all zeros if the test passes
tri.group - tri.pd




#Prints data in triangle format
tri.inc
tri.pd
(tri.os <- tri.inc - tri.pd)




# MACK CHAIN LADDER
#
#   The MackChainLadder model uses the chain ladder approach 
#   for predicting ultimate and IBNR values for each
#   row (in this case accident year) for a cumulative loss triangle. 
#   This model provides several methods for predicting the
#   ultimates, IBNRs, and the standard error of the IBNRs. 
#   The default method of the model predicts the ultimate values
#   using chain ladder ratios with the assumption of no tail factor, 
#   and the standard error of the ultimates are approximated 
#   using a log linear model. The model also has the option 
#   to use two other ratios in the place of the chain
#   ladder ratio, which are the simple average and 
#   the weighted average of the development ratios. 
#   There is the Mack method available to estimate 
#   the standard error of the ultimates, 
#   and this method is described in detail in the
#   following paper found at 
#     
#     http://www.actuaries.org/LIBRARY/ASTIN/vol29no2/361.pdf 
#
#   A tail factor can also be included in the predictions 
#   of the ultimate values, with the options of entering in 
#   the tail estimations manually or using a log linear 
#   regression for the tail estimations. Below is an example 
#   of the MackChainLadder model used to
#   predict the ultimates, IBNRs, chainladder ratios, 
#   and their standard errors of CAS "Other Liability" data.
#   
#   Note: In order to use Mack's estimation for standard error, 
#   three assumptions need to be true. These are found on
#   MackChainLadder info page in the details section, 
#   which is accessible by entering ?MackChainLadder into R.
#   
#
#
#   MackChainLadder <- function (
#     Triangle, 
#     weights = 1, 
#     alpha = 1, 
#     est.sigma = "log-linear",    
#     tail = FALSE, 
#     tail.se = NULL, 
#     tail.sigma = NULL, 
#     mse.method = "Mack") {...}
#
#   Triangle    the cumulative loss triangle
#   alpha       it is the ratio used in the prediction of ultimate values, 
#               alpha = 1 (default) is the chain ladder ratio, 
#               alpha = 0 is the simple average of the development ratios, and 
#               alpha = 2 is the weighted average of the development ratios
#   weights     the default is 1, which sets the weight for each triangle loss amount as 1
#               Alternatively, the weight for a particular entry can be set to 0 
#               to exclude loss amounts from the calculation of the chain ladder ratios. 
#               In order to do this, a separate triangle with 
#               the same dimension as the Loss triangle must be created 
#               with the desired weight values, 1 or 0, 
#               entered for each claim amount.
#   tail        can be logical or a numeric value. 
#               If tail = FALSE no tail factor will be applied (default), 
#               if tail=TRUE a tail factor will be estimated 
#               via a linear extrapolation of log(chainladderratios - 1), 
#               if tail is a numeric value(>1)
#               then this value will be used instead.
#   tail.se     See tail.sigma
#   tail.sigma  With tai.se, the standard error and the variation of each tail factor. 
#               Both are only needed if there is a tail factor and you have a 
#               numeric (>1) value to enter. Otherwise they are NULL 
#               and the model estimates them by a loglinear regression.
#   est.sig     it is the method used to estimate the standard error of the IBNRs. 
#               Default is "log-linear", or it can be estimated by Mack's method 
#               mentioned above by making the argument "Mack".
#   
#   Keep in mind by not passing in arguments, then the defaults are assumed

MackChainLadder(tri.inc)
MackChainLadder(tri.pd)
MackChainLadder(tri.gpe)

MackChainLadder(tri.inc, alpha = 0)$f   # Simple average of the dev ratios
MackChainLadder(tri.inc, alpha = 1)$f   # Chain Ladder ratio ("loss wtd average")
MackChainLadder(tri.inc, alpha = 2)$f   # Wtd Average of the dev ratios


MackChainLadder(tri.inc, alpha = c(2,2,2,1,1,1,1,1,1,1))


MackChainLadder(tri.inc)
MackChainLadder(tri.inc, tail = 1.1)
MackChainLadder(tri.inc, tail = TRUE)

MackChainLadder(tri.inc)$f
MackChainLadder(tri.inc, tail = 1.1)$f
MackChainLadder(tri.inc, tail = TRUE)$f

MackChainLadder(tri.inc)$tail
MackChainLadder(tri.inc, tail = 1.1)$tail
MackChainLadder(tri.inc, tail = TRUE)$tail

w <- matrix(1, nrow = 10, ncol = 10)
w[6, 1] <- 0

MackChainLadder(tri.inc)
MackChainLadder(tri.inc, weights = w)




# You can plot a MackChainLadder object without much effort
plot(tri.inc)
plot(cum2incr(tri.inc))
plot(MackChainLadder(tri.inc))




# Full Model Plot
#
#   Plots six different graphs
#     1. Mack Chain Ladder Results
#     2. Chain ladder developments by origin period
#     3. Standardised residuals by Fitted value
#     4. Standardised residuals by Origin Period
#     5. Standardised residuals by Calendar period
#     6. Standardised residuals by Development period
# 
#     The residual plots should be scattered with no pattern or direction for Mack's 
#     method of calculating the standard error to apply. 
#     Patterns could be a result of a trend that should be 
#     investigated further. For more information see 
#     
#       http://www.casact.org/pubs/proceed/proceed00/00245.pdf
#
plot(MackChainLadder(tri.inc))




# MUNICH CHAIN LADDER
#
#   The Munich-chain-ladder model predicts ultimate claims 
#   based on a cumulative paid and incurred claims triangle.
#   This model uses the correlation between incurred losses 
#   and paid losses to make future projections for both the total
#   paid and incurred ultimate. The "Munich" ratios are calculated 
#   using chain ladder ratios, paid/incurred ratios, and
#   the slope of the regression line in the residual plot 
#   of the incurred (or paid) losses. For a better idea on how these
#   ratios are calculated, please read this paper 
#
#     http://www.variancejournal.org/issues/02-02/266.pdf
#
#   The standard error of the incurred and paid ultimate 
#   for this model can be calculated either by a log linear regression(default),
#   or by Mack's method which is the same as in the MackChainLadder model, 
#   or a combination of the two. This model also has the option 
#   for the inclusion of a tail factor in either ultimate calculation.
#   
#
#
#   MunichChainLadder <- function(
#     Paid, 
#     Incurred, 
#     est.sigmaP = "log-linear", 
#     est.sigmaI = "log-linear", 
#     tailP = FALSE, 
#     tailI = FALSE
#   ) {...}
#
#   Paid        A cumulative paid triangle
#   Incurred    A cumulative incurred triangle
#   est.sigmaI  How the standard error for the incurred loss triangle is calculated, 
#               either "loglinear"(default) or "Mack"
#   est.sigmaP  How the standard error for the paid loss triangle is calculated, 
#               either "loglinear"(default) or "Mack"
#   tailP       Defines how the tail for the paid loss triangle is calculated, 
#               if TRUE then a log linear regression is used to
#               estimate the tail factor, or a numeric value(>1) 
#               can be entered for a tail factor. 
#               Otherwise FALSE(default) and no tail is included in the model.
#   tailI       Similar to tailP for Incurred
#   
#   Recall that if any argument is not present then the default value is assumed.
#

MunichChainLadder(Paid = tri.pd, Incurred = tri.inc)




# Full Model Plot
#
#   Plots four different graphs
#     1. Munich Chain Ladder Results
#     2. Munich Chain Ladder versus Standard Chain Ladder
#     3. Paid residual plot
#     4. Incurred residual plot
# 
plot(MunichChainLadder(tri.pd, tri.inc))



# Model Results
MuCL <- MunichChainLadder(tri.pd, tri.inc)
names(MuCL)

MuCL$MackPaid
MuCL$MackIncurred




# BOOT CHAIN LADDER
#   
#   The BootChainLadder is a model that provides a predicted distribution 
#   for the IBNR values for a claims triangle. However, this model predicts 
#   IBNR values by a different method than the previous two models. 
#   First, the development factors are calculated and then 
#   they are used in a backwards recursion to predict 
#   values for the past loss triangle. Then the predicted values 
#   and the actual values are used to calculate Pearson residuals. 
#   The residuals are adjusted by a formula specified in appendix 3 
#   in the follow paper 
#
#     http://www.actuaries.org.uk/system/files/documents/pdf/sm0201.pdf)
# 
#   Using the adjusted residuals and the predicted losses from before, 
#   the model solves for the actual losses in the Pearson formula 
#   and forms a new loss triangle. The steps for predicting past losses 
#   and residuals are then repeated for this new triangle. 
#   After that, the model uses chain ladder ratios to predict 
#   the future losses then calculates the ultimate and
#   IBNR values like in the previous Mack model. 
#   This cycle is performed R times, depending on the argument values in the
#   model (default is 999 times). The IBNR for each origin period is calculated 
#   from each triangle (the default 999) and used to form 
#   a predictive distribution, from which summary statistics 
#   are obtained such as mean, prediction error, and quantiles.
#   
#
#
#   BootChainLadder <- function (
#     Triangle, 
#     R = 999, 
#     process.distr = c("gamma", "od.pois")
#
#   Triangle        Data
#   R               the number of bootstraps(the default is 999)
#   process.distr   or the way the process error is calculated for each 
#                   predicted IBNR values with the options of
#                   "gamma"(default) and 
#                   "od.pois" (over dispersed Poisson)
#

BootChainLadder(tri.inc)


# The output has some of the same values as the Munich and Mack models did.
# The Mean and SD IBNR is the average and the standard deviation 
# of the predictive distribution of the IBNRs for each origin year

# The output also gives the 75% and 95% quantiles of 
# the predictive distribution of IBNRs, in other words 95% or 75% of
# the predicted IBNRs lie at or below the given values.


# Other stuff to talk about
#   ata
tri.inc
ata(tri.inc)

#   cum2incr
tri.inc
cum2incr(tri.inc)

#   incr2cum


#   getLatestCumulative
tri.inc
getLatestCumulative(tri.inc)

#   ClarkLDF
ClarkLDF(tri.inc)
plot(ClarkLDF(tri.inc))

# ClarkCapeCod
ClarkCapeCod(tri.inc, Premium = getLatestCumulative(tri.gpe))




