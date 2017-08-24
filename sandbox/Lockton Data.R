# Author: Adam L. Rich
# Date:   September 8, 2014
# Description:
#
#   To do a GLM analysis on the Lockton data
#



# Setup analysis
source('W:/SL/Actuarial/R-Software/Utilities/Dev/SLPseudoPackage.R')
setwd('P:/home/projects/2014/Lockton Rater/')



# Load in data
policies <- read.csv('Benchmarking Data.csv')



# Summarize
summary(policies)
head(policies)
str(policies)



# Premium adjusted for State
policies$PremiumAdj.CA <- ifelse(
  policies$California == 'Yes', 
  policies$Premium / 1.10,
  policies$Premium
)



# Premium before FirstParty
policies$PremiumAdj.FP <- ifelse(
  policies$FirstParty == 'Yes',
  policies$PremiumAdj.CA / 1.10,
  policies$PremiumAdj.CA
)



# Let the guideline deductible be a function of the Limit,
#   1% of Limit
policies$GuidelineRetention <- policies$Limit * 0.01
policies$RetentionRatio <- policies$Retention / policies$GuidelineRetention



# How many policies are there by Hazard Group?
table(policies$HazardClass)
table(policies[, c('HazardClass', 'RetentionRatio')])






# What should the ILFs be?  
# Use class 5 with retention ratio of 1.0
model.ilf <- glm(
  log(PremiumAdj.FP) ~ log(Revenue) + log(Limit), 
  data = policies[policies$HazardClass == 'Class 5' & 
                  policies$RetentionRatio == 1.0, ]
)
summary(model.ilf)



# What ILFs does model.ilf imply?
limits <- c(250e3, 500e3, 1e6, 2e6, 3e6, 4e6, 5e6)
ilf.power <- model.ilf$coefficients['log(Limit)']
ilf <- function(limit) {round(25 * (limit / 1e6) ^ ilf.power, 0) / 25}

ilfs <- data.frame(
  Limit = limits,
  ILF = ilf(limits)
)



# Adjust premium for ILFs
policies$LimitFactor <- ilf(policies$Limit)
policies$PremiumAdj.ILF <- policies$PremiumAdj.FP / policies$LimitFactor




# What should the base rate factors be?
model.base10 <- glm(
  log(PremiumAdj.ILF) ~ log(Revenue) + log(Retention) + HazardClass, 
  data = policies[policies$Retention == 10000, ]
)
summary(model.base10)



# Going from HC1 to HC5 adds roughly 16%
exp(model.base10$coefficients['HazardClassClass 5'])



# Assume that going up a class always adds an additional 4% to the HC1 rates
policies$HCFactor <- as.integer(right(as.character(policies$HazardClass), 1)) * 0.04 - 0.04 + 1.00
policies$PremiumAdj.HC1 <- policies$PremiumAdj.ILF / policies$HCFactor



# What should the revenue factor be?
model.revenue <- glm(
  log(PremiumAdj.HC1) ~ log(Revenue),
  data = policies[policies$Retention == 10000, ]
)
summary(model.revenue)






