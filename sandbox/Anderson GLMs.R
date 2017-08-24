# Author: Adam L. Rich
# Date:   December 30, 2013
# Description:
#
#   GLMs in R
#

source('w:/SL/Actuarial/R-Software/Utilities/Dev/SLPseudoPackage.R')


# Notes from Anderson, et al.
df.0119 <- data.frame(
  Gender = rep(c('Male', 'Female'), each = 2),
  Territory = rep(c('Urban', 'Rural'), times = 2),
  AvgClaimSev = c(800, 500, 400, 200)
)



# Basic liner model
lm(AvgClaimSev ~ Gender + Territory, data = df.0119)


# Define indicator variables, like Anderson
df.0119$Male <- c(1, 1, 0, 0)
df.0119$Female <- c(0, 0, 1, 1)
df.0119$Urban <- c(1, 0, 1, 0)


# New lm on indicators
lm(AvgClaimSev ~ Male + Female + Urban, data = df.0119)


# Set intercept to zero to get coefficient for Female
lm(AvgClaimSev ~ Male + Female + Urban - 1, data = df.0119)



# Do the same with a GLM
model <- glm(
  AvgClaimSev ~ Gender + Territory, 
  data = df.0119,
  family = gaussian
)
model
cbind(df.0119, model$linear.predictors)


lm(log(AvgClaimSev) ~ Male + Female + Urban - 1, data = df.0119)
glm(
  AvgClaimSev ~ Male + Female + Urban - 1, 
  data = df.0119, 
  family = gaussian('log')
)
glm(
  AvgClaimSev ~ Male + Female + Urban - 1, 
  data = df.0119, 
  family = poisson('log')
)





model <- glm(
  AvgClaimSev ~ Gender + Territory, 
  data = df.0119,
  family = poisson(link = 'log')
)
model
cbind(df.0119, exp(model$linear.predictors))



predict(model, type = 'link')
predict(model, type = 'response')
predict(model, type = 'terms')



model <- glm(
  AvgClaimSev ~ Gender + Territory, 
  data = df.0119,
  family = gaussian
)
model
cbind(df.0119, predict(model, type = 'response'))
anova(model)




