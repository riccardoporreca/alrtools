# Author: Adam L. Rich
# Date:   September 20, 2011
# Description:
#
#   A premium module for R
#
#     uepr()
#     written()
#     earned()
#
#     Each takes (nearly) the same arguments, which are all vectors
#       premium   the premium to be earned, etc.
#       p.eff     Policy Effective Date
#       p.exp     Policy Expiration Date
#       t.eff     Transaction/Endorsement Effective Date
#       t.exp     Transaction/Endorsement Expiration Date
#       v.eff     Valuation Period Start Date
#       v.exp     Valuation Period End Date
#       method    The earnings method to be used
#                 Currently, only 'straight' is supported
#

# For manual testing:
#   premium <- 1
#   p.eff <- as.Date('2011-1-1')
#   p.exp <- as.Date('2012-1-1')
#   t.eff <- p.eff
#   t.exp <- p.exp
#   v.eff <- 0
#   v.exp <- as.Date('2011-7-1')
#   method <- 'stright'

uepr <- function(
    premium,
    p.eff,
    p.exp,
    t.eff = p.eff,
    t.exp = p.exp,
    v.exp,
    method = 'straight') {
  
  # How long will our result vector be?
  l <- max(length(premium), 
           length(p.eff), 
           length(p.exp), 
           length(t.eff), 
           length(t.exp), 
           length(v.exp), 
           length(method))
  
  # Force recycle so as to not get errors below
  #   Also, convert dates to integers by as.integerating
  premium  <- rep(premium, length.out = l)
  p.eff    <- rep(as.integer(p.eff), length.out = l)
  p.exp    <- rep(as.integer(p.exp), length.out = l)
  t.eff    <- rep(as.integer(t.eff), length.out = l)
  t.exp    <- rep(as.integer(t.exp), length.out = l)
  v.exp    <- rep(as.integer(v.exp), length.out = l)
  method   <- rep(method, length.out = l)
  
  # All dates will be coerced to integers when passed
  # This automatically as.integerates all times back to the beginning of the day
  # Valuation dates are intepreted to be at 12:00am
  # All other dates are intepreted to be at 12:01am
  # So, a policy written on 1/1/2011 will have no UEPR when valued at 1/1/2011
  #
  # When calculating UEPR is it common to want the UEPR at the end of the quarter, year, month, etc, say 12/31/2010
  # To get this date, though, one must pass 1/1/2011
  #
  # For purposes of counting day differences, however, the one minute is ignored
  
  # Fix policy dates
  #   Policy expiration always has to be after policy effective
  #   If not, we trust that policy effective is correct
  p.exp <- ifelse(p.eff < p.exp, p.exp, p.eff)
  
  # Fix transaction dates
  #   The transaction period must always fall within the policy period
  #   If not, we trust the constrain to the policy period
  #   TODO: Would this handle extended reporting endorsements or other extra-policy period items?
  t.exp <- ifelse(t.exp < p.exp, t.exp, p.exp)
  t.eff <- ifelse(t.eff > p.eff, t.eff, p.eff)
  t.exp <- ifelse(t.eff < t.exp, t.exp, t.eff)
  
  # Has the policy even been written yet?
  #   A policy will have no UEPR until it is actually written
  written.b <- t.eff < v.exp
  
  # Has the transaction period already completed?
  expired.b <- t.exp <= v.exp
  
  # Get the length of the transaction period
  t.length <- t.exp - t.eff
  
  # At this point, because of the fixes made above,
  #   the transaction period should not be of negative length
  #   But just in case it is, we check for non-positive lengths
  #
  # If a transaction has zero length, it is completely earned at the instance it is written
  trivial.b <- t.length <= 0
  
  # The only currently supported method is straight line earnings
  ifelse(
    written.b & !expired.b & !trivial.b, 
    ifelse(method == rep('straight', l),
      premium * (t.exp - v.exp) / t.length, 
      NA
    ),
    0
  )

}

written <- function(
    premium,
    p.eff,
    p.exp,
    t.eff = p.eff,
    t.exp = p.exp,
    v.eff = 0,
    v.exp,
    method = 'straight') {

  # If the Transaction Effective date falls inside the valuation period, then it has been written
  #
  # If the policy is written on 5/15/2011
  #   and the val period is second quarter 2011, it is written
  #
  # An edge case:
  #   If the policy is effective 4/1/2011 and the val dates are 4/1/2011 and 5/1/2011 (the month of April, see uepr above)
  #
  #   TransEff = 4/1/2011 @ 12:01am
  #   ValEff   = 4/1/2011 @ 12:00am
  #
  #   It is written
  
  # How long will our result vector be?
  l <- max(length(premium), 
           length(t.eff), 
           length(v.eff), 
           length(v.exp))
  
  
  premium  <- rep(premium, length.out = l)
  t.eff    <- rep(as.integer(t.eff), length.out = l)
  v.eff    <- rep(as.integer(v.eff), length.out = l)
  v.exp    <- rep(as.integer(v.exp), length.out = l)
  
  # Since these are integers, we have to include equality to get the edge cases right
  ifelse(t.eff >= v.eff & t.eff < v.exp, premium, 0)
  
}

earned <- function(
    premium,
    p.eff,
    p.exp,
    t.eff = p.eff,
    t.exp = p.exp,
    v.eff = 0,
    v.exp,
    method = 'straight') {

  # written + opening UEPR less closing UEPR
  
  v.exp <- ifelse(v.eff < v.exp, v.exp, v.eff)

  written(premium, p.eff, p.exp, t.eff, t.exp, v.eff, v.exp, method) +
     uepr(premium, p.eff, p.exp, t.eff, t.exp, v.eff,        method) -
     uepr(premium, p.eff, p.exp, t.eff, t.exp,        v.exp, method)
    
}

earned(1, p.eff = as.Date('2010-1-1'), 
          p.exp = as.Date('2011-1-1'), 
          v.exp = as.Date('2010-7-1'))

earned(1, p.eff = as.Date('2010-1-1'), 
          p.exp = as.Date('2010-1-1'), 
          v.exp = as.Date('2010-1-1'))

earned(1, p.eff = as.Date('2010-1-1'), 
          p.exp = as.Date('2010-1-1'), 
          v.exp = as.Date('2011-1-1'))

earned(1, p.eff = as.Date('2010-1-1'), 
          p.exp = as.Date('2011-1-1'),
          v.eff = as.Date('2010-1-1'),
          v.exp = as.Date('2010-4-1'))
