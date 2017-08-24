# Author: Adam L. Rich
# Date:   February 1, 2013
# Description:
#
#   Resources for the AdmittedBBR work
#



# Using the DEV Pseudo Package
# It references a corrected slnorm function

source('w:/sl/actuarial/r-software/utilities/DEV/SLPseudoPackage.R')
require(RODBC)



GuidelineDeductible <- function(x) {
  
  params <- GuidelineDeductible.params
  
  # Get the row this revenue falls in
  i <- findInterval(x, params$start)
  i <- ifelse(i <= 0 | i > nrow(params), NA, i)
  
  
  unbounded <- params$a[i] * x + params$b[i]
  bounded <- pmin(pmax(params$min[i], unbounded), params$max[i])
  
  return(bounded)
  
}



Interpolate <- function(x, map, FUN = I, at.edges = 'level') {
  stopifnot(
    at.edges %in% c('level', 'NA', NA),
    c('x', 'y') %in% names(map)
  )
  
  if (is.na(at.edges)) at.edges <- 'NA'
  
  i.lower <- findInterval(x, map$x)
  i.upper <- i.lower + 1
  
  if (at.edges == 'level') {
    i.min <- 1
    i.max <- length(map$x)
  }
  
  if (at.edges == 'NA') {
    i.min <- NA
    i.max <- NA
  }
  
  i.lower <- ifelse(i.lower <= 0, i.min, i.lower)
  i.upper <- ifelse(i.upper > length(map$x), i.max, i.upper)
  
  y <- ifelse(
    i.lower == i.upper, 
    map$y[i.lower], 
    (FUN(x) - FUN(map$x[i.lower])) / 
      (FUN(map$x[i.upper]) - FUN(map$x[i.lower])) * 
      (map$y[i.upper] - map$y[i.lower]) +
      map$y[i.lower]
  )
  
  return(y)
  
}



copy.bigtable <- function(obj) {
  f <- file(description = 'clipboard-5000', open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)
}



ModelPremium <- function(
  data, p, l.fs = 0.05, l.ed = -0.05, Agg.base = 1e6, Not.base = 25e3) {
  
  
  a.r    <- p[['a.r']]
  b.r    <- p[['b.r']]
  a.hc   <- p[['a.hc']]
  b.hc   <- p[['b.hc']]
  e.agg  <- p[['e.agg']]
  f.agg  <- p[['f.agg']]
  e.not  <- p[['e.not']]
  f.not  <- p[['f.not']]
  
  
  datum <- data[, c(
    'Revenue', 
    'HazardGroup',
    'AggLimit',
    'Notifications',
    'LiabilityRetention',
    'OldPremium'
  )]
  
  datum$id <- 1:nrow(datum)
  
  params <- data.frame(
    HazardGroup   = c('Retail', 'Financial Services', 'Higher Education', 'Healthcare'),
    a             = c(a.r, a.r, a.hc, a.hc),
    b             = c(b.r, b.r, b.hc, b.hc), 
    l             = c(1, 1 + l.fs, 1 + l.ed, 1)
  )
  
  datum <- merge(
    x = datum, 
    y = params, 
    all.x = TRUE, 
    sort = FALSE
  )
  
  
  datum <- within(datum, {
    
    BaseRate <- l * (a * Revenue ^ b)
    
    ILF.agg        <- GetILFs(AggLimit, e.agg, f.agg, base = Agg.base, as.map = FALSE)
    ILF.not        <- GetILFs(Notifications, e.not, f.not, base = Not.base, as.map = FALSE)
    
    Ret.guide <- GuidelineDeductible(Revenue)
    Ret.ratio <- LiabilityRetention / Ret.guide
    
    Ret.factor <- Interpolate(
      Ret.ratio,
      map.ret
    )
    
    ModelPremium <- BaseRate * ILF.agg * ILF.not * Ret.factor
    
  })
  
  return(datum)
  
}



GetILFs.2P <- function(limits, p1, p2, base = 1e6, as.map = TRUE) {
  
  # Assumes lognormal loss curve
  # ILF function approximated by
  #
  #   ILF = exp(p0 + p1 * ln(Limit) + p2 * [ln(Limit)] ^ 2) /
  #         exp(p0 + p1 * ln(Base)  + p2 * [ln(Base)] ^ 2)
  #
  # Which reduces to
  #   
  #   ILF = (limit / Base) ^ (p1 + p2 * ln(LB))
  #
  
  
  ILFs <- (limits / base) ^ (p1 + p2 * log(limits * base))
  
  if (as.map) {
    return(data.frame(
      Limit = limits,
      ILF = ILFs
    ))
  } else {
    return(ILFs)
  }
  
}



GetILFParameters.2P <- function(limits, ilfs, base = 1e6) {
  
  # Give two points, (L1, I1) and (L2, I2)
  #   return P1 and P2 that define the ILF curve that passes through them
  #
  # 
  
  I1 <- ilfs[1]
  I2 <- ilfs[2]
  L1 <- limits[1]
  L2 <- limits[2]
  B  <- base[1]
  
  
  P2 <- (log(I1) / log(L1 / B) - log(I2) / log(L2 / B)) / log(L1 / L2)
  
  P1 <-  log(I1) / log(L1 / B) - P2 * log(L1 * B)
  
  return(list(P1 = P1, P2 = P2))
  
}


