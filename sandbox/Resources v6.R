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



Guideline <- function(Revenue, params) {
  
  # Get the row this revenue falls in
  i <- findInterval(Revenue, params$start)
  i <- ifelse(i <= 0 | i > nrow(params), NA, i)
  
  
  unbounded <- params$a[i] * Revenue + params$b[i]
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



copy.table <- function(obj, row.names = FALSE, buffer.size = 128) {
  f <- file(description = paste('clipboard-', buffer.size, sep = ''), open = 'w')
  write.table(obj, f, row.names = row.names, sep = '\t')
  close(f)
}

copy.bigtable <- function(obj, row.names = FALSE) {
  copy.table(obj, row.names, 5000)
}


# 
# 
# GetILFTable <- function(
#     limits, revenues, a, b, 
#     base = 1e6, alpha = 0.25, 
#     odf = 2.5, lambda = 1000) {
#     
#   #
#   # Assumes losses come from a lognormal distribution 
#   #   with sigma = 2, and 
#   #
#   #   mu = aleph * log(Revenue) + beta
#   #
#   # a and b are translated from [-Inf, Inf] to [0, 1]
#   #
#   # An ILF curve is constructed for each revenue point
#   #
#   
#   revenues <- sort(revenues)
#   limits <- sort(limits)
#   ILF.table <- data.frame(limit = as.integer(limits))
#   
#   
#   aleph  <- pnorm(a)
#   beta   <- pnorm(b)
#   
#   
#   for (r in revenues) {
#     
#     m <- data.frame(
#       lo     = 0,
#       hi     = 1,
#       mu     = aleph * log(r) + beta,
#       sigma  = 2,
#       start  = 0,
#       end    = 0 # Signifies no upper bound
#     )
#     
#     curve <- ILFlnorm3(
#       SeverityCurves  = m,
#       xvalues         = limits,
#       xbase           = base,
#       alpha           = alpha,
#       odf             = odf,
#       lambda          = lambda,
#       SEEPLOT         = FALSE
#     )$ILF
#       
#     ILF.table <- cbind(ILF.table, curve)
#   }
#   
#   names(ILF.table) <- c('limit', as.integer(revenues))
#   
#   ILFs <- list(
#     Table = ILF.table,
#     Limits = limits,
#     Revenues = revenues,
#     a = a, 
#     b = b,
#     aleph = aleph,
#     beta = beta,
#     alpha = alpha,
#     odf = odf,
#     lambda = lambda,
#     Matrix = as.matrix(ILF.table[, -1])
#   )
#   
#   return(ILFs)  
#     
# }
#   
# 


GetILFs <- function(limits, revenues, ILFs) {
  
  # Interpolate across limit and revenue to get ILF
  #
  # We are given Rx and Lx,
  #   we have I11, I12, I21, and I22 from the ILFs matrix
  # Solve for Ixx
  #
  #   +---+--------+-----+-----+-----+
  #   |   | limits | R1  | Rx  | R2  |
  #   +===+========+=====+=====+=====+
  #   | 1 | L1     | I11 | I1x | I12 |
  #   +---+--------+-----+-----+-----+
  #   | 2 | Lx     | Ix1 | Ixx | Ix2 |
  #   +---+--------+-----+-----+-----+
  #   | 3 | L2     | I21 | I2x | I22 |
  #   +---+--------+-----+-----+-----+
  #   
  
  # Get the number of rows in the matrix
  nr <- nrow(ILFs$Matrix)
  
  R1.index <- findInterval(revenues, ILFs$Revenues)
  R2.index <- pmin(R1.index + 1, length(ILFs$Revenues))
  R1.index <- ifelse(R1.index < 1, 1, R1.index)
  
  L1.index <- findInterval(limits, ILFs$Limits)
  L2.index <- pmin(L1.index + 1, length(ILFs$Limits))
  L1.index <- ifelse(L1.index < 1, 1, L1.index)
  
  R1 <- ILFs$Revenues[R1.index] 
  R2 <- ILFs$Revenues[R2.index] 
  L1 <- ILFs$Limits[L1.index] 
  L2 <- ILFs$Limits[L2.index] 
  
  I11 <- ILFs$Matrix[L1.index + (R1.index - 1) * nr]
  I12 <- ILFs$Matrix[L1.index + (R2.index - 1) * nr]
  I21 <- ILFs$Matrix[L2.index + (R1.index - 1) * nr]
  I22 <- ILFs$Matrix[L2.index + (R2.index - 1) * nr]
  
  Rw <- ifelse(R1 == R2, 0, (revenues - R1) / (R2 - R1))
  Lw <- ifelse(L1 == L2, 0, (limits - L1) / (L2 - L1))
  
  I1x <- Rw * (I12 - I11) + I11  
  I2x <- Rw * (I22 - I21) + I21
  
  Ixx <- Lw * (I2x - I1x) + I1x
  
  return(Ixx)
  
}




# 
# GetILFs <- function(limits, p1, p2, base = 1e6, as.map = TRUE, max.limit = max(limits)) {
#   
#   # Assumes lognormal loss curve
#   # ILF function approximated by
#   #
#   #   ILF = exp(p0 + p1 * ln(Limit) + T(p2) * [ln(Limit)] ^ 2) /
#   #         exp(p0 + p1 * ln(Base)  + T(p2) * [ln(Base)] ^ 2)
#   #
#   # Which reduces to
#   #   
#   #   ILF = (limit / Base) ^ (p1 + T(p2) * ln(LB))
#   #
#   # p2 can be any number and ILF curve is guaranteed to 
#   #   have a montonically increasing first derivative, because
#   #
#   # We apply the transform T
#   #
#   #   T(x) = x ^ 2 - p1 / 2 / log(max.limit)
#   #
#   # Since P2^2 is always greater than or equal to zero, 
#   #   T(p2) >= - p1 / 2 / log(max.limit)
#   #
#   # If p2 gets too big, however, the ILF curve will have a positive
#   #   2nd derivative and will quickly diverge
#   #
#   # At least it is bounded on one side
#   #
#   
#   Tp2 <- p2 ^ 2 - p1 / 2 / log(max.limit)
#   ILFs <- (limits / base) ^ (p1 + Tp2 * log(limits * base))
#   
#   if (as.map) {
#     return(data.frame(
#       Limit = limits,
#       ILF = ILFs
#     ))
#   } else {
#     return(ILFs)
#   }
#   
# }
# 
# 
# 
# GetILFParameters <- function(limits, ilfs, base = 1e6, max.limit = max(limits)) {
#   
#   # Give two points, (L1, I1) and (L2, I2)
#   #   return P1 and P2 that define the ILF curve that passes through them
#   #
#   # 
#   
#   I1 <- ilfs[1]
#   I2 <- ilfs[2]
#   L1 <- limits[1]
#   L2 <- limits[2]
#   B  <- base[1]
#   
#   
#   TP2 <- (log(I1) / log(L1 / B) - log(I2) / log(L2 / B)) / log(L1 / L2)
#   
#   P1 <-  log(I1) / log(L1 / B) - TP2 * log(L1 * B)
#   
#   P2 <- sqrt(TP2 + P1 / 2 / log(max.limit))
#   
#   return(list(P1 = P1, P2 = P2, max.limit = max.limit))
#   
# }
# 
# 


# Generic fitting function
OptimizeModel <- function(p, p.fixed, data, FUN, err = 'absolute', cells = c(), ...) {
  
  # Error method can be one of
  #
  #   absolute      abs value of model - actual
  #   squared       squared value of model - actual
  #   percent-sq    squared value of percent difference 
  #   percent-abs   abs value of percent difference 
  #
  # p has the parameters we want to optimize over,
  #   it may have extra
  # If p has anything that is in p.fixed,
  #   do not pass the extras to optim
  #
  
  p.var <- p[setdiff(names(p), names(p.fixed))]
  
  
  # The function that optim will call to score each iteration
  ModelError <- function(p.var, p.fixed, data, FUN, err, cells, ...) {
    
    p.all <- c(p.var, p.fixed)
    
    Model <- FUN(p = p.all, data = data, ...)
    
    if(length(cells) > 0) {
      
      Model.cells <- aggregate(
        x = Model[, c('Fitted', 'Target')], 
        by = Model[, cells, drop = FALSE], 
        FUN = mean
      )
      
      Fitted <- Model.cells$Fitted
      Target <- Model.cells$Target
    
    } else {
    
      Fitted <- Model$Fitted
      Target <- Model$Target  
    
    }
    
    if (err == 'absolute')      return(sum(abs(Fitted - Target), na.rm = TRUE))
    if (err == 'squared')       return(sum((Fitted - Target) ^ 2, na.rm = TRUE))
    if (err == 'percent-abs')   return(sum(abs(Fitted / Target - 1), na.rm = TRUE))
    if (err == 'percent-sq')    return(sum((Fitted / Target - 1) ^ 2, na.rm = TRUE))
    
  }
  
  
  Optimized <- optim(
    par = p.var, 
    fn = ModelError, 
    data = data, 
    p.fixed = p.fixed, 
    FUN = FUN,
    err = err, 
    cells = cells,
    control = list(maxit = 20000), ...)
  
#   Optimized <- optim(
#     par = p.var, 
#     fn = ModelError, 
#     data = data, 
#     p.fixed = p.fixed, 
#     FUN = FUN,
#     err = err, Agg.base = 3e6)
  
  
  p.new <- c(p.fixed, Optimized$par)
  
  Optimized$Model    <- FUN(p = p.new, data = data, ...)
  Optimized$Data     <- data
  Optimized$FUN      <- FUN
  Optimized$err      <- err
  Optimized$p.fixed  <- p.fixed
  
  return(Optimized)
  
}


