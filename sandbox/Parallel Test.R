# Author: Adam L. Rich
# Date:   January 6, 2016
# Description:
#
#   Reverse engineering the MACK SE
#   Must run a "New Reserving v6.R" first
#




CL <- chainladder(triangle.b, weights = 1, delta = 1)



#' Parallel model calculations for first in CL$Models
#' 
#' lm(formula = y ~ x + 0, 
#'   data = data.frame(x = Triangle[, i], y = Triangle[, i + 1]), 
#'   weights = weights[, i]/Triangle[, i]^delta[i]
#' )
#' 
i <- 1
Triangle <- triangle.b
data <- data.frame(x = Triangle[, i], y = Triangle[, i + 1])
weights <- CL$weights
delta <- CL$delta

weights[, i]
Triangle[, i]

weights <- weights[, i]/Triangle[, i]^delta[i]

lm(formula = y ~ x + 0, data = data, weights = weights)
CL$Models[[1]]




##################################
# Prep and do lm fit
##################################


# args to lm
formula <- y ~ x + 0
method = "qr"
model = TRUE
x = FALSE
y = FALSE
qr = TRUE
singular.ok = TRUE
contrasts = NULL


# match.call is meaningless outside of a function call
# So, fake it to see what is going on
#   cl <- match.call()
#   mf <- match.call(expand.dots = FALSE)
#
# get_mf <- function (formula, data, subset, weights, na.action, method = "qr", 
#                     model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
#                     contrasts = NULL, offset, ...) {
#   cl <- match.call()
#   mf <- match.call(expand.dots = FALSE)
# }
# mf <- get_mf(formula = y ~ x + 0, data = data, weights = weights)
# cl <- mf
# 
# m <- match(c("formula", "data", "subset", "weights", "na.action", 
#              "offset"), names(mf), 0L)
# mf <- mf[c(1L, m)]
# 
# mf$drop.unused.levels <- TRUE
# mf[[1L]] <- quote(stats::model.frame)
# mf <- eval(mf, parent.frame())
#



# The first few lines of lm take the function call and prepare a call to 
#   stats::model.frame
#
mf <- model.frame(
  formula = y ~ x + 0, 
  data = data, 
  weights = weights, 
  drop.unused.levels = TRUE
)

ret.x <- x
ret.y <- y

mt <- attr(mf, "terms")


# Casting?
y <- model.response(mf, "numeric")
w <- as.vector(model.weights(mf))
offset <- as.vector(model.offset(mf))
x <- model.matrix(mt, mf, contrasts)


# z is what lm returns
#   z <- lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok)






##############################
# Calculate Z using lm.wfit
##############################
offset <- NULL
tol <- 1e-07
n <- nrow(x)
ny <- NCOL(y)
if (!is.null(offset)) y <- y - offset
dots <- list()
x.asgn <- attr(x, "assign")
zero.weights <- any(w == 0)
p <- ncol(x)
wts <- sqrt(w)



z <- .Call(stats:::C_Cdqrls, x * wts, y * wts, tol, FALSE)


  summary(CL$Models[[1]])







############################################333
# Pick up on ChainLadder
##############################################3


MackModel <- CL[["Models"]]
FullTriangle <- predict(CL)
est.sigma <- 'Mack'
weights <- CL[["weights"]]
alpha <- 1






n <- ncol(FullTriangle)
m <- nrow(FullTriangle)
f <- rep(1, n - 1)
f.se <- rep(0, n - 1)
sigma <- rep(0, n - 1)

# f is link ratios
f <- sapply(MackModel, function(x) summary(x)$coef["x", "Estimate"])
f.se <- sapply(MackModel, function(x) summary(x)$coef["x", "Std. Error"])

                                                    
sigma <- sapply(MackModel, function(x) summary(x)$sigma)
isna <- is.na(sigma)


# n <- length(sigma)
# dev <- 1:n
# my.dev <- dev[!is.na(sigma) & sigma > 0]
# my.model <- lm(log(sigma[my.dev]) ~ my.dev)
# sigma[is.na(sigma)] <- exp(predict(my.model, newdata = data.frame(my.dev = dev[is.na(sigma)])))
# list(sigma = sigma, model = my.model)




for (i in which(isna)) {
  sigma[i] <- sqrt(
    abs(
      min(
        (sigma[i - 1]^4/sigma[i - 2]^2), 
        min(
          sigma[i - 2]^2, 
          sigma[i - 1]^2
          )
        )
      )
    )
  f.se[i] <- sigma[i]/sqrt(weights[1, i] * FullTriangle[1, i]^alpha[i])
}



W <- weights
W[is.na(W)] <- 1
F.se <- t(
  `/`(
    sigma,
    t(
      sqrt(
        W[, -n] * 
          t(t(FullTriangle[, -n])^alpha[-n])   
        )
      )
    )
  )
return(list(sigma = sigma, f = f, f.se = f.se, F.se = F.se))




StdErr <- ChainLadder:::Mack.S.E(CL[["Models"]], FullTriangle, est.sigma = est.sigma, 
                   weights = CL[["weights"]], alpha = alpha)




output$Mack.S.E[, 11]
# output[["Mack.S.E"]] <- sqrt(StdErr$FullTriangle.procrisk^2 + 
#                                StdErr$FullTriangle.paramrisk^2)




ChainLadder:::MackRecursive.S.E(FullTriangle, StdErr$f, StdErr$f.se, StdErr$F.se, mse.method = 'Mack')





n <- ncol(FullTriangle)
m <- nrow(FullTriangle)
FullTriangle.procrisk <- FullTriangle[, 1:n] * 0
FullTriangle.paramrisk <- FullTriangle[, 1:n] * 0
colindex <- 1:(n - 1)
for (k in colindex) {
  for (i in (m - k + 1):m) {
    
    FullTriangle.procrisk[i, k + 1] <- 
      sqrt(FullTriangle[i, k]^2 * (F.se[i, k]^2) 
           + FullTriangle.procrisk[i, k]^2 * f[k]^2)
    
    
    
    
    FullTriangle.paramrisk[i, k + 1] <- 
      sqrt(FullTriangle[i, k]^2 * (f.se[k]^2) 
           + FullTriangle.paramrisk[i, k]^2 * f[k]^2)
  }
}
return(list(FullTriangle.procrisk = FullTriangle.procrisk, 
            FullTriangle.paramrisk = FullTriangle.paramrisk))






