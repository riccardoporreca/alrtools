#Layered Fit
#To fit to multiple layers at once

#Example
#FitLayers(X)

#currently only for lognormal

library(stats4)

FitLayers <- function(X)	{

  x <<- X$claim
  L <<- X$limit
  E <<- X$excess

  #assume data has
  ##incurred
  #limit
  #excess

  #  (later) CIA (Costs in Additon)

  est <- mle(minuslog=ll, start=list(m=10,s=1.5))
  #to see trace of iterations
  #est<-mle(minuslog=ll, start=list(m=11,s=2),control = list(trace=TRUE,REPORT=1))

  summary(est)
}

ll <- function(m,s) { 
  N <- (x<(L+E) & x>E) * 
        ifelse(x == 0, 0, ( log(s)+.5*((log(x)-m)/s)^2 ))   +(x>=(L+E))*( -plnorm(L+E,meanlog=m,sdlog=s,lower=FALSE,log=TRUE))
  
  D <- (x >= E) * (-plnorm(E, meanlog = m, sdlog=s, lower = FALSE, log = TRUE))
  
  sum(N-D)
}


#test for layering
#use X from ExampleFitlnorm.xlsm
#X2<-cbind(X,10e6,1e6)
#names(X2)<-c("claim","limit","excess")
#FitLayers(X2)
