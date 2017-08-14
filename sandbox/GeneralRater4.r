# Author: Simon Brickman
# Date:   Various
# Description:
#
#   version 3 includes functions slnorm2,ATLlnorm2,ILFlnorm2 and Ilnorm2 to handle calculations
#   of excedence probability, Average claim to layer, ILFs and Moments for lognormal
#   based on composite lognormal severity curves object loss_matrix
#
#   Several of the functions below expect a matrix of losses 
#     where each column lists the losses in a given year
#
#
#   AExp        "Aggregate Experience"
#               Returns Mean, Standard Deviation, Nil-loss Probability, 
#               95th and 99th Percentiles of the potion of x that sits 
#               between Excess and Excess + Limit
#               
#   colcSum     "Column Cumulative Sums"
#               sum is to colSum as 
#               cumsum is to colcSum
#
#   fc          "First Case"
#               Returns the index of the first element in x that exceeds year
#
#   nSirExhaustion
#               Applies fc to columns of a matrix               
#
#   colpSum     "Colum Partial Sum"
#               Calculates the partial sums on columns of a matrix
#
#   AggClmToInsurer
#               ???
#   
#   PerClaimToLayer
#               ???
#   
#   Rlnorm2     Random value from piecewise lnorm
#   
#   SimX        ???
#   
#   LayerCalcs  ???
#   
#   slnorm2     Survival function, piecewise lnorm
#   
#   ILFlnorm2   ???
#   
#   Ilnorm2     ???
#   
#   ATLnorm2    ???
#
#   ILFlnorm2   ???
# 
#   X2lnorm2    ???
# 
#   COVlnorm2   ???
# 
# 
# 
# 
# 
# 

library(actuar)
library(MASS)

AExp <- function (x, Limit, Excess = 0) {
  
  # Aggregate calc, returning mean and sd, etc.
  #
  # Parameters
  #   x         A vector
  #   Limit     Maximum to apply to x - Excess
  #             For example, to get stats for x in $2m xs $5m,
  #               Limit is $2m, Excess of $5m
  #   Excess    "Excess of"
  #             
  # Example
  #   x <- runif(100)
  #   AExp(x, limit = .5, excess = .25)
  #
  
  cl <- pmin(pmax(x - Excess, 0), Limit)
  
  AE <- array(0, c(1, 5))
  AE[1,1] <- mean(cl)
  AE[1,2] <- sd(cl)
  AE[1,3] <- quantile(cl, 0.95, 1)
  AE[1,4] <- quantile(cl, 0.99, 1)
  AE[1,5] <- ecdf(cl)(0)
  
  colnames(AE) <- c(
    "mean", 
    "sd", 
    "95percentile",
    "99percentile",
    "NilLossProbability"
  )
  
  return(AE)
  
}


colcSum <- function(x) {
  # Function to produce column cumulative sums
  apply(x, 2, cumsum)
}


fc <- function(x,y) {
  # Function to identify first case of vector x exceeding y
  which(x > y)[1]
}


nSirExhaustion <- function(x, m) {
  # Function to identify first case each year of maintenance being used
  apply(x, 2, fc, (m - .01))
}


colpSum <- function(x, y, a, b, u, v, w) {
  # Function to produce column sums for partial range
  #
  #   x     Matrix of losses
  #         Each column represents a year
  #
  #   y     MaintenanceThereafter
  #
  #   a     "limit x to y from a to b, inclusive"
  #         First case where SIR is exhausted
  #
  #   b     "do not add beyond this column"
  #         Number of claims in the year
  #
  #   u     MaintenanceBefore
  #
  #   v     PerClaimSIR
  #
  #   w     AggSIR
  #
  
  colpSum <- rep(0, dim(x)[2])    # Post exhuastion retained losses
  colbSum <- colpSum              # Pre exhuastion retained losses
  presum1 <- colpSum
  presum2 <- colpSum
  
  # Do each year separately
  for (i in 1:dim(x)[2]) {
  
    # a is NA when SIR is never exhausted?
    if ( !is.na(a[i]) ) {
    
      if (a[i] > 1) {
      
        # The amount retained as part of the PerClaimSIR, not the MaintenanceBefore
        presum1[i] <- sum(pmin(pmax(x[1:(a[i] - 1), i] - u, 0), v))
        
        # Total of MaintenanceBefore and PerClaimSIR, limited to claim amount
        presum2[i] <- sum(pmin(x[1:a[i]-1,i],u+v))
        
      }
      
      # u is in here because
      #   we only did the above on a-1, 
      #   so we are missing one maintenance before,
      #   the one from the year that we pay the partial PerOccAgg,
      #   and reach the AggSIR
      colbSum[i] <- u + w + presum2[i] - presum1[i]
      
      # Add up the MaintenanceThereafter for all claims after AggSIR is exhausted
      if (a[i] < b[i]) {
        colpSum[i] <- sum(pmin(x[(a[i] + 1):b[i], i], y))
      }

    }
    
    # If AggSIR is never exhausted
    else  { 
      if (b[i] > 0) {
        colbSum[i] <- sum(pmin(x[1:b[i], i], u + v)) 
      }  
    }
    
  }
  return(list(colpSum, colbSum))
}


AggClmToInsurer <- function (xSim, PerClaimSIR, AggSIR = Inf, MaintenanceThereafter = 0, MaintenanceBefore = 0) {
  
  # Note this can work on matrix of claims, with columns representing different years of claims output
  #
  #   xSim                      A list of two objects
  #
  #   xSim[[1]]                 A matrix of claims
  #                             Each column represents a years
  #                             Claims in a column are listed in the order they occurred
  #
  #   xSim[[2]]                 A vector having length equal to the number of columns in xSim[[1]]
  #                             Element i gives the number of claims in the ith column of xSim[[1]]
  #                             Everything after i is to be ignored
  #
  #   PerClaimSIR               The amount the insured pays on each claim 
  #                             until the AggSIR has been met
  #                             MaintenanceBefore does not count toward the AggSIR
  #
  #   AggSIR                    Insured pays PerClaimSIR until AggSIR has been met                             
  #                             MaintenanceBefore does not count toward the AggSIR
  #
  #   MaintenanceThereafter     The amount paid on a per occurrence basis by the insured after aggregate retention is exhausted
  #
  #   MaintenanceBefore         The amount, over and above PerClaimSIR, that the insured pays on each claim
  #                             MaintenanceBefore does not count toward the AggSIR
  #
  
  x  <- xSim[[1]]
  ny <- xSim[[2]]
  
  # MaintenanceThereafter is irrelevant if AggSIR is Infinity
  if (AggSIR == Inf) {
    return(colSums(pmax(x - PerClaimSIR - MaintenanceBefore, 0), na.rm = T))
  }

  # What is the running total of the losses occurring in the layer
  #   between MaintenanceBefore and PerClaimsSIR?
  cpcl  <- colcSum(pmin(pmax(x - MaintenanceBefore, 0), PerClaimSIR))
  
  # At which loss has the AggSIR been exhausted?
  ncase <- nSirExhaustion(cpcl, AggSIR)
  
  
  # The following calculates the retention by the insured
  #
  #   x                       A matrix of simulated claims
  #                           One column per year
  #
  #   MaintenanceThereafter   The amount paid on a per occurrence basis by the insured 
  #                           after aggregate retention is exhausted
  #
  #   ncase                   The first case where the SIR is exhausted
  #                           Insured does not have an SIR on claims starting here
  #
  #   ny                      Number of claims in each year
  #                           Has the same number of elements as columns in x
  #
  #   MaintenanceBefore       The amount, over and above PerClaimSIR, that the insured pays on each claim
  #                           MaintenanceBefore does not count toward the AggSIR
  #
  #   PerClaimSIR             The amount the insured pays on each claim 
  #                           until the AggSIR has been met
  #                           MaintenanceBefore does not count toward the AggSIR
  #
  #   AggSIR                  Insured pays PerClaimSIR until AggSIR has been met                             
  #                           MaintenanceBefore does not count toward the AggSIR
  #
  #
  
  
  apcd  <- colpSum(
    x = x, 
    y = MaintenanceThereafter, 
    a = ncase, 
    b = ny, 
    u = MaintenanceBefore, 
    v = PerClaimSIR, 
    w = AggSIR
  )

  al <- colSums(x, na.rm = T)

  if (MaintenanceThereafter == 0) {
    return(al - apcd[[2]])
  } else {
    return(al - apcd[[1]] - apcd[[2]])
  }
}




PerClaimToLayer <- function (x, PerClaimLimit, PerClaimExcess = 0, AggClaimLimit = PerClaimLimit, AggClaimDeductible = 0) {
  
  # Note this can work on matrix of claims, with columns representing different years of claims output
  #
  # 
  # 
  #   x                     A matrix of claims
  #
  #   PerClaimLimit         
  #   PerClaimExcess        
  #   AggClaimLimit
  #   AggClaimDeductible
  #
  
  
  x <- colSums(pmin(pmax(x - PerClaimExcess, 0), PerClaimLimit), na.rm = T)
  PerClaimToLayer <- AExp(x, AggClaimLimit, AggClaimDeductible)
  return(PerClaimToLayer)

  #example
  #x<-array(c(1:12),c(3,4))
  #PerClaimToLayer(x,4,1,10,2)
  # mean = 7.5, sd=sqrt(228/12)
}


Rlnorm2 <- function(n,m) {

#mu  = mean of lognormal for individual claim amount distribution
#sigma  = sd of lognormal
#m = loss matrix specifying following composite lognormal distribution
#m<-data.frame(lo=c(0,.77545,.98204),hi=c(.77545,.98204,1),mu=c(9.9233,9.8717,11.69),
#              sigma=c(2.358,2.0828,1.6),start=c(0,1e5,1e6),end=c(1e5,1e6,0))
#       lo      hi      mu  sigma start   end
#1 0.00000 0.77545  9.9233 2.3580 0e+00 1e+05
#2 0.77545 0.98204  9.8717 2.0828 1e+05 1e+06
#4 0.98204 1.00000 11.6900 1.6000 1e+06 0e+00
#
  r<-runif(n)

  i<-findInterval(r, m$hi)
  i<-i+1

  mu<-m$mu[i]
  sigma<-m$sigma[i]
  start<-m$start[i]
  end<-m$end[i]

  lo<-m$lo[i]
  hi<-m$hi[i]

  f<-(r-lo)/(hi-lo)
  z1<-plnorm(start,mu,sigma,TRUE)
  z2<-plnorm(end,mu,sigma,TRUE)
  z2[end == 0]<-1        #treat 0 end value as infinite, iwth cum prob=1

  qlnorm(f*(z2-z1)+z1,mu,sigma)
}


SimX <- function(nclm, m, odf = 2.5, nyr = 1000) {
  
  # m           Loss matrix, a dataframe with six columns        
  #             Defines a piecewise lognormal distribution
  #               lo      
  #               hi
  #               mu
  #               sigma
  #               start
  #               end
  #             lo and hi define the probability for a given piecewise
  #             TODO             
  #
  # nclm    no of claims above the minimum value specified in the loss matrix m
  #         this will need to be at or below maintenance deductible for simulation to provide complete information
  #         for losses to layer structure
  # odf     overdispersion factor for NegBin
  # nyr     number of years simulation


#remove rows from m which have zero probability
m<-with(m,m[!lo==hi,])

# NOTE the expressions for ms AND mf ARE dependent on the parameters being set at global level
# so need << assignment and removal at end of these global values
  global_mc<<-nclm
  global_odf<<-odf
  global_m<<-m

  nodes <- list(year = nyr)

  #v=m=+m^2/h=m*odf: h=m/(odf-1)

  mf <- expression(year = rnegbin(mu=global_mc,theta=global_mc/(global_odf-1)))
  ms <- expression(year=Rlnorm2(m=global_m))
  pf <- simul(nodes, mf, ms)

  #now clean up
  rm(list=ls(1)[grepl("glob",ls(1))],envir=globalenv())

  sf<-severity(pf, by = "year")   #see severity claims by year
  sn<-as.numeric(frequency(pf,classification=FALSE))    #claim number by year

  return(list(sf$main,sn))
}

    LayerCalcs<-function(sf,c) {

    #c is cover in form per claim/aggregate SIR and agg ded/limit
    #this is expecting 6 column cover object
     if (!dim(c)[2]==6) { 
        return("FATAL ERROR: The cover object does not have correct number (6) of columns. Please check you are running latest version of General Rater")}
    
    
    
          #set up layers
          lst<- array(0,c(dim(c)[1],5))
          for (i in 1:dim(c)[1])  {
            pcl<-c[i,2]

            pca<-c[i,3]
            pca[pca==0]<-Inf
            pca[is.na(pca)]<-Inf

            pcl[is.na(pcl)]<-pca

            pcma<- c[i,4]
            pcma[is.na(pcma)]<-0
            if (pca==Inf) {pcma<-0}

            pcmb<- c[i,1]
            pcmb[is.na(pcmb)]<-0
                        
            agd<-c[i,5]
            agd[is.na(agd)]<-0

            res<-AggClmToInsurer(sf,pcl,pca,pcma,pcmb)    #claims to insurer
            lst[i,]<-AExp(res,c[i,6],agd)
          }
          return(lst)
    }


slnorm2 <- function(x, m)   {
  
  # [February 1, 2013 ALR]
  # Failing on x = 0
  # Should return 1, regardless of mu and sigma
  if(x <= 0) return(1)

  
  # Continue if x != 0
  i <- findInterval(x, m$start)
  
  mu     <- m$mu[i]
  sigma  <- m$sigma[i]
  start  <- m$start[i]
  end    <- m$end[i]
  lo     <- m$lo[i]
  hi     <- m$hi[i]
  
  # Changed 29/12/2012 to allow for final end point not being 0
  # Remove z<-plnorm(x,mu,sigma,TRUE)
  z<-ifelse(x>end & end>0,1,plnorm(x,mu,sigma,TRUE))
  
  z1<-plnorm(start,mu,sigma,TRUE)
  # z2<-ifelse(end==0,1,plnorm(end,mu,sigma,TRUE)) #treat 0 end value as infinite, with cum prob=1
  z2<-ifelse(x>end,1,plnorm(end,mu,sigma,TRUE)) #treat 0 end value as infinite, with cum prob=1
  
  return(1-(lo+(hi-lo)*(z-z1)/(z2-z1)))
  
}


# # Example added May 2, 2017
# x <- c(0, 1e+05, 1e+06, 4145831.75030801, Inf)
# 
# 
# m <- data.frame(
#   lo = c(0, 0.431284947318597, 0.930256290205632),
#   hi = c(0.431284947318597, 0.930256290205632, 1),
#   mu = c(12.06, 11.73, 11.32),
#   sigma = c(2.89, 1.41, 1.68),
#   start = c(0, 1e+05, 1e+06),
#   end = c(1e+05, 1e+06, Inf)
# )
# 
# 
# # slnorm2 <- function (x, m)
# 
#   
#   
#   x[x < m$start[1]] <- m$start[1]
#   i <- findInterval(x, m$start)
#   mu <- m$mu[i]
#   sigma <- m$sigma[i]
#   start <- m$start[i]
#   end <- m$end[i]
#   lo <- m$lo[i]
#   hi <- m$hi[i]
#   end <- ifelse(end == 0, Inf, end)
#   
#   
#   
#   z <- ifelse(x > end, 1, plnorm(x, mu, sigma, TRUE))
#   z1 <- plnorm(start, mu, sigma, TRUE)
#   z2 <- plnorm(end, mu, sigma, TRUE)
#   p <- pmax(0, 1 - (lo + (hi - lo) * (z - z1)/(z2 - z1)))
#   #  return(p)
#   p
# 
#   


#   check:
#    lo  	  hi	  mu	    sigma	start	    end
#    0.000	0.500	11.029	2.200	0	        100,000
#    0.500	0.800	12.029	2.000	100,000	  1,000,000
#    0.800	1.000	10.029	2.300	1,000,000	0
#    ATLlnorm(1e5,5e4,loss_matrix)



#ATLlnorm2<-function(Limit,Retention=0,m)  {
#  i1<-findInterval(Retention,m$start)
#  i2<-findInterval(Retention+Limit,m$start)
#  m1<-0
#
#    for (i in i1:i2)   {
#      mu<-m$mu[i]
#      sigma<-m$sigma[i]
#      start<-m$start[i]
#      end<-m$end[i]
#
#      lo<-m$lo[i]
#      hi<-m$hi[i]
#
#      f<-(hi-lo)/Ilnorm2(start,end,mu,sigma)
#
#      s<-max(Retention,start)
#      e<-ifelse(end==0,Retention+Limit,min(Retention+Limit,end))
#
#      m1<-m1+Ilnorm2(s,e,mu,sigma,f,1,Retention)
#    }
#
#    return (m1/slnorm2(Retention,m))     
#}
#

#Ilnorm2<- function(start,end,mu,sigma,multiplier=1,moment=0,excess=0)  {
#  z1<-plnorm(start,mu,sigma,TRUE)
#  z2<-ifelse(end==0,1,plnorm(end,mu,sigma,TRUE))      #treat 0 end value as infinite, with cum prob=1
#  t<-exp(mu+.5*sigma^2)
#  
#  if (moment==0) {
#    Ilnorm2<-multiplier*(z2-z1) 
#   } else {
#    t2<-ifelse(end==0,t,levlnorm(end,mu,sigma,1))
#    Ilnorm2<-multiplier*(t2-levlnorm(start,mu,sigma,1))
#  }
#  
#  return(Ilnorm2)
#}


ILFlnorm2 <-function(SeverityCurves,xvalues=0,xbase=1e6,SEEPLOT=FALSE)  {


  if (length(xvalues)==1)     {
     if (xvalues==0) {xvalues<-seq(1e5,20e6,1e5)}
     }
  bv<-ATLlnorm2(xbase,0,SeverityCurves)
  v<-sapply(xvalues,ATLlnorm2,m=SeverityCurves)
  p<-sapply(xvalues,slnorm2,m=SeverityCurves)
  
  df<-data.frame("x-values"=xvalues,"ExcedenceProb"=p,"LEV"=v,"ILF"=v/bv)

  #optional plot
   if (SEEPLOT==TRUE) {plot(df[,1],df[,4],type="l",col="red",log="",xlab="X-values",ylab="ILF") }

  return(df)
}

Ilnorm2<- function(start,end,mu,sigma,multiplier=1,moment=0)  {
  z1<-plnorm(start,mu,sigma,TRUE)
  z2<-ifelse(end==0,1,plnorm(end,mu,sigma,TRUE))      #treat 0 end value as infinite, with cum prob=1
  t<-exp(mu+.5*sigma^2)
  
  if (moment==0) {
    Ilnorm2<-multiplier*(z2-z1) 
   } else {
     #29/12/2012 corrected formula to use moment rather than 1
    t2<-ifelse(end==0,t,levlnorm(end,mu,sigma,moment))
    Ilnorm2<-multiplier*((t2-levlnorm(start,mu,sigma,moment)) -(end*(1-z2)-start*(1-z1)))  
  }
    
  return(Ilnorm2)
}

  ATLlnorm2<-function(Limit,Retention=0,m)  {
  i1<-findInterval(Retention,m$start)
  i2<-findInterval(Retention+Limit,m$start)
  m1<-0

    for (i in i1:i2)   {
      mu<-m$mu[i]
      sigma<-m$sigma[i]
      start<-m$start[i]
      end<-m$end[i]

      lo<-m$lo[i]
      hi<-m$hi[i]

      f<-(hi-lo)/Ilnorm2(start,end,mu,sigma)

      s<-max(Retention,start)
      e<-ifelse(end==0,Retention+Limit,min(Retention+Limit,end))

      m1<-m1+Ilnorm2(s,e,mu,sigma,f,1)

    }

    p2<-(Limit+Retention)*slnorm2(Limit+Retention,m)
    p1<-Retention*slnorm2(Retention,m)
    return ((m1+p2-p1)/slnorm2(Retention,m))
}



ILFlnorm3 <-function(SeverityCurves,xvalues=0,xbase=1e6,SEEPLOT=FALSE,
                     alpha=0,odf=2.5,lambda=100)  {
  
  #allows for loadings to premium  using standard deviation load
  #alpha= load factor (typically 15%-25%)
  #odf = overdispersion factor for claim number (typically 2.5 for heavy -use 20 for low)
  #lambda= expected claim number, for portfolio over year. Use default 100
  #relativity to UNLOADED IS 1+alpha*sqrt(1/h+E(X^2)/E(X)^2/lambda)
  
  if (length(xvalues)==1)     {
    if (xvalues==0) {xvalues<-seq(1e5,20e6,1e5)}
  }
  bv<-ATLlnorm2(xbase,0,SeverityCurves)
  v<-sapply(xvalues,ATLlnorm2,m=SeverityCurves)
  
  if (alpha>0) {
    bvm<-1+alpha*sqrt(1/odf+1/lambda*(1+COVlnorm2(xbase,0,SeverityCurves)^2))
    vm<- 1+alpha*sqrt(1/odf+1/lambda*
                      (1+sapply(xvalues,COVlnorm2,m=SeverityCurves)^2))
    bv<-bv*bvm
    v<-v*vm                                    
  }
  p<-sapply(xvalues,slnorm2,m=SeverityCurves)
  
  df<-data.frame("x-values"=xvalues,"ExcedenceProb"=p,"LEV"=v,"ILF"=v/bv)
  
  #optional plot
  if (SEEPLOT==TRUE) {plot(df[,1],df[,4],type="l",
                           col="red",log="",xlab="X-values",ylab="ILF") }
  
  return(df)
  
  #examples
#   m<-data.frame(lo=c(0),hi=c(1),mu=c(12),
#                  sigma=c(2),start=c(0),end=c(0))
#   #choose xvalues to use
#   xvalues<-c(1e5,250e3,5e5,1e6,2e6,3e6,5e6,10e6,15e6)
#   ILFlnorm3(m,xvalues<-xvalues,SEEPLOT=TRUE,alpha=.25,odf=2.5)
#   
#   #same results as per ILFlnorm2 when alpha is set to 0
#   ILFlnorm2(m,SEEPLOT=TRUE)
#   
#   #composite ILF
#   m<-data.frame(lo=c(0,.7),hi=c(.7,1),mu=c(11,12),
#                 sigma=c(1.5,2),start=c(0,1e6),end=c(1e6,0))
#   ILFlnorm3(m,xvalues<-xvalues,SEEPLOT=TRUE,alpha=.25,odf=2.5)
#   
#   #odf changes base price but relativities hardly altered
#   ILFlnorm3(m,xvalues<-xvalues,SEEPLOT=TRUE,alpha=.25,odf=1.1)
#   
#   #reducing number of risks makes ilfs steeper
#   ILFlnorm3(m,xvalues<-xvalues,SEEPLOT=TRUE,alpha=.25,odf=2.5,lambda=10) 
#   
#   #inducing number of risks makes ilfs flatter - back to same relativities
#   #as with no loading, but again base price is heavier  
#   ILFlnorm3(m,xvalues<-xvalues,SEEPLOT=TRUE,alpha=.25,odf=2.5,lambda=1000) 
#   ILFlnorm3(m,xvalues<-xvalues,SEEPLOT=TRUE,alpha=0) 
  
  
}

X2lnorm2<-  function(Limit,Retention=0,m)  {
    i1<-findInterval(Retention,m$start)
    i2<-findInterval(Retention+Limit,m$start)
    m2<-0
    
    for (i in i1:i2)   {
      mu<-m$mu[i]
      sigma<-m$sigma[i]
      start<-m$start[i]
      end<-m$end[i]
      
      lo<-m$lo[i]
      hi<-m$hi[i]
      
      f<-(hi-lo)/Ilnorm2(start,end,mu,sigma)
      
      s<-max(Retention,start)
      e<-ifelse(end==0,Retention+Limit,min(Retention+Limit,end))
      
      m2<-m2+Ilnorm2(s,e,mu,sigma,f,2)
      
    }
    
    p2<-((Limit+Retention)^2)*slnorm2(Limit+Retention,m)
    p1<-(Retention^2)*slnorm2(Retention,m)
    return ((m2+p2-p1)/slnorm2(Retention,m))
  }

COVlnorm2<-function(Limit,Retention=0,m)  {
  return(sqrt(X2lnorm2(Limit,Retention=0,m)/
                ATLlnorm2(Limit,Retention=0,m)^2 -1))
  #example
#   m<-data.frame(lo=c(0),hi=c(1),mu=c(11),
#                 sigma=c(2),start=c(0),end=c(0))
#   #theoretical COV unlimited is sqrt(exp(sigma^2)-1)
#   COVlnorm2(1e12,m=m)
#   sqrt(exp(2^2)-1)  #should be the same!
  
}


#Author:  Ben Lawson
#Date Added: 9/19/2013
#Purpose:  Function to piecewise define a lognormal data approximation 
#based upon variable breaks in the data

FitPiecewiseLNorm <- function(x, breaks) {
  
  breaks <- union(breaks, c(Inf, 0))
  breaks <- breaks[breaks >= 0]
  breaks <- sort(breaks)
  
  n <- length(breaks) - 1
  
  out <- rep(0, length.out = n * 6)
  dim(out) <- c(n, 6)
                                          
  for(i in 1:n) {
    f <- Fitlnorm(x, start = breaks[i], end = breaks[i + 1])
    out[i, 3] <- f$estimate[1]
    out[i, 4] <- f$estimate[2]
    
    out[i, 1] <- sum(x <= breaks[i]) / length(x)
    out[i, 2] <- sum(x <= breaks[i + 1]) / length(x)
    
    out[i, 5] <- breaks[i]
    out[i, 6] <- breaks[i + 1]
    
  }
  
  out <- as.data.frame(out)
  
  names(out) <- c('lo', 'high', 'mu', 'sigma', 'start', 'end')
  
  return(out)
  
}