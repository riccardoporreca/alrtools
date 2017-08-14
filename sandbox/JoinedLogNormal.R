#desc: chart of joined lognormals , with overlay of actual data points
#      allow option of fitting too

#version:  1.00 21Jun2012 - basic chart

#read in df of joined lognormals
#test case
m<-data.frame(lo=c(0,.3,.8),hi=c(.3,.8,1),mu=c(10,11,12),sigma= c(3,2,1.5),
              start=c(0,1e5,1e6),end=c(1e5,1e6,0))

#create compound function to define distribution cdf
pJoinedLN<-function(x,m)  {
  
  #check m is a joined lognormal
    stop(if identical(m$lo[-1],m$hi[-dim(m)[1]])==False )
}