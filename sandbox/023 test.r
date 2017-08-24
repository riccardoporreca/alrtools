#Development to Test stacked graph features in ggplot2

#create dummy data

library(XLConnect)
setwd("w:/sl/actuarial/R-Software/Mango 2014Course/Test")
data<-readWorksheetFromFile("TestData.xlsx",sheet="Test")

data$Year<-factor(data$Year)

library(ggplot2)
q<-ggplot(data=data,aes(x=FocusArea,y=Value,fill=Measure))
q<-q+geom_bar(stat="identity",position="stack")
q<-q+facet_grid(.~Year)
q

q<-ggplot(data=data,aes(x=Year,y=Value,fill=Measure))
q<-q+geom_bar(stat="identity",position="stack")
q<-q+facet_grid(.~FocusArea)
q


q<-ggplot(data=data,aes(x=Year,y=Value,fill=Measure))
q<-q+geom_bar(stat="identity",position="stack")
q<-q+facet_grid(FocusArea~.)


summary(q)

#add % display for y axis
library(scales)
q<-q+ylab("Loss Ratio")+ scale_y_continuous(labels = percent)
q


#try and add page numbers to output
QtrToUse<-'2014q1'
path<-"W:/SL/Actuarial/ClaimCategorisation_ALR/Quarters/"

#load source file 
source('w:/sl/actuarial/r-software/utilities/Dev/SLPseudoPackage.R')

#needed for plotting
#source("W:/SL/Actuarial/ClaimCategorisation_ALR/Projects/BZLYCharts/BZLYCharts/R/ChartFunctions.r")

library(BZLYCharts)

#load from ulrs
load(paste(path,QtrToUse,"/ulrs.RData",sep=""))


#remove factors
ulrC<-unfactor(ulrC)   # Loading in the Combined Lists

#available COBs
CBSel<-sort(unique(ulrC$COB),decreasing=F)

#try adding "Held"
resH<-subset(ulrC,Type=="Blend")  

#add Held - can caclculate ibnr from this
resH$Loss_Ratio<-resH$Ultimate_Claims/resH$Premiums
resH$Type<-"Held(prevQPrePeer)"
ulrC<-rbind(ulrC,resH)


head(ulrC)

#select just 4 to try
COBs<-c("A&E MM & PE","D&O","EPL","PI-Lawyers exBLPT")

ulr<-subset(ulrC,COB %in% COBs)
#just tke curren position and ignore irrelevant columns
ulr<-ulr[ ulr$Development_Quarter==4*(2014-as.numeric(ulr$YOA))+1,c(2:4,8)]







#below doesnt work!
q<-ggplot(data=data,aes(x=Year,y=Value))
q<-q+geom_path(position="stack")
q<-q+facet_grid(.~FocusArea)
q

#OBJECTS

statSummary<-function(x) {
  r<-list(x,mean(x),sd(x))
  names(r)<-c("data","mean","sdev")
  class(r)<-"statSummary"
  r
}

print.statSummary<-function(x,...) {
  cat("mean = ",x$mean,"\nstandard deviation = ",x$sdev,"\n",sep="")
}

plot.statSummary<-function(x,...) {
  hist(x$data,...)
  abline(v=x$mean,col="red")
}

plot(x,col="blue",main="hello")

x<-1:5
x<-statSummary(x)
x

class(x)
statSummary(x)

attr(x,"class")


#create S4 version of statSummary (say SS)

setClass("SS",representation(data="numeric",mean="numeric",sd="numeric"))

statSummary2<-function(x) {
  new("SS",data=x,mean=mean(x),sd=sd(x))
}

SS2<-function(x) {new("SS",x=)}
x<-new("SS",data=1:10,)
?setClass

#expressions 
#evalS<-function (chard) {eval(parse(text= paste("head = ",chard,sep="") ) )}
evalS<-function (chard) {eval(parse(text= paste("head = ",chard,sep="") ) )}

head(evalS)


#sortDatas
#use data

data[order(data$FocusArea),]
SortData<-function (data,ls) {
  data[do.call("order",data[,ls]),]
}

SortData(data=data,c("FocusArea","Measure"))


#set up with ellipsis
histplot<-function(col="blue",ls,... ) {
  n<-length(ls)
  par(mfrow=c(n,1))
  invisible(lapply(ls,hist,col=col,...))
}

ls<-list(a=rnorm(10),b=rnorm(5),c=rpois(10,2))
histplot(ls=ls,col="red")

options()

#shiny

#testthat
library(testthat)
#example
expect_that(mean(1:10), is_identical_to(5.5))
expect_that(mean(1:10), equals(5.4))
expect_that(mean(1:10), equals("help"))
expect_that(5+p, throws_error(),"tests error is generated")   #passes



toRoman<-function(x) {
  if(!is.integer(x)) stop("must be an integer")
  if(x<1|| x>3999) stop("must be between 1 and 3999")

  RomanTable<-data.frame(intv=c(1,5,10,50,100,500,1000),char=c("I","V","X","L","C","D","M"))
  
  ?loop
  res<-NextRomanBlock(x,RomanTable)
  x<-res$x
  q<-paste0(q,res$q)
  
  res<-NextRomanBlock(x,500,"D")
  x<-res$x
  q<-paste0(q,res$q)
  
  res<-NextRomanBlock(x,100,"C")
  x<-res$x
  q<-paste0(q,res$q)

  res<-NextRomanBlock(x,500,"D")
  x<-res$x
  q<-paste0(q,res$q)
  
  res<-NextRomanBlock(x,500,"D")
  x<-res$x
  q<-paste0(q,res$q)
  
  res<-NextRomanBlock(x,500,"D")
  x<-res$x
  q<-paste0(q,res$q)
  
  n<-floor(x/1000)
  if(n>0) {
    q<-rep("M",n)
    x<-x-n*1000
  }
  
  n<-floor(x/500)
  q<-paste0(q,paste0(rep("D",n),collapse=""))
  x<-x-n*500
  
  n<-floor(x/100)
  q<-q+rep("C",n)
  x<-x-n*100
  
  n<-floor(x/50)
  q<-q+rep("L",n)
  x<-x-n*50
  
  n<-floor(x/10)
  q<-q+rep("X",n)
  x<-x-n*10
  
  n<-floor(x/5)
  q<-q+rep("V",n)
  x<-x-n*5
  
  q<-q+rep("I",n)
  
#use paste to get output 
  cat(q,sep="")
}

NextRomanBlock<-function(x,...) {
   
  i<-findInterval(x,RomanTable$intv)
  
  n<-RomanTable$intv[i]
  char<-RomanTable$char[i]
  
  m<-floor(x/n)
  q<-NULL
  if(m>0) {
    q<-paste0(rep(char,m),collapse="")
    x<-x%%n
  }
  return(list(q=q,x=x))
}

LoopRomanBlock<-function(x) {
  res<-NextRomanBlock(x,RomanTable)
  x<-res$x
  q<-paste0(q,res$q)
}