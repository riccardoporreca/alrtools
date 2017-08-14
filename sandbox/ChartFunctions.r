# Author: Simon Brickman
# Date:   July 24, 2012
# Description:
#
#   Functions for smart plotting 
#     triangle-type Pre Peer-like charts
#
# Examples:
#
#   Plot development to ultimate
#
#     pdf(file = "EPL OpenMarket NonBlock RAG Report 2.pdf")
#     plotchart(res, HeadTitle = "EPL Premium Frequency")
#     plotchart(res, vals = "FREQ_FTE_100K", yl = "Claims per 000 FTE", , HeadTitle = "EPL HeadCount Frequency")
#     dev.off()
#
#amended 7/Apr/2013 
# to deal with scaling with no factors (before scaling was appluied after setting maximum)
#
#Note function requires legend to be defined (can deal with no factgors but cannot yet
#cope with no legend)

#amended 29/Apr/2013
# to allow non zero as mimimum yaxis value (if  above zero)
# added xscale.by to operate as per scale.by

#amended 17/july 2013
# force BZLY selection of colours and fix to particular year and add shapes  

plotchart <- function(df, fac = "ClaimSeverityInd", 
                          vals = "FREQ_EPI_1m", 
                          leg = "YOA",
                          x = "dev", 
                          xl = "Development quarters", 
                          yl = "Claims per ?m of premiums", 
                          HeadTitle = "",
                          scale.by = NULL,
                          xscale.by=NULL,
                          yScaleMin=NULL,
                          usedirectlabel=TRUE,
                          UseZeroYaxis=TRUE,
                          useshape=TRUE,
                          useBZLYcolor=TRUE,
                          BZLYcol=c("red","blue","orange","green","darkred","darkblue",
                                     "darkorange","darkolivegreen"),
                          tgtyr=2007,  
                          tgtpos=5,
                          tgtshape=1  #minimum 1
                      ) {

  
#   #test
#             df<-res2
#             fac<-"Type"
#             vals<-"Loss_Ratio"
#             x<-"Development_Quarter"
#             yl<-"Loss Ratio"
#             leg<-"YOA"
#             HeadTitle<-paste(paste("COB: ", CB,sep=""), 
#                             "Loss Ratios", sep = "\n")
#             xl = "Development quarters"

 
#             CB="Test"
#             df=res2
#             fac="ClaimSeverityInd"
#             vals="Ratio"
#             x="Development_Quarter"
#             xl = "Development quarters"
#             yl="Claims per $m of premiums"
#             leg="YOA"
#             HeadTitle=paste("COB: ", CB, "\nFrequency/$m", sep = "")
#   
#             scale.by = NULL
#             xscale.by=NULL
#             yScaleMin=NULL
#             usedirectlabel=FALSE
#             UseZeroYaxis=TRUE
#             useshape=TRUE
#             useBZLYcolor=TRUE
#             BZLYcol=c("red","blue","orange","green","darkred","darkblue",
#                       "darkorange","darkolivegreen")
#             tgtyr=2007
#             tgtpos=5
#             tgtshape=1
#   
#added 18/4/2014
#select df as dataframe  
  
  if ("data.frame" %in% class(df) ==FALSE) stop ("df is not a dataframe")
  df<-as.data.frame(df)
  
  
  #remove any rows with NA or Inf
  df<-  na.omit(df)
  df<-df[!is.infinite(df[,vals]),]
  
  if(leg=="YOA" & useBZLYcolor==TRUE) {
     
    yrlst<-sort(unique(as.numeric(df[,leg])))
    
    lensym<-15                #number of symbols to use
    BZLYsym<-0:(lensym-1)
    lencol<-length(BZLYcol)
           
    cs<-BZLYcol[(tgtpos+yrlst-tgtyr-1)%%lencol+1]
    ct<-BZLYsym[(tgtshape+yrlst-tgtyr-1)%%lensym+1]
  }
  
   
  library(ggplot2)
  library(directlabels)
  
  
  # Check scale.by 
  if (is.null(scale.by)) {
    if (left(toupper(yl), 10) == "LOSS RATIO") {
      scale.by <- 100
    } else {
      scale.by <- 1
    }
  }
  
  # Check xscale.by 
  if (is.null(xscale.by)) xscale.by <- 1
  
  
  if (fac == "" || is.null(fac)) {
    df <- df[, c(x, vals, leg)]
    names(df) <- c("x", "vals", "leg")
    
    if (!is.factor(df$leg)) df$leg <- factor(df$leg)
    
    # Apply scale.by
    df$vals <- df$vals * scale.by
    df$x <- df$x * xscale.by
    
    ylmax<-max(df$vals,na.rm=TRUE)
    if (is.null(yScaleMin)==FALSE) ylmax<-max(ylmax,yScaleMin)
    #fixed 7/apr/2013 to allow for scaling with no factors
    ylmin<-min(df$vals,na.rm=TRUE)
    if(UseZeroYaxis==TRUE) ylmin=min(0,ylmin)
    
      pb<-qplot(
        x,
        vals, 
        data = df, 
        colour = leg,
        main = paste(HeadTitle),
        xlab = xl,
        ylab = yl,
        geom = c("point", "line"),
        ylim=c(ylmin,ylmax)  
      )
      
      if(useshape==TRUE) pb<-pb+aes(shape=leg)
      
      if(leg=="YOA" & useBZLYcolor==TRUE) {
        # pb+scale_colour_brewer(palette = "Set1") #is alternative color palette to use
        pb<-pb+scale_shape_manual(values = ct)+scale_colour_manual(values= cs)
      }
      
      pb<-pb+scale_x_discrete(limits=seq(from=0,to=max(df[,"x"]),by=4))
    
      if (usedirectlabel==TRUE) {
        print(direct.label(pb))   
      }else{
        print(pb)                       
      }
    
  } else {
    df <- df[, c(x, vals, leg, fac)]
    names(df) <- c("x", "vals", "leg", "fac")
    
    if (!is.factor(df$fac)) df$fac <- factor(df$fac)
    if (!is.factor(df$leg)) df$leg <- factor(df$leg)
    
    # Apply scale.by
    df$vals <- df$vals * scale.by
    df$x <- df$x * xscale.by  
    
    for (j in as.character(levels(df$fac))) { 
      
      tempC <- df[df$fac == j, ] 
      
      ylmax<-max(tempC$vals,na.rm=TRUE)
      if (is.null(yScaleMin)==FALSE) ylmax<-max(ylmax,yScaleMin)
      ylmin<-min(tempC$vals,na.rm=TRUE)
      if(UseZeroYaxis==TRUE) ylmin=min(0,ylmin)
      
      pb<-qplot(
          x,
          vals, 
          data = tempC, 
          colour = leg,
          main = paste(HeadTitle, "  ", fac, ": ", j, sep=""),
          xlab = xl,
          ylab = yl,
          geom = c("point", "line"),
          ylim=c(ylmin,ylmax)                                                       
        )
      
      if(useshape==TRUE) pb<-pb+aes(shape=leg)
      
      if(leg=="YOA" & useBZLYcolor==TRUE) {
        pb<-pb+scale_shape_manual(values = ct)+scale_colour_manual(values= cs)
      }
      
      pb<-pb+scale_x_discrete(limits=seq(from=0,to=max(tempC[,"x"]),by=4))
#         pb+scale_x_discrete(step=4)
      
      
      if(!dim(tempC)[1]==0) {
        if (usedirectlabel==TRUE) {
          print(direct.label(pb))
        }else{
          print(pb)
        }
        
      } #end if for printing plot (trapping for zero rows)
    } # end for
  } # end if
} # end function


toDate<-function(colnum,df) {
  # convert posixct date to date and handles GMT/BST
  as.Date(df[,colnum]+3600) }

monthsteps<-function(d1,d2) {
  #date:    21/feb/2013
  #purpose: create monthly vector from start to end month inclusive
  #version: 1.0 
  #author:  Simon Brickman
  
  stopifnot(is.Date(d1) & is.Date(d2))             #check we have dates
  n<-12*(year(d2)-year(d1))+(month(d2)-month(d1))  #calculate length inclusive of opening and closing months
  s<-(month(d1)+0:n)                               #create vector of this length
  i<-(s-1)%%12 +1                                  #months for each element
  j<-(s-1)%/%12                                    #years for each element
  v<-as.Date(paste(year(d1)+j,i,1,sep="-"))        #turn into dates
  return(v)
  
  #example
  #   d1<-as.Date("2007-03-31")
  #   d2<-as.Date("2009-02-01")
  #   monthsteps(d1,d2)
  #   
  
}



MAChart<-function(MAP=12,df,xcol=5,ycol=4,ywt=3,xstep=1,
                  xlab="Date",ylab="Duration",main="",leg="",fac="",xRotate=FALSE) {
  
  # Author: Simon Brickman
  # Date:   February 21 2013
  # Description:
  #
  #   creates moving average of ycol with term MAP
  #   against xcol
  #   option of using weights ywt
  #   assumes columns are contained in dataframe df, and that xcol is complete
  #   can use function monthsteps to create monthly steps
  #   Add option to see by factor eg size or category
  #
  # Amended 7th May 2013
  # added xRotate to allow axis to be rotated (default false)
  
  
  # #   #testing
  #   df<-as.data.frame(CAS2)
  #   ywt<-3
  #   MAP<-12
  #   ycol<-4
  #   xcol<-5  #try dates
  #   xstep<-6
  #   ylab<-"Duration" 
  #   xlab<-"Closed Month"
  #   main="TMB"
  #   leg="ClaimSeverityInd"
  
  
  
  library(ggplot2)
  library(directlabels)
  library(scales)
  
  #set up xstep skip
  xstp<-paste(xstep," months",sep="")
  
  fn <- rep(1/MAP, MAP)
  
  #message in heading on claims involved
  if (is.na(ywt)) {
    totmsg<-""
  }else{
    totwt<-sum(df[,ywt])
    totmsg<-paste(" with total ",totwt,sep="")
  }
  
  
  if (fac == "" || is.null(fac)) {
    
      if (leg==""|| is.null(leg)){
        
        if (is.na(ywt)) {
          y1_lag <- filter(df[,ycol], fn, sides=1)
        }else{
          y1_lag <- filter(df[,ycol]*df[,ywt], fn, sides=1)/
            filter(df[,ywt], fn, sides=1)
        }
        
        y1_lag[is.na(y1_lag)]<-0
        tempC<-data.frame(x=df[-(1:(MAP-1)),xcol],y=y1_lag[-(1:(MAP-1))])
        
        m<-ggplot(tempC, aes(x, y))
        m<-m +geom_line() +geom_point()
        m<-m +scale_x_date(labels = date_format("%m-%Y"), breaks = date_breaks(xstp))
        if(xRotate==TRUE) m<-m+ theme(axis.text.x=element_text(angle=90, hjust=1))
        m<-m+xlab(xlab)+ylab(ylab)
        m<-m+ggtitle(paste(main,totmsg,sep=""))
        print(m)   #cannot do direct label as no legend to separate out
        
      }else{
        if (!is.factor(df[,leg])) df[,leg] <- factor(df[,leg]) 
        dfc<-NULL
        for (j in as.character(levels(df[,leg]))) { 
          tempC <- df[df[,leg] == j, ] 
          if (is.na(ywt)) {
            y1_lag <- filter(tempC[,ycol], fn, sides=1)
          }else{
            y1_lag <- filter(tempC[,ycol]*tempC[,ywt], fn, sides=1)/
              filter(tempC[,ywt], fn, sides=1)
          }
          
          y1_lag[is.na(y1_lag)]<-0
          
          tempC<-data.frame(x=tempC[-(1:(MAP-1)),xcol],y=y1_lag[-(1:(MAP-1))],leg=j)
          if (is.null(dfc)) {
            dfc<-tempC
          }else{  
            dfc<-rbind(dfc,tempC)
          }  
        }
        
        m<-ggplot(dfc, aes(x, y,col=leg,group=leg))
        m<-m +geom_line() +geom_point()
        m<-m +scale_x_date(labels = date_format("%m-%Y"), breaks = date_breaks(xstp))  
        if(xRotate==TRUE) m<-m+ theme(axis.text.x=element_text(angle=90, hjust=1))
        #brillaint way to skip on xaxis
        m<-m+xlab(xlab)+ylab(ylab)
        m<-m+ggtitle(paste(main,totmsg,sep=""))
        print(direct.label(m))
        
      }  #end if 
  
  } else {
        
    if (!is.factor(df$fac)) df$fac <- factor(df[,fac])
    
    for (j in as.character(levels(df$fac))) { 
      
      dfsub <- df[df$fac == j, ]   
      
      
      if (leg==""|| is.null(leg)){
        
        if (is.na(ywt)) {
          y1_lag <- filter(dfsub[,ycol], fn, sides=1)
        }else{
          y1_lag <- filter(dfsub[,ycol]*dfsub[,ywt], fn, sides=1)/
            filter(dfsub[,ywt], fn, sides=1)
        }
        
        y1_lag[is.na(y1_lag)]<-0
        tempC<-data.frame(x=dfsub[-(1:(MAP-1)),xcol],y=y1_lag[-(1:(MAP-1))])
        
        m<-ggplot(tempC, aes(x, y))
        m<-m +geom_line() +geom_point()
        m<-m +scale_x_date(labels = date_format("%m-%Y"), breaks = date_breaks(xstp))
        if(xRotate==TRUE) m<-m+ theme(axis.text.x=element_text(angle=90, hjust=1)) 
        m<-m+xlab(xlab)+ylab(ylab)  
        m<-m+ggtitle(paste(main, "  ", fac, ": ", j,totmsg,sep=""))
        print(m)   #cannot do direct label as no legend to separate out
        
      }else{
        if (!is.factor(dfsub[,leg])) dfsub[,leg] <- factor(dfsub[,leg]) 
        dfc<-NULL
        for (j in as.character(levels(dfsub[,leg]))) { 
          tempC <- dfsub[dfsub[,leg] == j, ] 
          if (is.na(ywt)) {
            y1_lag <- filter(tempC[,ycol], fn, sides=1)
          }else{
            y1_lag <- filter(tempC[,ycol]*tempC[,ywt], fn, sides=1)/
              filter(tempC[,ywt], fn, sides=1)
          }
          
          y1_lag[is.na(y1_lag)]<-0
          
          tempC<-data.frame(x=tempC[-(1:(MAP-1)),xcol],y=y1_lag[-(1:(MAP-1))],leg=j)
          if (is.null(dfsubc)) {
            dfc<-tempC
          }else{  
            dfc<-rbind(dfc,tempC)
          }  
        }
        
        m<-ggplot(dfc, aes(x, y,col=leg,group=leg))
        m<-m +geom_line() +geom_point()
        m<-m +scale_x_date(labels = date_format("%m-%Y"), breaks = date_breaks(xstp))  
        if(xRotate==TRUE) m<-m+ theme(axis.text.x=element_text(angle=90, hjust=1))
        #brillaint way to skip on xaxis
        m<-m+xlab(xlab)+ylab(ylab)
        m<-m+ggtitle(paste(main,totmsg,sep=""))
        print(direct.label(m))
        
      }  #end if 
    }
  }
  #example
  #   MAChart(df=as.data.frame(CAS2),ywt=3,main="A&E (12m moving average)",xcol=5,ycol=4
  #           ,xlab="Closed Month",ylab="Average Duration (days)",xstep=3,leg="ClaimSeverityInd")
  
}    #end function


CreateClosureDf <- function(df,Close="Closed",IncFees=T) {
  
  # Author: Simon Brickman
  # Date:   Sep 2012
  # Description:
  #
  #   Function for ClosureRates work
  
  #test 
#     df<-rs
#     Close<-"Closed"
#     IncFees<-F
# df<-sC2
# Close<-"C"
# IncFees<-T  
   
  RS<-data.table(df)
    
  #rename
  setnames(RS,"yr","yoa") 
  
  setkey(RS,yoa,dev)
  
  if(IncFees==T) {
      RST<-RS[,list(Num=sum(numb),NZ=sum(nilclaims),Inc=sum(inc)),by=key(RS)]
      setkey(RS,yoa,dev,Status)
      RSS<-RS[,list(Num=sum(numb),NZ=sum(nilclaims),Inc=sum(inc)),by=key(RS)]
    }else{
      RST<-RS[,list(Num=sum(numb),NZ=sum(nilclaims),Inc=sum(incxf)),by=key(RS)]
      setkey(RS,yoa,dev,Status)
      RSS<-RS[,list(Num=sum(numb),NZ=sum(nilclaims),Inc=sum(incxf)),by=key(RS)]
  }  
    
  
  #now merge
  RSU<-RST[RSS]
  RSU<-RSU[,numPC:=Num.1/Num]   
  RSU<-RSU[,incPC:=Inc.1/Inc]
  RSU<-RSU[,NZPC:=NZ.1/Num]
    
  
  #remove unwanted
  RSU<-RSU[,Num.1:=NULL]
  RSU<-RSU[,Inc.1:=NULL]
  RSU<-RSU[,NZ.1:=NULL]
  RSU<-RSU[,NZ:=NULL]  
  
  
  #we had problems with this before - change from subset
  rsu<-RSU[RSU$Status==Close,]
  
  #trap for no closed claims! (eg Crime 2011)
  if(dim(rsu)[1]==0) return(NULL)
  

  #turn rsu yoa into factor
  rsu$yoa<-factor(rsu$yoa)
  rsv<-as.data.frame(rsu)  #must be dataframe to work (cann't leave as data.table too)
  
  rsa<-rsv
  rsa$Type<-"number"
  #rename column
  names(rsa)[match("numPC",names(rsa))]<-"ClosedPC"
  #remove unwanted column
  rsa<-rsa[,-match("incPC",names(rsa))]
  
  rsb<-rsv
  rsb$Type<-"incurred"
  #rename column
  names(rsb)[match("incPC",names(rsb))]<-"ClosedPC"
  #remove unwanted column
  rsb<-rsb[,-match("numPC",names(rsb))]
  
  #join together
  rsw<-rbind(rsa,rsb)
  
  return(rsw)
}

incSplit<-function(df,splitcol="ClosedPC",grpcol="yoa",sortcol="dev") {
  
  #function to create incremental values of column splitcol in dataframe df
  #adds new column called inc to dataframe 
  #and shows disaggregation of column splitcol within column grpcol, sorted by sortcol
  
  #find cols
  j<-match(grpcol,names(df)) 
  k<-match(splitcol,names(df)) 
  l<-match(sortcol,names(df)) 
  
  stopifnot(!is.na(j),!is.na(k),!is.na(l)) 
  
  #ensure data is sorted initially
  idx<-with(df,order(df[,j],df[,l]))
  df<-df[idx,]
  
  s<-split(df,df[,j])
  
  for (i in 1:length(s)){
    x<-c(s[[i]][1,k],diff(s[[i]][,k]))
    s[[i]]$step<-x
  }
  
  df<-unsplit(s,df[,j])
  return(df)
}

incSum<-function(df,inccol="step",grpcol="yoa",sortcol="dev") {
  
  #function to create cumulative sum column cum from column inccol in dataframe df
  #adds new column called cum to dataframe 
  #and shows accumulation of column inccol within column grpcol
  
  #find cols
  j<-match(grpcol,names(df)) 
  k<-match(inccol,names(df)) 
  l<-match(sortcol,names(df)) 
  
  stopifnot(!is.na(x=j),!is.na(k),!is.na(l)) 
  
  #ensure data is sorted initially
  idx<-with(df,order(df[,j],df[,l]))
  df<-df[idx,]
  
  s<-split(df,df[,j])
  
  for (i in 1:length(s)){
    x<-cumsum(s[[i]][,k])
    s[[i]]$cum<-x
  }
  
  #regroup
  df<-unsplit(s,df[,j])
  
  return(df)
}

MeshDev<-function(df1,df2,splitcol="Num",grpcol="yoa",sortcol="dev",Type="Number") {
  
  #function to create cumulative sum column cum from column splitcol in dataframes df1,df2
  #which meshes together the sortcol
  #must have Type column for measure type to use
  
  #requires dataframes to have common columns for splitcol,grpcol and sortcol
  
  #check no factors
  df1<-unfactor(df1)
  df2<-unfactor(df2)
  
  #choose by Type
  df1<-df1[df1$Type==Type,]
  df2<-df2[df2$Type==Type,]
  
  #add field to show source
  df1$source<-"df1"
  df2$source<-"df2"
  
  #now create incremental
  s<-incSplit(df1,splitcol=splitcol,grpcol=grpcol,sortcol=sortcol)
  t<-incSplit(df2,splitcol=splitcol,grpcol=grpcol,sortcol=sortcol)
  df<-rbind(s,t)
  
  #sort by yoa and dev then create cumsum within yoa
  #find cols
  j<-match(grpcol,names(df)) 
  l<-match(sortcol,names(df)) 
  
  idx<-with(df,order(df[,j],df[,l]))
  df<-df[idx,]
  
  #apply cumsum within yoa
  df<-incSum(df,grpcol=grpcol,sortcol=sortcol)
  
  return(df)
}


WeightedMeshDev<-function(df1,df2,splitcol="Num",grpcol="yoa",sortcol="dev",sharecol="ClosedPC",Type="Number") {
  
  #function to create cumulative sum column cum from column splitcol in dataframes df1,df2
  #which meshes together the sortcol
  #must have Type column for measure type to use, and ClosedPC column to work out totals
  
  #for testing 
#     df1<-rs_KP
#     df2<-rs_kbr
#     splitcol<-"Num"
#     grpcol<-"yoa"
#     sortcol<-"dev"
#     sharecol<-"NZPC"
#     Type<-"number"    
  
  denum<-MeshDev(df1,df2,splitcol=splitcol,grpcol=grpcol,sortcol=sortcol,Type=Type) 
  
  l<-match(sharecol,names(denum)) 
  
  df1<-CreatePartCol(subset(df1,Type=Type),splitcol=splitcol,sharecol=sharecol)
  df2<-CreatePartCol(subset(df2,Type=Type),splitcol=splitcol,sharecol=sharecol)
  
  df<-MeshDev(df1,df2,splitcol="Part",grpcol=grpcol,sortcol=sortcol,Type=Type) 
  
  #checks
  #   df1[df1$yr==2010 & df1$Type=="number" & df1$dev>7 & df1$dev<8,]
  #   df2[df2$yr==2010 & df2$Type=="number" & df2$dev>7 & df2$dev<8,]
  #   denum[denum$yr==2010 & denum$Type=="number" & denum$dev>7 & denum$dev<8,]
  #   df[df$yr==2010 & df$Type=="number" & df$dev>7 & df$dev<8,]
  
  denum[,l]<-ifelse(denum$cum==0,0,df$cum/denum$cum)
  
  #checks
  #   denum[denum$yr==2010 & denum$Type=="number" & denum$dev>7 & denum$dev<8,]
  
  return(denum)
}


CreatePartCol<-function(df,splitcol="Num",sharecol="ClosedPC"){
  #find cols
  k<-match(splitcol,names(df)) 
  l<-match(sharecol,names(df)) 
  
  df$Part<-df[,k]*df[,l]
  df$Part[is.na(df$Part)]<-0  #replace NAs with 0
  
  return(df)
}

#create test data
# test1<-data.frame(dev=c(rep(c(1,3,5,7,1,3),2)),
#                   ClosedPC=c(c(20,45,55,70,30,40),
#                              c(20,5,15,40,50,20)),
#                   yoa=c(rep(c(2011,2011,2011,2011,2012,2012),2)),
#                   Num=c(rep(c(50,70,90,100,40,80),2)),   #totals
#                   Inc=c(rep(c(500e3,700e3,1e6,2e6,3e6,4e6),2)),
#                   Type=c(rep.int("Number",6),
#                          rep.int("Incurred",6)))
# test2<-data.frame(dev=c(rep(c(2,4,6,2),2)),
#                   ClosedPC=c(c(40,50,60,45),
#                              c(60,20,40,50)),
#                   yoa=c(rep(c(2011,2011,2011,2012),2)),
#                   Num=c(rep(c(100,140,180,150),2)),
#                   Inc=c(rep(c(200e3,500e3,1e6,300e3),2)),
#                   Type=c(rep.int("Number",4),
#                          rep.int("Incurred",4)))  
# 
# 
# u<-WeightedMeshDev(test1,test2)
# v<-WeightedMeshDev(test1,test2,Type="Incurred",splitcol="Inc")
# res_test<-rbind(u,v)
# 
# plotchart(res_test,fac="Type",vals="ClosedPC",x="dev",yl="Closed",
#           leg="yoa",HeadTitle="Composite Closed%")




#this is used by library(lattice) for stacked  charts
panel.stackedDens <-
  function(x, y,
           overlap = 0.3,
           horizontal = TRUE,
           
           alpha = plot.polygon$alpha,
           border = plot.polygon$border,
           lty = plot.polygon$lty,
           lwd = plot.polygon$lwd,
           col = plot.polygon$col,
           
           varwidth = FALSE,
           ref = TRUE,
           
           bw = NULL,
           adjust = NULL,
           kernel = NULL,
           window = NULL,
           width = NULL,
           n = 50,
           from = NULL,
           to = NULL,
           cut = NULL,
           na.rm = TRUE,
           
           ...)
{
    if (all(is.na(x) | is.na(y))) return()
    x <- as.numeric(x)
    y <- as.numeric(y)
    
    library(lattice)
    
    reference.line <- trellis.par.get("reference.line")
    plot.polygon <- trellis.par.get("plot.polygon")
    
    ## density doesn't handle unrecognized arguments (not even to
    ## ignore it).  A tedious but effective way to handle that is to
    ## have all arguments to density be formal arguments to this panel
    ## function, as follows:
    
    darg <- list()
    darg$bw <- bw
    darg$adjust <- adjust
    darg$kernel <- kernel
    darg$window <- window
    darg$width <- width
    darg$n <- n
    darg$from <- from
    darg$to <- to
    darg$cut <- cut
    darg$na.rm <- na.rm
    
    my.density <- function(x) do.call("density", c(list(x = x), darg))
    
    numeric.list <- if (horizontal) split(x, factor(y)) else split(y, factor(x))
    levels.fos <- as.numeric(names(numeric.list))
    d.list <- lapply(numeric.list, my.density)
    ## n.list <- sapply(numeric.list, length)  UNNECESSARY
    dx.list <- lapply(d.list, "[[", "x")
    dy.list <- lapply(d.list, "[[", "y")
    
    max.d <- sapply(dy.list, max)
    if (varwidth) max.d[] <- max(max.d)
    
    ##str(max.d)
    
    xscale <- current.panel.limits()$xlim
    yscale <- current.panel.limits()$ylim
    height <- (1+overlap)
    
    if (horizontal)
    {
      for (i in rev(seq_along(levels.fos)))
      {
        n <- length(dx.list[[i]])
        panel.polygon(x = dx.list[[i]][c(1, 1:n, n)],
                      y = levels.fos[i] - 0.5 + height * c(0, dy.list[[i]], 0) / max.d[i],
                      col = col, border = border,
                      lty = lty, lwd = lwd, alpha = alpha)
        if (ref)
        {
          panel.abline(h = levels.fos[i] - 0.5,
                       col = reference.line$col,
                       lty = reference.line$lty,
                       lwd = reference.line$lwd,
                       alpha = reference.line$alpha)
        }
      }
    }
    else
    {
      for (i in rev(seq_along(levels.fos)))
      {
        n <- length(dx.list[[i]])
        panel.polygon(x = levels.fos[i] - 0.5 + height * c(0, dy.list[[i]], 0) / max.d[i],
                      y = dx.list[[i]][c(1, 1:n, n)],
                      col = col, border = border,
                      lty = lty, lwd = lwd, alpha = alpha)
        if (ref)
        {
          panel.abline(v = levels.fos[i] - 0.5,
                       col = reference.line$col,
                       lty = reference.line$lty,
                       lwd = reference.line$lwd,
                       alpha = reference.line$alpha)
        }
      }
    }
    invisible()
  }


#example

# 
# 
# overlap <- 0.3
# load("~/2013RConference/Claims.RData")
# 
# df<-sClaimCat.anon[   sClaimCat.anon$IncurredUSD>0 & 
#                         sClaimCat.anon$PeriodEnding %in% c(pdq[1],pdq[2]) &
#                         sClaimCat.anon$YOA<2011,]
# df$YOA<-factor(df$YOA)  #needed for labels on y-axis
# bwplot(YOA ~ IncurredUSD|ClaimStatus, df,
#        panel = panel.stackedDens,
#        overlap = overlap,
#        lattice.options = list(axis.padding = 
#                                 list(factor = c(0.6, 1 + overlap))),
#        scales=list(x=list(log=TRUE)),   #does log scale with labels
#        main="Test - closed/open"
# )


ShowChart<-function(MAP=12,xrange,xstep=1,IBase=NULL,IndexDt,ClaimV,
                    ExposureV,cohort="All",ExposureDesc="Earned Fees") {
  
  # revised 4Dec2012
  # added option to just show frequency per $m by not passing IBase
  
  # Smoothed with lag:
  # average of current month and n-1 months
  
  #added 1st October 2013 (also in PRPFunctions)
  
  fn <- rep(1/MAP, MAP)
  
  #if indexed use IBase
  #claims
  y1_lag <- filter(ClaimV, fn, sides=1)
  
  #fees
  y2_lag <- filter(ExposureV, fn, sides=1)
  
  #freq
  y3<-y1_lag/y2_lag
  
  if(!is.null(IBase)) {
    y1<-y1_lag/y1_lag[IBase]*100
    y2<-y2_lag/y2_lag[IBase]*100
    y3<-y3/y3[IBase]*100
    ylab<-paste("Index, 100 at ",IndexDt)
    ym<-max(y1[MAP:length(xrange)],y2[MAP:length(xrange)],y3[MAP:length(xrange)])
    y<-y1[MAP:length(xrange)]
    col<-"blue"
    lty<-2
    lwd<-1
  }else{
    ylab<-"Freq/ million prem"
    ym<-max(y3[MAP:length(xrange)]*1e6)
    y<-y3[MAP:length(xrange)]*1e6
    col<-"red"
    lty<-1
    lwd<-2
  }
  
  
  
  
  #need to place x,y pairs one month later to corresponding to completed months
  #also don't plot NAs
  x<-xrange[MAP:length(xrange)]
  
  plot(y,col=col,type="l",lty=lty,lwd=lwd,xaxt="n",ylim=c(0,ym),
       main=paste(cohort," with Exposure on ",ExposureDesc, " , total claims= ",sum(ClaimV)),
       xlab="Date",ylab=ylab,cex.main=.6)
  
  xvts<-seq(from=1,to=length(x),by=xstep)
  #add back an x-axis with dates, las and cex.axis set direction and size of dates
  axis(1, at=xvts,x[xvts],las=2,cex.axis=.5)  #adjust to every xstep
  
  # Draw gridlines
  grid()
  
  if(!is.null(IBase)) {
    #add lines
    y<-y2[MAP:length(xrange)]
    lines(y, col="black",type="l",lty=3)
    
    y<-y3[MAP:length(xrange)]
    lines(y, col="red",type="l",lty=1,lwd=2)
    
    # add a legend, lwd   sets line width, you can use x,y coordinates instead of "bottomleft"
    legend("topleft", 
           c("12 month rolling average of claim numbers",
             paste("12 month rolling average of ",ExposureDesc),
             paste("12 month rolling average of claim/",ExposureDesc)), 
           col = c("blue","black", "red"), 
           cex=.5,lwd = 1,lty=c(2,3,1), title="key") 
    
  }
}