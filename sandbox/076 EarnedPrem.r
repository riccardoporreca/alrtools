EarnedPrem <- function (Exp,start="2006-01-01",end="2009-05-01",RO=FALSE,ROPd=1.5,GP="Y",Detail=FALSE,AnnualMeasure=FALSE)  {  

#Exposure is 3 columns of exposure date assumed to be start date, end date, written prem or annualised exposure measure
#RO for run-off: use true
#Data Entry really uses UK 1Jan-1Jan policy period for full year
#GP is for grouping output by Year or Month

   # convert to numeric if a factor         

    sd <- Exp[,1]
                 if (is.factor(sd)) {
                                 tmpf <- sd
                                 sd <- as.numeric(levels(tmpf))[tmpf]
                 }

                # force date format
                 sd <- as.Date(sd, origin="1899-12-30")

    ed <- Exp[,2]
                  if (is.factor(ed)) {
                                 tmpf <- ed
                                 ed <- as.numeric(levels(tmpf))[tmpf]
                 }

                # force date format
                 ed <- as.Date(ed, origin="1899-12-30")
    
    
  #correct for one day problem caused by incorrect reading from Excel
  sd<-sd+1
  ed<-ed+1 
  
  #old method - replaced by above check on factors
  #  sd<-as.Date(Exp[,1])
  #  ed<-as.Date(Exp[,2])
    pr<-Exp[,3]
    
    start<-as.Date(start)
    end<-as.Date(end)
    GP<-toupper(GP)
    rec<-length(sd)    #records
    
    n<-as.numeric(difftime(end,start,units="days"))
    term<-as.numeric(difftime(ed,sd,units="days"))
    
    d<-as.numeric(difftime(sd,start,units="days"))
    n1<-(abs(d)+d)/2            #positive values only, else 0
    n1[n1>n]<-n                 #cap n1 to n
    
    e<-as.numeric(difftime(end,ed,units="days"))
    n2<-n-(abs(e)+e)/2   
    n2[n2<0]<-0             
    
    ep<-rep(0,n)
    dt<-start+seq(from=0, to=n-1, by=1)
    
    if (Detail==FALSE) {
    
      for (j in 1:rec)   {
	  if (AnnualMeasure==FALSE) {z=1} else {z=term[j]/365}
 
       if (n2[j]>n1[j])   {                   #only use policies with exposure
          if (!(RO==TRUE & term[j]>365*ROPd))  {
            ep[(n1[j]+1):n2[j]]<-ep[(n1[j]+1):n2[j]]+ pr[j]/term[j]*z
          }
          else  {
            for (i in (n1[j]+1):n2[j]) {
                  ep[i]<-ep[i]+2*pr[j]/term[j]*(1-(i-d[j])/(1+term[j]))*z
            }
          }   
        }              
      }
      
  
      if (GP=="Y")    {epf<-tapply(ep,format(dt,"%Y"),sum)}
      if (GP=="M")    {epf<-tapply(ep,format(dt,"%Y%m"),sum)}
      
     	return(epf)
	    
    }
    
    else   {
    
        for (j in 1:rec)   {
	  if (AnnualMeasure==FALSE) {z=1} else {z=term[j]/365}

        ep<-rep(0,n)

        if (n2[j]>n1[j])   {                   #only use policies with exposure
          if (!(RO==TRUE & term[j]>365*ROPd))  {
            ep[(n1[j]+1):n2[j]]<-pr[j]/term[j] *z
          }
          else  {
            for (i in (n1[j]+1):n2[j]) {
                  ep[i]<-2*pr[j]/term[j]*(1-(i-d[j])/(1+term[j]))*z
            }
          }   
        }
         
        if (GP=="Y")    {epd<-tapply(ep,format(dt,"%Y"),sum)}
        if (GP=="M")    {epd<-tapply(ep,format(dt,"%Y%m"),sum)}   
   
        
        
        if (j==1) {epf<-epd}
        else {epf<-rbind(epf,epd)}
 
                      
      }
        
      return(epf)
    
    }
    
 }
