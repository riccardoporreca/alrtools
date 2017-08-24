sink("modifieddata")
Claims<-read.csv("C:/Users/Qiao/Desktop/Claims for R.csv") #read the data
attach(Claims) 
NewId<-as.integer(Claims$ClaimId) #change Claims'ID into number for easy tracking
ClaimId<-NewId #replace ClaimId with number
fordivider1<-table(ClaimId)
fordivider2<-data.frame(fordivider1)
divider<-fordivider2[,2] # it containes the number of records for each claim. It can help us separate different claims.
number.Claims<-max(ClaimId) #total number of different claims
ClaimStatus<-as.integer(ClaimStatus)
for(i in 2:number.Claims)
{
  divider[i]=divider[i]+divider[i-1]
} # to ease function append

dev<-Claims$dev # next three lines are only for test
ClaimStatus<-Claims$ClaimStatus
ClaimSeverityInd<-as.integer(Claims$ClaimSeverityInd)

insert.amount<-dev[1]-1
if(dev[1]!=1){
  ClaimSeverityInd<-append(ClaimSeverityInd,rep(4,dev[1]-1),0)
  ClaimStatus<-append(ClaimStatus,rep(2,dev[1]-1),0)
  dev<-append(dev,c(1:(dev[1]-1)),0) 
}
ptm<-proc.time()
for(i in 2:(number.Claims-1))
{
  if(dev[1+divider[i]+insert.amount]!=1)
    {
    c=insert.amount
    insert.amount=insert.amount+dev[1+divider[i]+c]-1
    ClaimSeverityInd<-append(ClaimSeverityInd,rep(4,dev[1+divider[i]+c]-1),divider[i]+c)
    ClaimStatus<-append(ClaimStatus,rep(2,dev[1+divider[i]+c]-1),divider[i]+c)
    dev<-append(dev,c(1:(dev[1+divider[i]+c]-1)),divider[i]+c)
  }  
    
}
proc.time<-ptm
x<-data.frame(dev,ClaimSeverityInd,ClaimStatus)
print(x)
sink()

number.quarters<-table(dev)
plot(1:40,number.quarters,col="blue")

