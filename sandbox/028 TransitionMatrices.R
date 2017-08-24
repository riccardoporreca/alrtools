ptm<-proc.time()
whole.matrix<-matrix(rep(0,8*8*40),nrow=8,ncol=8*40)
n=length(dev)
for(j in 1:(max(dev)-1)){
  TM<-matrix(rep(0,64),nrow=8,ncol=8)
for(i in 1:(n-1))
{
  if(dev[i]==j&dev[i+1]==j+1){
  for(h in 1:4){
    for(k in 1:2){
      for(z in 1:4){
        for(m in 1:2){
          if(ClaimStatus[i]==k&ClaimSeverityInd[i]==h&ClaimStatus[i+1]==m&ClaimSeverityInd[i+1]==z)
            TM[(k-1)*4+h,(m-1)*4+z]=TM[(k-1)*4+h,(m-1)*4+z]+1
        }
      }
      
    }
  }   
  }  
}
  whole.matrix[,((j-1)*8+1):(8*j)]<-TM/sum(TM)
}

proct.time()-ptm
print(ptm)