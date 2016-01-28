

get_tla<-function(i,a_sgr,sgr,mort,a)
{
  a_sgr=0.05
  i=0.08
  mort=7
  a=30
  ea=30
  age<-age[1:(length(age)-1)]
  sgr<-0.0501
  
  np<-numeric(length(age))
  v<-numeric(length(age))
  sts<-numeric(length(age))
  
  xpmx<-get_xpmx(mort=7)[1:length(age)]
  if (a>=ea){
    np<-v<-sts<-c(rep.int(1,(a-ea+1)))
    
    for (j in (a-ea+1):(length(age)-(a-ea))){
      np[j+1]=xpmx[j]*np[j]
      v[j+1]=v[j]/(1+i)
      sts[j+1]=sts[j]*(1+sgr)
    }
  }
  tla<-sum((np*v*sts)[1:length(age)])
  
  return(tla)
}
