
#Importing libraries
library(foreach)

library(doParallel)

#Clear all objects
rm(list=ls())


#set program to use 4 cores
 #cl<-makeCluster(3)
 #registerDoParallel(cl)

# Load R file for functions
source("functions.R")

# Cost method vector
c_m<-c('EAN','PUC')

#entry age vector
e_a<-c(25,30,35)

#retirement age vector
r_a<-seq(55,65,5)

#salary growth ratevector
act_sgr<-sgr<-seq(0.03,0.06,0.01)

#discount rate vector
i_r<-seq(0.03,0.09,0.01)


#current age vector
ca<-seq(25,85,10)

#Mortality table numbers
mort_num<-c(2,4,6) 

#starting salary
sal<-1

#Benefit factor vector
bf_p<-seq(0.02,0.06,0.01)




#afc vector
afc_p<-c(1,5,10)

#cola vector
cola_r<-seq(0,0.04,0.01)

#inflation rate
inflation<-0.02

#vesting period
vesting<-5

#run number
run<-0

#funding level
median_p<-0.854

#amortization period

amortization<-30

val<-0

#Max age limit of the model
max_age<-100

dat_sample<-NULL


mort_name <- function(mort_number){
  
  
  
  m<-switch(mort_number,'2'="RP2014_Employee_total",'4'="RP2000_Employee_total",'6'="RP2010_Employee_total")
  return(m)
}

iter_number=10

print(paste0("Start Time ",Sys.Date()," ",Sys.time()))

 #foreach(icount(iter_number)) %dopar% {
  for(i in seq(1,100)){
      l1<-sample(ca,1)  
      a1<-sample(e_a,1)
      k1<-sample(r_a,1)
      b1<-sample(i_r,1)
      c1<-sample(act_sgr,1)
      e1<-sample(cola_r,1)
      f1<-sample(afc_p,1)
      g1<-sample(bf_p,1)
      h1<-sample(c_m,1)
      j1<-sample(mort_num,1)
      #print(paste0("ea",a1," ca",l1," ra",k1))      
      nc<-c(get_NC(a1,k1,b1,c1,c1,e1,f1,g1,h1,j1,vesting),rep(0,(max_age-retire)))[l1-a1+1]
      act_sal<-c(get_act_sal(a1,k1,c1),rep(0,(max_age-retire)))[l1-a1+1]
      aal<-get_AAL(a1,k1,b1,c1,c1,e1,f1,g1,h1,j1,vesting)[l1-a1+1]
      adc<-get_ARC(rep(1,(100-a1)+1),a1,k1,median_p,b1,c1,b1,c1,c1,c1-inflation,e1,f1,g1,h1,j1,vesting,amortization)[l1-a1+1]
      rpvfbx<-get_rPVFBx_after_r(a1,k1,b1,c1,c1,e1,f1,g1,j1)[2]
      ret_annuity<-get_retirement_annuity(a1,k1,c1,c1,f1,g1)
      repl_rate<-get_replacement_rate(a1,k1,c1,c1,f1,g1)
      dat_sample<-rbind(dat_sample,data.frame(run,nc,act_sal,aal,adc,rpvfbx,ret_annuity,repl_rate,
                    a1,k1,l1,median_p,b1,c1,inflation,c1-inflation,e1,f1,
                    g1,h1,mort_name(as.character(j1)),vesting,amortization))
      
      run=run+1
      
  
      }
 #}
print(paste0("END Time ",Sys.Date()," ",Sys.time()))


names(dat_sample)<-c('Run No.','Current Normal Cost','Current Salary','Current AAL','Current ARC','Expected value of annuity','Retirement Annuity','Replacement Rate',
             'Entry Age','Retirement Age','Current Age','Funding Level(%)','Discount Rate(%)','Salary growth rate(%)','Inflation Rate(%)','Payroll Growth Rate(%)',
             'COLA (%)','AFC','Benefit Factor(%)','Cost Method','Mortality Table','Vesting Period','Amortization Period')



save(dat_sample,file='Simulate_sample.RData')
write.csv2(dat_sample,file='Simulate_sample.csv')

