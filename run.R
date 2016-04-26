#Importing libraries
library(foreach)
library(doParallel)

#Clear all objects
rm(list=ls())


#set program to use 4 cores
cl<-makeCluster(4)
registerDoParallel(cl)


# Load R file for functions
source("functions.R")

# Cost method vector
c_m<-c('EAN','PUC')

#entry age vector
e_a<-seq(25,35,5)

#retirement age vector
retire_age<-seq(55,65,5)

#salary growth ratevector
a_sgr_v<-sgr_v<-seq(0.03,0.06,0.01)

#discount rate vector
i_v<-seq(0.03,0.09,0.01)

#current age vector
c_a<-seq(25,85,10)

#Mortality table numbers
mort_num<-c(2,4,6) 

#starting salary
sal<-1

#Benefit factor vector
b_f<-seq(0.02,0.06,0.01)

#afc vector
afc_P<-c(1,5,10)

#cola vector
cola_p<-seq(0,0.04,0.01)

#inflation rate
inflation_r<-0.02

#vesting period
vesting_p<-5

#run number
run<-0

#funding level
median_per<-0.854

#amortization period
amortization_p<-30

val<-0


#Max age limit of the model
max_age<-100

dat<-'NA'

iteration_num<-length(c_m)*length(e_a)*length(retire_age)*length(c_a)*length(sgr_v)*length(i_v)*length(mort_num)*length(b_f)*length(afc_P)*length(cola_p)

mort_name <- function(mort_number){
  m<-switch(mort_number,'2'="RP2014_Employee_total",'4'="RP2000_Employee_total",'6'="RP2010_Employee_total")
  return(m)
  }

foreach(icount(iteration_num)) %dopar% {
  
suppressWarnings(
for(a1 in e_a){
  for(l1 in c_a){  
    for(k1 in retire_age){
      for(b1 in i_v){
        for(c1 in a_sgr_v){
          for(e1 in cola_p){
            for(f1 in afc_P){
              for(g1 in b_f){
                for(h1 in c_m){
                  for(j1 in mort_num){
                    nc<-c(get_NC(a1,k1,b1,c1,c1,e1,f1,g1,h1,j1,vesting),rep(0,(max_age-retire)))[l1-a1+1]
                    act_sal<-c(get_act_sal(a1,k1,c1),rep(0,(max_age-retire)))[l1-a1+1]
                    aal<-get_AAL(a1,k1,b1,c1,c1,e1,f1,g1,h1,j1,vesting)[l1-a1+1]
                    adc<-get_ARC(rep(1,(100-a1)+1),a1,k1,median_p,b1,c1,b1,c1,c1,c1-inflation,e1,f1,g1,h1,j1,vesting,amortization)[l1-a1+1]
                    rpvfbx<-get_rPVFBx_after_r(a1,k1,b1,c1,c1,e1,f1,g1,j1)[2]
                    ret_annuity<-get_retirement_annuity(a1,k1,c1,c1,f1,g1)
                    repl_rate<-get_replacement_rate(a1,k1,c1,c1,f1,g1)
                    
                    dat<-rbind(dat,
                        data.frame(run,nc,act_sal,aal,adc,rpvfbx,ret_annuity,repl_rate,
                                   a1,k1,l1,median_p,b1,c1,inflation,c1-inflation,e1,f1,
                                   g1,h1,mort_name(as.character(j1)),vesting,amortization))
  
                    run=run+1
                    print(paste0("Run no:",run," ","val no:",val," ",Sys.Date()," ",Sys.time()))
  }}}}}}}}}})

}
names(dat)<-c('Run No.','Current Normal Cost','Current Salary','Current AAL','Current ARC','Expected value of annuity','Retirement Annuity','Replacement Rate',
              'Entry Age','Retirement Age','Current Age','Funding Level(%)','Discount Rate(%)','Salary growth rate(%)','Inflation Rate(%)','Payroll Growth Rate(%)',
              'COLA (%)','AFC','Benefit Factor(%)','Cost Method','Mortality Table','Vesting Period','Amortization Period')

  
save(dat,file='Simulate.RData')
write.csv2(dat,file='Simulate.csv')
load("Simulate.RData")

