
#Clear all objects
rm(list=ls())

# Load R file for functions
source("functions.R")

# Cost method vector
cm<-c('EAN','PUC')

#entry age vector
ea<-seq(25,35,5)

#retirement age vector
retire<-seq(55,65,5)

#salary growth ratevector
a_sgr<-sgr<-seq(0.03,0.06,0.01)

#discount rate vector
i<-seq(0.03,0.09,0.01)

#current age vector
ca<-seq(25,85,10)

#Mortality table numbers
mort<-c(2,4,6) 

#starting salary
sal<-1

#Benefit factor vector
bf<-seq(0.02,0.06,0.01)

#afc vector
afc<-c(1,5,10)

#cola vector
cola<-seq(0,0.04,0.01)

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

dat<-'NA'


mort_name <- function(mort_number){
  m<-switch(mort_number,'2'="RP2014_Employee_total",'4'="RP2000_Employee_total",'6'="RP2010_Employee_total")
  return(m)
  }


suppressWarnings(
for(a1 in ea){
  for(l1 in ca){  
    for(k1 in retire){
      for(b1 in i){
        for(c1 in a_sgr){
          for(e1 in cola){
            for(f1 in afc){
              for(g1 in bf){
                for(h1 in cm){
                  for(j1 in mort){
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


names(dat)<-c('Run No.','Current Normal Cost','Current Salary','Current AAL','Current ARC','Expected value of annuity','Retirement Annuity','Replacement Rate',
              'Entry Age','Retirement Age','Current Age','Funding Level(%)','Discount Rate(%)','Salary growth rate(%)','Inflation Rate(%)','Payroll Growth Rate(%)',
              'COLA (%)','AFC','Benefit Factor(%)','Cost Method','Mortality Table','Vesting Period','Amortization Period')

  
save(dat,file='Simulate.RData')
write.csv2(dat,file='Simulate.csv')
View("dat")
