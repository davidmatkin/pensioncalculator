#########################################################################################################
#########################################################################################################
#                                                                                                       #
#                                          DESCRIPTION                                                  #
#   sample_run.R  creates a dataframe and xlsx sheet of 100 observations of ARC, Normal Cost, AAL,      #
#   Expected value of annuity, Retirement Annuity, Replacement Rate for an random combination of        #
#   Entry Age, Retirement Age, Current Age, Funding Level, Discount Rate, Salary growth rate, Payroll   #
#   Growth Rate(%), COLA, AFC, Benefit Factor, Cost Method, Mortality Table,Amortization Period         #
#   in the current working directory                                                                    #
#                                                                                                       #
#########################################################################################################
#########################################################################################################



############################ LIBRARY IMPORT ##############################
# foreach: R library provide looping construct for collection of values
# REFERENCE: https://cran.r-project.org/web/packages/foreach/vignettes/foreach.pdf
##########################################################################

library(foreach)

############################ LIBRARY IMPORT ##############################
# xlsx Provides R functions to read/write/format Excel 2007 and Excel 97/2000/XP/2003 file formats
# REFERENCE: https://cran.r-project.org/web/packages/xlsx/xlsx.pdf
##########################################################################

library(xlsx)

############################ LIBRARY IMPORT ##############################
# doParallel: Provides a parallel backend for the %dopar% function using
# the in-built parallel packages
# REFERENCE: https://cran.r-project.org/web/packages/doParallel/doParallel.pdf
##########################################################################

library(doParallel)

############################# R COMMAND ##################################
# rm() removes a variable from the workspace. A list of all the variables 
# in the workspace is passed as a parameter to rm to clear all variables 
# in the workspace to ensure that the model uses only the updated values
# sent by the user for computations
##########################################################################

rm(list=ls())


### Configure program to use 4 cores ###
 # cl<-makeCluster(4)  # Making a cluster of cores
 # registerDoParallel(cl) # registering cluster

############################# R COMMAND ##################################
# source() notifies R that the variables and functions present in the
# file passed as an argument to it should be loaded to the r environment   
# and is avaiable for use in this file
##########################################################################
# we source our model logic present in functions.R

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

#Max age limit of the model
max_age<-100

# An empty dataframe. Will be used later to store results
dat_sample<-NULL

# A mortality table code conversion function.
mort_name <- function(mort_number){
    m<-switch(mort_number,'2'="RP2014_Employee_total",'4'="RP2000_Employee_total",'6'="RP2010_Employee_total")
  return(m)
}

# Setting number od iteration for each core
# iter_number=10

# Printing start time for the sample data generation
print(paste0("Start Time ",Sys.Date()," ",Sys.time()))

 #foreach(icount(iter_number)) %dopar% {
  for(i in seq(1,100)){
      a1<-sample(e_a,1) # random sample value of entry age
      l1<-sample(subset(ca,ca>a1),1)   # random sample value of current age greater than or equal to entry age
      k1<-sample(r_a,1) # random sample value of retirement age
      b1<-sample(i_r,1) # random sample value of interest rate
      c1<-sample(act_sgr,1) # random sample value of salary growth rate
      e1<-sample(cola_r,1) # random sample value of COLA
      f1<-sample(afc_p,1) # random sample value of AFC
      g1<-sample(bf_p,1) # random sample value of Benefit factor
      h1<-sample(c_m,1) # random sample value of Cost Method ('EAN','PUC')
      j1<-sample(mort_num,1) # random sample value of mortality table number
      nc<-c(get_NC(a1,k1,b1,c1,c1,e1,f1,g1,h1,j1,vesting),rep(0,(max_age-retire)))[l1-a1+1]  # nc value for random actuarial inputs
      act_sal<-c(get_act_sal(a1,k1,c1),rep(0,(max_age-retire)))[l1-a1+1] # actual salary for random actuarial inputs
      aal<-get_AAL(a1,k1,b1,c1,c1,e1,f1,g1,h1,j1,vesting)[l1-a1+1] # AAL for random actuarial inputs
      adc<-get_ARC(rep(1,(100-a1)+1),a1,k1,median_p,b1,c1,b1,c1,c1,c1-inflation,e1,f1,g1,h1,j1,vesting,amortization)[l1-a1+1] # ARC for random actuarial inputs
      rpvfbx<-get_rPVFBx_after_r(a1,k1,b1,c1,c1,e1,f1,g1,j1)[2] # Expected value of annuity for random actuarial inputs
      ret_annuity<-get_retirement_annuity(a1,k1,c1,c1,f1,g1) # Retirement Annuity for random actuarial inputs
      repl_rate<-get_replacement_rate(a1,k1,c1,c1,f1,g1) # Replacement rate for random actuarial inputs
      dat_sample<-rbind(dat_sample,data.frame(run,nc,act_sal,aal,adc,rpvfbx,ret_annuity,repl_rate,
                    a1,k1,l1,median_p,b1,c1,inflation,c1-inflation,e1,f1,
                    g1,h1,mort_name(as.character(j1)),vesting,amortization)) # generating dataframe
      
      run=run+1 # incrementing run value
      
      }
 #}
# print end tim for the sample run
print(paste0("END Time ",Sys.Date()," ",Sys.time()))


names(dat_sample)<-c('Run No.','Current Normal Cost','Current Salary','Current AAL','Current ARC','Expected value of annuity','Retirement Annuity','Replacement Rate',
             'Entry Age','Retirement Age','Current Age','Funding Level(%)','Discount Rate(%)','Salary growth rate(%)','Inflation Rate(%)','Payroll Growth Rate(%)',
             'COLA (%)','AFC','Benefit Factor(%)','Cost Method','Mortality Table','Vesting Period','Amortization Period')



save(dat_sample,file='Simulate_sample.RData')  # saving the Rdata file

write.xlsx(dat_sample,file='Simulate_sample2.xls') # writing the dataframe to an excel file

