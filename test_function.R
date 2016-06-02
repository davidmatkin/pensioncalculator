#########################################################################################################
#########################################################################################################
#                                                                                                       #
#                                          DESCRIPTION                                                  #
#   This file allows you to enter Values for desired inputs and check vectors and values for            #
#   all functions in functions.R                                                                        #
#                                                                                                       #
#########################################################################################################
#########################################################################################################

############################# R COMMAND ##################################
# rm() removes a variable from the workspace. A list of all the variables 
# in the workspace is passed as a parameter to rm to clear all variables 
# in the workspace to ensure that the model uses only the updated values
# sent by the user for computations
##########################################################################
rm(list=ls())

############################# R COMMAND ##################################
# source() notifies R that the variables and functions present in the
# file passed as an argument to it should be loaded to the r environment   
# and is avaiable for use in this file
##########################################################################
# we source our model logic present in functions.R
source("functions.R")

# Enter value for entry age. Model's range for entry age is between 25-45
ea<-30

# Enter value for retirement age. Model's range for entry age is between 55-65
retire<-65

# Enter value for current age.
ca<-65

# Enter value for actual salary growth rate.
a_sgr<-0.05

# Enter expected salary growth rate. Our model can function with different actual and expected salary growth rates.
# We have set the modelto have same actual and expected salary growth rates
sgr<-0.0568

# Enter salary at entry age
sal<-60000

# Enter mortality table code. Codes: 2- RP2014 Total, 4- RP2000 Total, 6-RP2010 Total
mort<-2

# Enter AFC period
afc<-5

# Enter benefit factor
bf<-0.02

# Enter inflation rate. Model is set to constant inflation of 2%
inflation<-0.02

# Enter discoount rate
i<-0.08

# Enter payroll growth rate
pgr<-0.0368

# Enter COLA
cola<-0.01

# Enter amortization period
amortization<-30

# Enter vesting period
vesting<-5

# Enter cost method
cm<-'EAN'

# Enter funding level
median_p<-0.854

# Enter discount rate for calculating assets. Models sets this to the discount rate mentioned earlier
median_dr<-0.08

# Enter salary growth rate for calculating assets. Models sets this to the salary growth rate mentioned earlier
median_sgr<-0.0568

# creating an uniform population
pop<-rep(1,71)

# Set population type
pop_type<-'Uniform'

# Set Population size
pop_size<-100000

# set median age
median<-45

#set age limit
age_limit<-100

# generate active members population Uniform distribution
active<-seq(30,65)

# generate retirees population Uniform distribution
retirees<-seq(66,100)

# get ARC
sum(get_ARC(pop,ea,retire,median_p,median_dr,median_sgr,i,a_sgr,sgr,pgr,cola,afc,bf,cm,mort,vesting,amortization))

# get funding ratio
get_FR(pop,ea,retire,median_p,median_dr,median_sgr,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)

#get median asset
sum(get_median_asset(ea,retire,median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting))

# get normal cost vector
get_NC(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)

# get AAL vector
get_AAL(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)

# get statistics for age groups
get_stat(ea,retire,active,retirees,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)

# get PVTC vector
get_PVTC_t(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort,vesting)

# get termination cost vector
get_term_cost(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort,vesting)

# get vesting cost vector
get_vesting_cost(ea,retire,i,a_sgr,cola,afc,bf,mort,vesting)

# grading function vector
get_gxv(ea,retire,vesting)

# get RPVFBx vector
get_rPVFBx(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort)

# get annuity after retirement
get_a_after_r(ea,retire,i,cola,mort)

# get temporary annuity
get_tla_t(ea,retire,i,sgr,mort)

# get period certain annuity
get_am(ea,retire,i,amortization)

# get straight life annuity
get_ar(ea,retire,i,cola,mort)

# get replacemnt rate
get_replacement_rate(ea,retire,a_sgr,sgr,afc,bf)

# get Probablity to continue to retirement
get_rxpxT(ea,retire,mort)

# get Probablity to continue to next year
get_xpxT(ea,retire,mort)

# get discount factor from x to 100
get_vxr(ea,retire,i)

# get discount factor from x to r
get_vrx(ea,retire,i)

# get accumulated benefits at 65
get_acc_benefit_65(ea,retire,a_sgr,sgr,afc,bf)

# get accumulated benefits at 65 vector
get_acc_benefit_r(ea,retire,a_sgr,sgr,afc,bf)

# get accumulated benefits
get_acc_benefits(ea,retire,a_sgr,afc,bf)

# get Survival probablity from retirement age to certain age 
get_xpmr(ea,retire,mort)

# get Survival probablity from x to retirement age
get_rpmx(ea,retire,mort)

# get Survival probablity from x to x+1
get_xpmx(ea,retire,mort)

# get mortality table
get_mort(ea,retire,mort) 

# get expected salary at retirement vector
get_exp_sal_r(ea,retire,a_sgr,sgr)

# get accumulated salary
get_acc_sal(ea,retire,a_sgr)

# get expected salary
get_exp_sal(ea,retire,sgr) 

# get actual salary
get_act_sal(ea,retire,a_sgr)

# get population of retirees
population_retirees(ea,retire,mort,population)

# get active members population
generate_pop(ea,retire,pop_type,pop_size,median)

# get age group labels
age_grp_labels(ea,retire,age_limit)

# set age information
age_information(ea,retire)

