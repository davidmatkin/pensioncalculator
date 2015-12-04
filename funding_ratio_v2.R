
rm(list=ls())
source("E://SUNYALBANY/Pension_Simulation_Project/Code/functions.R")

###################### Load winklevoss and mortality tables #################################
load('E://SUNYALBANY/Pension_Simulation_Project/Code/Data/winklevossdata.RData')
load('E://SUNYALBANY/Pension_Simulation_Project/Code/Data/Mortality.RData')

######################### Parameters constant but likely to change ##########################
#userInput<-function(){
  salary_growth_r<<-0.0501
  benefit_factor<<-0.02
  interest_r<<-0.0825
  cola_p<<-0.0
  ea<<-30
  asgr<<-0.05
  #### asset conditions #############
  median_cond<<-"df"
  median_percent<<-0.854
  median_value<<-0.08
    
  
  ###################################
  
  cost_m<<-"EAN"
  mort<<-7
  
  
  amort_p<<-30
  vesting_p<<-5
  #}

  age_information(ea)

#get_act_sal(a_sgr=asgr)
#get_acc_sal(a_sgr=asgr)
#get_exp_sal(sgr=salary_growth_r)
#get_exp_sal_r(a_sgr=asgr,sgr=salary_growth_r)
#get_xpmx(mort=7)
#get_rpmx(mort=7)
#get_xpmr(mort=7)
#get_acc_benefits(a_sgr=asgr,bf=benefit_factor)
#get_acc_benefit_r(a_sgr=asgr,sgr=salary_growth_r,bf=benefit_factor)
#get_acc_benefit_65(a_sgr=asgr,sgr=salary_growth_r,bf=benefit_factor)
#get_vrx(i=interest_r)
#get_vxr(i=interest_r)
#get_qxt()
#get_xpxT(mort)
#get_npxT(mort=7)
#get_rxpxT(mort)
#get_ar(i=interest_r,cola=cola_p,mort=7)
#get_am(i=interest_r,amortization=amort_p)
#get_an(i=interest_r,cola=cola_p,mort=7,amortization=amort_p)
#get_tla(i=interest_r,sgr=salary_growth_r,mort=7,a=31)
#get_tla_t(i=interest_r,sgr=salary_growth_r,mort=7)
#get_rPVFBx(i=interest_r,a_sgr=asgr,sgr=salary_growth_r,cola=cola_p,bf=benefit_factor,mort=7)
#get_gxv(vesting=vesting_p)
#get_vesting_cost(i=interest_r,a_sgr=asgr, cola=cola_p,bf=benefit_factor,mort=7,vesting=vesting_p)
#get_term_cost(i=interest_r,a_sgr=asgr, cola=cola_p,bf=benefit_factor,mort=7,vesting=vesting_p)
#get_PVTC_t(i=interest_r,a_sgr=asgr, cola=cola_p,bf=benefit_factor,mort=7,vesting=vesting_p)
#get_NC(i=interest_r,a_sgr=asgr,sgr=salary_growth_r,cola=cola_p,bf=benefit_factor,cm=cost_m,mort=7,vesting=vesting_p)
#get_AAL(i=interest_r,a_sgr=asgr,sgr=salary_growth_r,cola=cola_p,bf=benefit_factor,cm=cost_m,mort=7,vesting=vesting_p)
get_median_asset(median_c=median_cond,median_p=median_percent,median_v=median_value,i=interest_r,a_sgr=asgr,sgr=salary_growth_r,cola=cola_p,bf=benefit_factor,cm=cost_m,mort=7,vesting=vesting_p)
  
#get_median_asset(median_c=median_cond,median_p=median_percent,median_v=median_value,i=interest_r,a_sgr=asgr,sgr=salary_growth_r,cola=cola_p,bf=benefit_factor,cm=cost_m,mort=7,vesting=vesting_p)
get_FR(median_c=median_cond,median_p=median_percent,median_v=median_value,i=interest_r,a_sgr=asgr,sgr=salary_growth_r,cola=cola_p,bf=benefit_factor,cm=cost_m,mort=7,vesting=vesting_p)
#get_ARC(median_c=median_cond,median_p=median_percent,median_v=median_value,i=interest_r,a_sgr=asgr,sgr=salary_growth_r,cola=cola_p,bf=benefit_factor,cm=cost_m,mort=7,vesting=vesting_p,amortization=amort_p)
#plot_fr(median_c=median_cond,median_p=median_percent,median_v=median_value,i=interest_r,a_sgr=asgr,sgr=salary_growth_r,cola=cola_p,bf=benefit_factor,cm=cost_m,mort=7,vesting=vesting_p)

