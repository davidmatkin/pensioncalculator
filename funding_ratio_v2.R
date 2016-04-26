

library(BayesBridge)
rm(list=ls())

source("functions.R")
###################### Load winklevoss and mortality tables #################################
load('E://SUNYALBANY/Pension_Simulation_Project/Code/Data/winklevossdata.RData')


load('E://SUNYALBANY/Pension_Simulation_Project/Code/Data/Mortality.RData')
######################### Parameters constant but likely to change ##########################


ea<-30

a_sgr<-0.05
sal=60000
sgr<-0.0568
mort<-2
afc<-5
bf<-0.02
i<-0.08
cola<-0.01
amortization<-30
vesting<-5
cm<-'EAN'
median_p<-0.854



median_dr<-0.08
median_sgr<-0.0568

pop<-rep(1,71)



afc<-5
retire<-65



get_qxt(ea,retire)

pop_type='Uniform'


pop_size=100000



ca=37
median=45



inflation=2




pop<-rep(1,71)
active<-seq(30,65)

pgr<-sgr-inflation


retirees<-seq(66,100)
ea<-30
sum(get_ARC(pop,ea,retire,median_p,median_dr,median_sgr,i,a_sgr,sgr,pgr,cola,afc,bf,cm,mort,vesting,amortization))

get_FR(pop,ea,retire,median_p,median_dr,median_sgr,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
get_median_asset(ea,retire,median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting)
get_nc_pop(pop,ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
get_NC(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
active<-rep(1,36)
retirees<-rep(1,71)
pgr<-3.68

get_stat(ea,retire,active,retirees,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)

get_aal_pop(pop,ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)

get_AAL(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)

get_PVTC_t(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort,vesting)
get_PVTC(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort,vesting,a)

get_term_cost(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort,vesting)

get_vesting_cost(ea,retire,i,a_sgr,cola,afc,bf,mort,vesting)
get_gxv(ea,retire,vesting)
get_xPVFBr(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort)
get_rPVFBx(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort)


get_a_after_r(ea,retire,i,cola,mort)

get_tla_t(ea,retire,i,sgr,mort)

get_tla(ea,retire,i,sgr,mort,a)
get_am(ea,retire,i,amortization)
get_ar(ea,retire,i,cola,mort)



get_replacement_rate(ea,retire,a_sgr,sgr,afc,bf)
get_sal_ca(ea,retire,ca,sal,inflation,sgr)
get_sal_vector_ea(ea,retire,sal,inflation,sgr)

get_rxpxT(ea,retire,mort)
get_xpxT(ea,retire,mort)
get_vxr(ea,retire,i)
get_vrx(ea,retire,i)
get_acc_benefit_65(ea,retire,a_sgr,sgr,afc,bf)
afc<-10

get_acc_benefit_r(ea,retire,a_sgr,sgr,afc,bf)
get_acc_benefits(ea,retire,a_sgr,afc,bf)
get_xpmr(ea,retire,mort)
get_rpmx(ea,retire,mort)
get_xpmx(ea,retire,mort)


get_mort(ea,retire,mort) 
get_exp_sal_r(ea,retire,a_sgr,sgr)
get_acc_sal(ea,retire,a_sgr)
get_exp_sal(ea,retire,sgr) 
get_act_sal(ea,retire,a_sgr)


3.5105/5.516





get_a_after_r(30,65,0.08,0.02,2)

get_act_sal(30,65,0.056)[length(get_act_sal(30,65,0.056))]

population_retirees(ea,retire,mort,population)
population=generate_pop(ea,retire,pop_type,pop_size,median)
age_grp_labels(ea,retire,age_limit)
age_limit=70
age_information(ea,retire)

