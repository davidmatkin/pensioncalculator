#####################################################################################################
  ################################################################################################
     ##################### Basic functions for Pension Simulation Calculator#################
  ################################################################################################
######################################################################################################
library(scales)
rm(list = ls())
dollar_format(prefix = "", suffix = "$",largest_with_cents = 1e-2, big.mark = ",", negative_parens = FALSE)
  

#################################################################################################
########################################## Load RData ###########################################
#################################################################################################
#load('winklevossdata.RData')
load('Mortality.RData')
load('Termination.RData')

#################################################################################################
############################## Age information for simulation ###################################
#################################################################################################

############## Setter method for age information ###############
 age_information <<- function(i_ea,retire) {
  ea <<- i_ea # entry age of members,  multiple entry ages are not introduced yet
  retire <<- retire  # retirement age
  max_age <<- 100 # maximum age limit
  min_ea<<-30 # minimum entry age
  max_ea<<-45 # maximum entry age
  age <<- seq(i_ea,retire)   # age vector from 30-65
  age_xr <<- seq(retire + 1,max_age)  # age vector from 66-100
  yos_xy <<- seq(ea:retire) - 1  # years of service vector
  yos_r_max<<- seq(retire:max_age)-1 # years of service vector from retirenment to max_age
  rx <<- (retire - age)  # future years before retirement vector

}


age_grp_labels<<- function(i_ea,age_limit){
  return(seq(i_ea,age_limit,5))
}

#############################################################################################
################################## Population generation ####################################
#############################################################################################

############## Generate active members population ############
generate_pop<-function(ea,retire,pop_type,pop_size,median){
        age_information(ea,retire)
        pop1<-numeric(retire-ea+1)
        if (pop_type == 'Customize curve')
          pop1 <-round(rtnorm(pop_size,median,15,left = ea-1,right = retire+1))
        if (pop_type == 'Over mature')
          pop1 <- round(rtnorm(pop_size,retire,15,left = ea-1,right = retire+1))
        if (pop_type == 'Under mature')
          pop1 <- round(rtnorm(pop_size,ea,15,left = ea-1,right = retire+1))
        pop1<-pop1[pop1!=ea-1]
        pop1<-pop1[pop1!=retire+1]
        if (pop_type == 'Uniform')
          {
          repcount <- round(pop_size/(retire-ea))
          for(i in ea:retire)
            pop1<-c(pop1,rep(i,repcount))
          pop1<-pop1[pop1!=0]
        }
        
    return(pop1)
}

############## Generate retirees population ############
population_retirees<-function(ea,retire,mort,population){
  pop_freq<-table(population)
  pop2<-ceiling(pop_freq[length(pop_freq)]*cumprod(1-get_mort(ea,retire,mort)[(retire-ea+1):(max_age-ea+1)]))
  pop3<-0
  j<-retire+1
  for(i in pop2){
    pop3<-c(pop3,rep(j,i))
    j=j+1
    if(j==max_age+1)
      break;
  }
  pop3<-pop3[2:length(pop3)]
  return(pop3)
}

#########################################################################################
################################ Salary functions #######################################
#########################################################################################

############## Actual Salary ############
get_act_sal <- function(ea,retire,a_sgr) {
  age_information(ea,retire)
  # actual salary at age x
  return(c((1 + a_sgr) ^ yos_xy))
}

############## Expected Salary ############
get_exp_sal <- function(ea,retire,sgr) {
  age_information(ea,retire)
  # expected salary at age x
  return(c((1 + sgr) ^ yos_xy))
}

############## Accumlated Salary ############
get_acc_sal <- function(ea,retire,a_sgr) {
  act_sal <- get_act_sal(ea,retire,a_sgr)
  #acculumated salary at age x
  return(c(cumsum(act_sal)))
}

############## Expected Salary at retirement ############
get_exp_sal_r <- function(ea,retire,a_sgr,sgr) {
  act_sal <- get_act_sal(ea,retire,a_sgr)
  exp_sal_r <- act_sal * ((1 + sgr) ^ rx) # expected salary at age r in year x
  return(exp_sal_r)
}

##############  Salary for constant inflation rate for different entry ages ############
get_sal_vector_ea <- function(ea,retire,sal,inflation,sgr){
  return((((1 + (inflation / 100)) ^ yos_xy)/(1 + ((sgr) / 100)) ^ yos_xy)*sal)
}

############## Entry salary from current Salary for constant inflation rate ############
get_sal_ca_ea<- function(ea,ca,retire,sal,inflation,sgr){
  return(((1 + ((sgr) / 100)) ^ yos_xy[length(age)-(retire-ca)])/((1 + (inflation / 100)) ^ yos_xy[length(age)-(retire-ca)])*sal)
}

###################################################################################################
############################### Mortality table ###################################################
###################################################################################################
get_mort <- function(ea,retire,mort) {
  age_information(ea,retire)
  annuitant_mort<-(mort+1) #skips column for annuitants
  min_mort_age<<-18
  max_mort_age<<-120
  mortality_tab<-c(mort_tables[[mort]][(ea-min_mort_age+1):(retire-min_mort_age+1)],
                   mort_tables[[(annuitant_mort)]][(retire-min_mort_age+2):(max_age-min_mort_age+1)])
  return(na.omit(mortality_tab))
}

##################################################################################################
############################### Survival probabilities ###########################################
##################################################################################################

######### Survival probablity from x to x+1 ##################
get_xpmx <- function(ea,retire,mort) {
  age_information(ea,retire)
  xpmx <- (1 - get_mort(ea,retire,mort))
  return(xpmx)
}

############# Survival probablity from x to 65 #################
get_rpmx <- function(ea,retire,mort) {
  xpmx <- get_xpmx(ea,retire,mort)
  rpmx <- c(rev(cumprod(rev(xpmx[1:(length(age) - 1)]))),1)
  return(rpmx)
}

############# Survival probablity from 65 to certain age (100 max)#########
get_xpmr <- function(ea,retire,mort) {
  xpmx <- get_xpmx(ea,retire,mort)
  
  rpmx <- get_rpmx(ea,retire,mort)
  
  xpmr <- numeric()
  xpmr[1] <- xpmx[length(age)] * rpmx[length(rpmx)]
  for (i in 1:(max_age - retire - 1))
    xpmr[i + 1] = xpmx[length(age) + i] * xpmr[i]
  
  return(xpmr[xpmr!=0])
}

#############################################################################################
########################### Benefit calculations ############################################
#############################################################################################

################ Accumulated benefits #############################
get_acc_benefits <- function(ea,retire,a_sgr,afc,bf) {
  acc_sal <- get_acc_sal(ea,retire,a_sgr)
  after_afc<-(acc_sal[(afc+2):length(acc_sal)] - acc_sal[2:(length(age)-afc)]) * yos_xy[(afc+2):length(yos_xy)]*bf/ afc
  acc_benefits <-
        c(0,(acc_sal[2:(afc+1)] - acc_sal[1]) * bf,after_afc)
  return(acc_benefits)  
}

############################ Accumlated benefits till retirement #############################
get_acc_benefit_r <- function(ea,retire,a_sgr,sgr,afc,bf) {
  if(afc>1)
    exp_sal_r <-get_exp_sal_r(ea,retire,a_sgr,sgr)[1:(length(age) - afc)]
  else
    exp_sal_r <-get_exp_sal_r(ea,retire,a_sgr,sgr)
  
  exp_sal <- get_exp_sal(ea,retire,sgr)
  act_sal <- get_act_sal(ea,retire,a_sgr)
  index_upper <- length(age) - afc
  index_lower <- length(age)
  acc_ben_avg<-numeric()
  acc_benefit_r <- exp_sal_r * (sum((1 + sgr) ^ seq(0,(-afc + 1))) / afc) * bf * (length(age) - 1)
  
  if(afc>1){
    for (i in seq((index_upper+1):index_lower)-1)
      acc_ben_avg<-c(acc_ben_avg,mean(na.omit(c(act_sal[(index_upper+1):(index_upper+1+i)],exp_sal[(index_upper+i+2):index_lower])))*bf*(length(age) - 1))
    acc_ben_avg[length(acc_ben_avg)]<-mean(act_sal[(index_upper+1):index_lower])*bf*(length(age) - 1)
  }
  acc_benefit_r <- c(acc_benefit_r,acc_ben_avg)
  
  return(acc_benefit_r)
}

############################ Accumulated benefit at 65 #############################
get_acc_benefit_65 <- function(ea,retire,a_sgr,sgr,afc,bf) {
  acc_benefit_65 <-
    as.numeric(get_acc_benefit_r(ea,retire,a_sgr,sgr,afc,bf)[length(get_acc_benefit_r(ea,retire,a_sgr,sgr,afc,bf))])
  return(acc_benefit_65)
}

############################################################################################
############################### Discount factor functions ##################################
############################################################################################

########### discount factor from x to r #########################
get_vrx <- function(ea,retire,i) {
  age_information(ea,retire)
  vrx <- c(1 / ((1 + i) ^ (rx)))
  return(vrx)
}

########### discount factor from x to r & r to 100 #########################
get_vxr <- function(ea,retire,i) {
  vxr <- c(get_vrx(ea,retire,i),c(1 / ((1 + i) ^ (seq(
    (retire + 1):max_age
  )))))
  return(vxr)
}

########################################################################################################
################################### Termination rate functions #########################################
########################################################################################################

####################### termination rates ##########################
get_qxt <- function(ea,retire) {
  age_information(ea,retire)
  min_qxt_age<<-18
  term_rate<-numeric()
  for(i in yos_xy){
    if(yos_xy[i+1]<2)
      term_rate<-c(term_rate,term_rate_xl$`Service<2`[ea-min_qxt_age+1+i]/100)
    else if(yos_xy[i+1]>=2 && yos_xy[i+1]<=4)
      term_rate<-c(term_rate,term_rate_xl$`Service=2,3,4`[ea-min_qxt_age+1+i]/100)
    else if(yos_xy[i+1]>=5 && yos_xy[i+1]<=9)
      term_rate<-c(term_rate,term_rate_xl$`Service=5-9`[ea-min_qxt_age+1+i]/100)
    else
      term_rate<-c(term_rate,term_rate_xl$`Service>=10`[ea-min_qxt_age+1+i]/100)
  }
  term_rate[is.na(term_rate)]<-0
  return(term_rate)
  
}

######### Probablity to continue to next year ###############

get_xpxT <- function(ea,retire,mort) {
  xpmx <- c(get_xpmx(ea,retire,mort)[1:length(age)])
  xpxT <- xpmx * (1 - get_qxt(ea,retire))
  return(xpxT)
}

########## Probablity to continue to 65 #####################

get_rxpxT <- function(ea,retire,mort) {
  #xpxT<-get_xpxT(ea,retire,mort)
  rxpxT<-get_rpmx(ea,retire,mort)* (1 - get_qxt(ea,retire))
  #rxpxT <- rev(cumprod(rev(xpxT))) to continue and survive
  return(rxpxT)
}

#######################################################################################################
############################### Annuity functions ####################################################
######################################################################################################

############ Straight Life Annuity factor for 1 dollar ##############
get_ar <- function(ea,retire,i,cola,mort) {
  ar <-
    sum(
      get_vxr(ea,retire,i)[(length(get_vrx(ea,retire,i))):length(get_vxr(ea,retire,i))] * 
    c(get_rpmx(ea,retire,mort)[length(get_rpmx(ea,retire,mort))],na.omit(get_xpmr(ea,retire,mort))) *
      ((1 + cola) ^ yos_r_max))

    return(ar)
}

############ period certain Annuity ##############
get_am <- function(ea,retire,i,amortization) {
  vxr <- get_vxr(ea,retire,i)
  am <- sum(vxr[1:amortization - 1])
  return(sum(am))
}

############# Temporary Annuity for a certain entry age ##############
get_tla <- function(ea,retire,i,sgr,mort,a)
  # tla for certain entry age
{
  age <- age[1:(length(age) - 1)]
  np <- numeric(length(age))
  v <- numeric(length(age))
  sts <- numeric(length(age))
  np <- v <- sts <- c(rep.int(0,(length(age))))
  xpmx <- get_xpmx(ea,retire,mort)[1:length(age)]
  
  if (a >= ea) {
    np[a - ea + 1] <- v[a - ea + 1] <- sts[a - ea + 1] <- 1
    for (j in (a - ea + 1):(length(age))) {
      np[j + 1] = xpmx[j] * np[j]
      v[j + 1] = v[j] / (1 + i)
      sts[j + 1] = sts[j] * (1 + sgr)
    }
  }
  tla <- sum((np * v * sts)[1:length(age)])
  
  return(tla)
}

############## Temporary Annuity ##################
get_tla_t <- function(ea,retire,i,sgr,mort) {
  age_information(ea,retire)
  tla_t <- as.numeric(length(age))
  for (k in 1:(length(age) - 1))
    tla_t[k] <- c(get_tla(ea,retire,i,sgr,mort,age[k]))
  return(tla_t)
}


########################  Annuity after r ##############################
get_a_after_r <- function(ea,retire,i,cola,mort) {
  after_ar <-
  rev(cumsum(rev(get_vxr(ea,retire,i)[length(get_vrx(ea,retire,i)):length(get_vxr(ea,retire,i))] * 
        c(get_rpmx(ea,retire,mort)[length(get_rpmx(ea,retire,mort))],get_xpmr(ea,retire,mort)) *
        ((1 + cola) ^ yos_r_max))))
  
  return(after_ar[2:length(after_ar)])
}

###################### Replacement rate ##########################
get_replacement_rate <- function(ea,retire,a_sgr,sgr,afc,bf) {
  acc_benefit_r<-get_acc_benefit_r(ea,retire,a_sgr,sgr,afc,bf)
  act_sal <- get_act_sal(ea,retire,a_sgr)
  repl_rate<- round((acc_benefit_r[length(acc_benefit_r)]/act_sal[length(act_sal)])*100,2)
  return(repl_rate)
}

################### Retirement Annuity ###########################
get_retirement_annuity<-function(ea,retire,a_sgr,sgr,afc,bf){
  acc_benefit<-get_acc_benefit_r(ea,retire,a_sgr,sgr,afc,bf)
  return(acc_benefit[length(acc_benefit)])
}

#############################################################################################
############################ Present value for future benefits ##############################
#############################################################################################

#################### PVFB from x to r ###################################
get_rPVFBx <- function(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort) {
  rPVFBx <-
      get_vrx(ea,retire,i) * get_xpxT(ea,retire,mort) * get_acc_benefit_r(ea,retire,a_sgr,sgr,afc,bf) * get_ar(ea,retire,i,cola,mort)
  return(rPVFBx)
}


############################ Present value Future benefits after retirement  ############################
get_rPVFBx_after_r <- function(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort) {
  a_after_r <- get_a_after_r(ea,retire,i,cola,mort)
  rPVFBx <- get_acc_benefit_65(ea,retire,a_sgr,sgr,afc,bf) * a_after_r
  
  return(rPVFBx)
}

##################################################################################
######################### grading function #######################################
##################################################################################
get_gxv <- function(ea,retire,vesting) {
  gxv <- c(rep(0,vesting), rep(100,(length(age) - vesting)))
  return(gxv)
}

##################################################################################
################################ Vesting cost ####################################
##################################################################################
get_vesting_cost <- function(ea,retire,i,a_sgr,cola,afc,bf,mort,vesting) {
  bx <- get_acc_benefits(ea,retire,a_sgr,afc,bf)
  vc <- (get_gxv(ea,retire,vesting) / 100) * bx * get_ar(ea,retire,i,cola,mort)
  return(vc)
}

###############################################################################
################################ term cost ####################################
###############################################################################
get_term_cost <- function(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort,vesting) {
  rpmx <- get_rpmx(ea,retire,mort)
  rpmx <- c(rpmx[2:length(rpmx)],1)
  vc <- get_vesting_cost(ea,retire,i,a_sgr,cola,afc,bf,mort,vesting)

  tc <- vc * get_qxt(ea,retire) * rpmx * get_vrx(ea,retire,i)
  return(tc)
}

###############################################################################
############################ PVTC #############################################
###############################################################################
get_PVTC <- function(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort,vesting,a){
  age <- age[1:(length(age) - 1)]
  tc <- get_term_cost(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort,vesting)
  np <- numeric(length(age))
  v <- numeric(length(age))
  np <- v <- c(rep.int(0,(length(age))))
  xpmx <- get_xpmx(ea,retire,mort)[1:length(age)]
  if (a >= ea) {
    np[a - ea + 1] <- v[a - ea + 1] <- 1
    for (j in (a - ea + 1):(length(age))) {
      np[j + 1] = xpmx[j] * np[j]
      v[j + 1] = v[j] / (1 + i)
    }
  }
  pvtc <- sum(np * v * tc)
  return(pvtc)
}

get_PVTC_t <- function(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort,vesting){
  pvtc_t <- as.numeric(length(age))
  for (k in 1:(length(age) - 1))
    pvtc_t[k] <-
      c(get_PVTC(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort,vesting,age[k]))
  return(c(pvtc_t,0))
}

##################################################################################
############################## AAL for cost methods ##############################
##################################################################################
get_AAL <- function(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting) {
  age_information(ea,retire)
  rPVFBx <- get_rPVFBx(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort)
  pvtc <- get_PVTC_t(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort,vesting)
  k <- (c(rPVFBx,rep(0,length(pvtc)-length(rPVFBx))) + pvtc)
  if (cm == 'EAN')
    aal <-
    k - (get_NC(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting) * c(get_tla_t(ea,retire,i,sgr,mort),get_tla(ea,retire,i,sgr,mort,age[length(age)])))
  else
    aal <- k * ((yos_xy) / (age[length(age)] - age[1]))
  return(c(aal,get_rPVFBx_after_r(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort)))
}

get_aal_pop<-function(pop,ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting){
  dat<-data.frame(Age=seq(ea,max_age),aal=pop*get_AAL(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting))
  AAL_POP<-split(dat,cut(dat$Age,age_grp_labels(ea,max_age),include.lowest = TRUE))
  aal_sum<-sum(AAL_POP[[1]]$aal.Freq)
  for(i in 2:length(names(AAL_POP)))
    aal_sum<-c(aal_sum,sum(AAL_POP[[i]]$aal.Freq))
  return(cbind(names(AAL_POP),aal_sum))
}

####################################################################
########################## Statistics ##############################
####################################################################
get_stat<-function(ea,retire,active,retirees,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting){
    active_pop<-active
    retire_pop<-retirees%>%table()
    active_pop<-table(active_pop)
    payroll<-c(get_act_sal(ea,retire,a_sgr)*active_pop,rep(0,length(c(active_pop,retire_pop))-length(active_pop)))
    nc<-c(get_NC(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)*active_pop,rep(0,length(c(active_pop,retire_pop))-length(active_pop)))
    aal<-c(active_pop,retire_pop)*get_AAL(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
    dat<-data.frame(Age=seq(ea,max_age),Payroll=payroll,"Normal Cost"=nc,AAL=aal)
    total_payroll<-sum(payroll)
    
    total_nc<-sum(nc)
    
    total_aal<-sum(aal)
  stat<-split(dat,cut(dat$Age,age_grp_labels(ea,max_age),include.lowest = TRUE))
    stats<-NULL
    for(j in 1:length(stat)){
      if(grepl('\\(',names(stat)[j])){
        num<-as.numeric(substr(x = names(stat)[j],2,3))
        names(stat)[j]<-gsub(num,num+1,names(stat)[j])
        }
      names(stat)[j]<-gsub(',','-',names(stat)[j])
      names(stat)[j]<-gsub('\\(','\\[',names(stat)[j])
      
        stats<-rbind(stats,data.frame(names(stat)[j], 
                    (sum(stat[[j]]$Payroll)/total_payroll)*100,
                    (sum(stat[[j]]$Normal.Cost)/total_nc)*100,
                    (sum(stat[[j]]$AAL)/total_aal)*100))
    }
    names(stats)<-c("Age Group","Salary/Total Salary (%)","Normal Cost/Total Normal Cost (%)","AAL/Total AAL (%)")
    return(stats)
}


get_summary<-function(pop,ea,retire,median_p,median_dr,median_sgr,i,a_sgr,sgr,pgr,cola,afc,bf,cm,mort,vesting,amortization){
  percent<-percent(median_p)
  total_aal<-sum(pop*get_AAL(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting))
  total_nc<-sum(get_NC(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)*pop[1:length(age)])
  total_assets<-sum(pop*get_median_asset(ea,retire,median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting))
  total_uaal<-total_aal-total_assets
  total_adc<-sum(get_ARC(pop,ea,retire,median_p,median_dr,median_sgr,i,a_sgr,sgr,pgr,cola,afc,bf,cm,mort,vesting,amortization))
  payroll<-sum(get_act_sal(ea,retire,a_sgr)*pop[1:length(age)])
  nc_pay<-percent(total_nc/payroll)
  uaal_pay<-percent(total_uaal/payroll)
  arc_pay<-percent(total_adc/payroll)
  total_aal<-dollar(total_aal)
  total_nc<-dollar(total_nc)
  total_uaal<-dollar(total_uaal)
  total_adc<-dollar(total_adc)
  sum_elem<-c("Funding Ratio","Total AAL","Total Normal Cost","Total UAAL","Total ADC","NC/Payroll","UAAL/Payroll","ARC/Payroll")
  sum_val<-c(percent,total_aal,total_nc,total_uaal,total_adc,nc_pay,uaal_pay,arc_pay)
  summary<-as.data.frame(cbind(sum_elem,sum_val))
  names(summary)<-c("Plan Costs","Estimate")
  return(summary)
}
  
##################################################################################
############################## NC for cost methods ##############################
#################################################################################
get_NC <- function(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting) {
  age_information(ea,retire)
  rPVFBx <- get_rPVFBx(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort)
  pvtc <- get_PVTC_t(ea,retire,i,a_sgr,sgr,cola,afc,bf,mort,vesting)
  exp_sal <- get_exp_sal(ea,retire,sgr)
  k <-(rPVFBx[1] + pvtc[1]) / (exp_sal[1] * get_tla_t(ea,retire,i,sgr,mort)[1])
  if (cm == 'EAN')
    nc <- (k) * exp_sal
  else
    nc <- ((c(rPVFBx,rep(0,length(pvtc)-length(rPVFBx))) + pvtc) / (age[length(age)] - age[1]))
  return(nc)
}


get_nc_pop<-function(pop,ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting){
  dat<-data.frame(Age=seq(ea,retire),nc=pop*get_NC(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting))
  NC_POP<-split(dat,cut(dat$Age,age_grp_labels(ea,retire),include.lowest = TRUE))
  nc_sum<-sum(NC_POP[[1]]$nc.Freq)
  for(i in 2:length(names(NC_POP)))
    nc_sum<-c(nc_sum,sum(NC_POP[[i]]$nc.Freq))
  return(cbind(names(NC_POP),nc_sum))
}

###########################################################################################
######################## Median asset conditions ##########################################
###########################################################################################
get_median_asset <-
  function(ea,retire,median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting) {
    i <- median_dr
    sgr <- median_sgr
    median_asset <-
      median_p * get_AAL(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
    return(median_asset)
  }

############################################################################
############################## Funding ratio ###############################
############################################################################
get_FR <-
  function(pop,ea,retire,median_p,median_dr,median_sgr,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting) {
    age_information(ea,retire)
    median_asset<-get_median_asset(ea,retire,median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting)
    aal<-get_AAL(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
    fr <- sum(pop * median_asset) / sum(pop * aal)
    return(fr)
  }

################################################################################################
############################### Annual Required Contribution ###################################
################################################################################################
get_ARC <-
  function(pop,ea,retire,median_p,median_dr,median_sgr,i,a_sgr,sgr,pgr,cola,afc,bf,cm,mort,vesting,amortization) {
    age_information(ea,retire)
    pop_ret<-pop[length(age)+1:(max_age-length(age)-ea+1)]
    pop<-as.vector(pop)[1:length(age)]
    new_pgr<-ifelse(i-pgr<=0,0.004999,0)
    nc<- get_NC(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
    aal<-get_AAL(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
    median_asset<-get_median_asset(ea,retire, median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting)
    uaal_pay<-sum((aal-median_asset)[1:length(age)])
    uaal_ret<-((aal[length(age)+1:(max_age-length(age)-ea+1)]-median_asset[length(age)+1:(max_age-length(age)-ea+1)])/get_am(ea,retire,i,amortization))*pop_ret
    pmt<-((uaal_pay/(1-((1+pgr)/(1+i))^amortization))*(i-pgr+new_pgr))*((1+pgr)^(yos_xy))
    arc<- c(pop * (nc + ((pmt/(i-pgr+new_pgr))*(1-((1+pgr)/(1+i))^yos_xy))),uaal_ret)
    return((arc))
  }
