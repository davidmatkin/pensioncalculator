  #####################################################################################################
  ################################################################################################
     ##################### Basic functions for Pension Simulation Calculator#################
  ################################################################################################
######################################################################################################

rm(list = ls())


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
  start_limit<-i_ea
  for(i in i_ea:age_limit-1){
    if(i%%5==0)
      start_limit<-c(start_limit,i)
  }
  start_limit<-c(start_limit,age_limit)
  return(unique(start_limit))
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
          #  pop1 <- round(runif(pop_size,min = ea-1,max = retire+1))
        }
        
    return(pop1)
}

############## Generate retirees population ############
population_retirees<-function(ea,retire,mort,population){
  pop_freq<-table(population)
  pop2<-ceiling(pop_freq[length(pop_freq)]*cumprod(1-get_mort(ea,mort)[(retire-ea+1):(max_age-ea+1)]))
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
  act_sal <- c((1 + a_sgr) ^ yos_xy) # actual salary at age x
  return(act_sal)
}

############## Expected Salary ############
get_exp_sal <- function(ea,retire,sgr) {
  age_information(ea,retire)
  exp_sal <- c((1 + sgr) ^ yos_xy) # actual salary at age x
  return(exp_sal)
}

############## Accumlated Salary ############
get_acc_sal <- function(ea,a_sgr) {
  act_sal <- get_act_sal(ea,a_sgr)
  acc_sal <- c(cumsum(act_sal)) #acculumated salary at age x
  return(acc_sal)
}

############## Expected Salary at retirement ############
get_exp_sal_r <- function(ea,a_sgr,sgr) {
  act_sal <- get_act_sal(ea,a_sgr)
  exp_sal_r <- numeric(length(act_sal))
  for (i in 1:length(act_sal)) {
    exp_sal_r[i] <-
      (act_sal[i] * ((1 + sgr) ^ rx[i])) # expected salary at age r in year x
  }
  return(na.omit(exp_sal_r))
}

##############  Salary for constant inflation rate and multiple entry ages ############
get_sal_vector_ea <- function(ea,sal,inflation,sgr){
  return((((1 + (inflation / 100)) ^ yos_xy)/(1 + ((sgr) / 100)) ^ yos_xy)*sal)
}

############## Current Salary for constant inflation rate and multiple entry ages ############
get_sal_ca <- function(ea,ca,sal,inflation,sgr){
  return(sum((((1 + sgr/100) ^ (seq(ea:ca)-1))/((1 + (inflation / 100)) ^ (seq(ea:ca)-1)))*sal))
}

###################################################################################################
############################### Mortality table ###################################################
###################################################################################################
get_mort <- function(ea,retire,mort) {
  age_information(ea,retire)
  return(na.omit(mort_tables[[mort]]))
}

##################################################################################################
############################### Survival probabilities ###########################################
##################################################################################################

######### Survival probablity from x to x+1 ##################
get_xpmx <- function(ea,retire,mort) {
  age_information(ea,retire)
  
  xpmx <- (1 - get_mort(ea,mort))
  return(xpmx)
}

############# Survival probablity from x to 65 #################
get_rpmx <- function(ea,mort) {
  xpmx <- get_xpmx(ea,mort)
  rpmx <- c(rev(cumprod(rev(xpmx[(ea-min_ea+1):(length(age)+ (ea-min_ea) - 1)]))),1)
  return(rpmx)
}

############# Survival probablity from 65 to certain age (100 max)#########
get_xpmr <- function(ea,mort) {
  xpmx <- get_xpmx(ea,mort)
  
  rpmx <- get_rpmx(ea,mort)
  xpmr <- numeric((length(xpmx))-length(age) + (ea-min_ea))
  xpmr[1] <- xpmx[length(age)+ (ea-min_ea)] * rpmx[length(rpmx)]
  for (i in 1:(max_age - retire - 1))
    xpmr[i + 1] = xpmx[length(age)+ (ea-min_ea) + i] * xpmr[i]
  
  return(xpmr[xpmr!=0])
}

#############################################################################################
########################### Benefit calculations ############################################
#############################################################################################

################ Accumulated benefits #############################
get_acc_benefits <- function(ea,a_sgr,afc,bf) {
  acc_sal <- get_acc_sal(ea,a_sgr)
  after_afc<-(acc_sal[(afc+2):length(acc_sal)] - acc_sal[2:(length(age)-afc)]) * yos_xy[(afc+2):length(yos_xy)]*bf/ afc
  
  acc_benefits <-
        c(0,(acc_sal[2:(afc+1)] - acc_sal[1]) * bf,after_afc)
  return(acc_benefits)  
}

############################ Accumlated benefits till retirement #############################
get_acc_benefit_r <- function(ea,a_sgr,sgr,afc,bf) {
  if(afc>1){
  exp_sal_r <-
    as.numeric(get_exp_sal_r(ea,a_sgr,sgr))[1:(length(age) - afc)]
  }
  else{
    exp_sal_r <-
      as.numeric(get_exp_sal_r(ea,a_sgr,sgr))
  }
  # accumulated benefit at age r
  exp_sal <- get_exp_sal(ea,sgr)
  
  act_sal <- get_act_sal(ea,a_sgr)
  index_upper = length(age) - afc
  index_lower = length(age)
  acc_benefit_r <-
    (exp_sal_r * (sum((1 + sgr) ^ seq(0,(
      -afc + 1
    )))) / afc) * bf * (length(age) - 1)
  if(afc>1){
  for (i in index_upper:index_lower)
    acc_benefit_r1 <- na.omit(cumsum(act_sal[index_upper:i + 1]))
  acc_benefit_r2 <-
    rev(cumsum(rev(na.omit(exp_sal[index_upper + 1:index_lower]))))
  acc_benefit_r3 <-
    acc_benefit_r1[1:(afc - 1)] + acc_benefit_r2[2:afc]
  acc_benefit_r3 <-
    c((acc_benefit_r3 / afc) * bf * (length(age) - 1),acc_benefit_r1[afc] * bf *
        (length(age) - 1) / afc)
  acc_benefit_r <- c(acc_benefit_r,acc_benefit_r3)
  }
  return(acc_benefit_r)
}

############################ Accumulated benefit at 65 #############################
get_acc_benefit_65 <- function(ea,a_sgr,sgr,afc,bf) {
  acc_benefit_65 <-
    as.numeric(get_acc_benefit_r(ea,a_sgr,sgr,afc,bf)[length(get_acc_benefit_r(ea,a_sgr,sgr,afc,bf))])
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
get_vxr <- function(ea,i) {
  vxr <- c(get_vrx(ea,i),c(1 / ((1 + i) ^ (seq(
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
  #qxt<-term_rates$`30`[11:length(term_rates$`30`)]
  qxt <- term_rate_xl$`Termination rate` / 100
  # rate of termination
  return(qxt[(ea-30+1):length(qxt)])
  
}

######### Probablity to continue to next year ###############

get_xpxT <- function(ea,mort) {
  xpmx <- c(get_xpmx(ea,mort)[1:length(age)])
  xpxT <- xpmx * (1 - get_qxt(ea))
  return(xpxT)
}

########## Probablity to continue to 65 #####################

get_rxpxT <- function(ea,mort) {
  qxt <- get_qxt(ea)
  rxpxT <- get_rpmx(ea,mort) * (1 - qxt)
  return(rxpxT)
}

#######################################################################################################
############################### Annuity functions ####################################################
######################################################################################################

############ Straight Life Annuity ##############
get_ar <- function(ea,i,cola,mort) {
  ar <-
    sum(get_vxr(ea,i)[length(get_vrx(ea,i)):length(get_vxr(ea,i))] * 
          c(get_rpmx(ea,mort)[length(get_rpmx(ea,mort))],na.omit(get_xpmr(ea,mort))) *
          ((1 + cola) ^ yos_r_max))

    return(ar)
}

############ Present value of Annuity ##############
get_ar <- function(ea,i,cola,mort) {
  ar <-
    sum(get_vxr(ea,i)[length(get_vrx(ea,i)):length(get_vxr(ea,i))] * 
          c(get_rpmx(ea,mort)[length(get_rpmx(ea,mort))],na.omit(get_xpmr(ea,mort))) *
          ((1 + cola) ^ yos_r_max))
  
  return(ar)
}

############ period certain Annuity ##############
get_am <- function(ea,i,amortization) {
  vxr <- get_vxr(ea,i)
  am <- sum(vxr[1:amortization - 1])
  return(sum(am))
}

############# Temporary Annuity for a certain entry age ##############
get_tla <- function(ea,i,sgr,mort,a)
  # tla for certain entry age
{
  age <- age[1:(length(age) - 1)]
  np <- numeric(length(age))
  v <- numeric(length(age))
  sts <- numeric(length(age))
  np <- v <- sts <- c(rep.int(0,(length(age))))
  xpmx <- get_xpmx(ea,mort)[1:length(age)]
  
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
get_tla_t <- function(ea,i,sgr,mort) {
  tla_t <- as.numeric(length(age))
  for (k in 1:(length(age) - 1))
    tla_t[k] <- c(get_tla(ea,i,sgr,mort,age[k]))
  return(tla_t)
}


########################  Annuity after r ##############################
get_a_after_r <- function(ea,i,cola,mort) {
  after_ar <-
  rev(cumsum(rev(get_vxr(ea,i)[length(get_vrx(ea,i)):length(get_vxr(ea,i))] * 
        c(get_rpmx(ea,mort)[length(get_rpmx(ea,mort))],get_xpmr(ea,mort)) *
        ((1 + cola) ^ yos_r_max))))
  
  return(after_ar[2:length(after_ar)])
}

#############################################################################################
############################ Present value for future benefits ##############################
#############################################################################################

#################### PVFB from x to r ###################################
get_rPVFBx <- function(ea,i,a_sgr,sgr,cola,afc,bf,mort) {
  rPVFBx <-
    get_vrx(ea,i) * get_rxpxT(ea,mort) * get_acc_benefit_r(ea,a_sgr,sgr,afc,bf) * get_ar(ea,i,cola,mort)
  
  return(rPVFBx)
}


############################ Future benefits after retirement  ############################
get_xPVFBr <- function(ea,i,a_sgr,sgr,cola,afc,bf,mort) {
  a_after_r <- get_a_after_r(ea,i,cola,mort)
  xPVFBr <- get_acc_benefit_65(ea,a_sgr,sgr,afc,bf) * a_after_r
  
  return(xPVFBr)
}

##################################################################################
######################### grading function #######################################
##################################################################################
get_gxv <- function(ea,vesting) {
  gxv <- c(rep(0,vesting), rep(100,(length(age) - vesting)))
  return(gxv)
}

##################################################################################
################################ Vesting cost ####################################
##################################################################################
get_vesting_cost <- function(ea,i,a_sgr,cola,afc,bf,mort,vesting) {
  bx <- get_acc_benefits(ea,a_sgr,afc,bf)
  vc <- (get_gxv(ea,vesting) / 100) * bx * get_ar(ea,i,cola,mort)
  return(vc)
}

###############################################################################
################################ term cost ####################################
###############################################################################
get_term_cost <- function(ea,i,a_sgr,sgr,cola,afc,bf,mort,vesting) {
  rpmx <- get_rpmx(ea,mort)
  rpmx <- c(rpmx[2:length(rpmx)],1)
  vc <- get_vesting_cost(ea,i,a_sgr,cola,afc,bf,mort,vesting)
  tc <- vc * get_qxt(ea) * rpmx * get_vrx(ea,i)
  return(tc)
}

###############################################################################
############################ PVTC #############################################
###############################################################################
get_PVTC <- function(ea,i,a_sgr,sgr,cola,afc,bf,mort,vesting,a)
{
  age <- age[1:(length(age) - 1)]
  tc <- get_term_cost(ea,i,a_sgr,sgr,cola,afc,bf,mort,vesting)
  np <- numeric(length(age))
  v <- numeric(length(age))
  np <- v <- c(rep.int(0,(length(age))))
  xpmx <- get_xpmx(ea,mort)[1:length(age)]
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

get_PVTC_t <- function(ea,i,a_sgr,sgr,cola,afc,bf,mort,vesting)
{
  pvtc_t <- as.numeric(length(age))
  for (k in 1:(length(age) - 1))
    pvtc_t[k] <-
      c(get_PVTC(ea,i,a_sgr,sgr,cola,afc,bf,mort,vesting,age[k]))
  return(pvtc_t)
}

##################################################################################
############################## AAL for cost methods ##############################
##################################################################################
get_AAL <- function(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting) {
  age_information(ea,retire)
  rPVFBx <- get_rPVFBx(ea,i,a_sgr,sgr,cola,afc,bf,mort)
  pvtc <- as.numeric(length(age))
  for (l in 1:(length(age) - 1))
    pvtc[l] <-
    c(get_PVTC(ea,i,a_sgr,sgr,cola,afc,bf,mort,vesting,age[l]))
  pvtc <- c(pvtc,pvtc[length(pvtc)])
  
  k <- (rPVFBx + pvtc)
  
  if (cm == 'EAN')
    aal <-
    k - (get_NC(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting) * c(get_tla_t(ea,i,sgr,mort),get_tla(ea,i,sgr,mort,age[length(age)])))
  else
    aal <- k * ((yos_xy) / (age[length(age)] - age[1]))
  return(c(aal,get_xPVFBr(ea,i,a_sgr,sgr,cola,afc,bf,mort)))
}



get_aal_pop<-function(pop,ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting){
  dat<-data.frame(Age=seq(ea,max_age),aal=pop*get_AAL(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting))
  AAL_POP<-split(dat,cut(dat$Age,age_grp_labels(ea,max_age),include.lowest = TRUE))
  aal_sum<-sum(AAL_POP[[1]]$aal.Freq)
  for(i in 2:length(names(AAL_POP)))
    aal_sum<-c(aal_sum,sum(AAL_POP[[i]]$aal.Freq))
  return(cbind(names(AAL_POP),aal_sum))
}

####################################################################
########################## Statistics ##############################
####################################################################
get_stat<-function(ea,active,retirees,median_p,median_dr,median_sgr,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting,amortization){
active_pop<-active
retire_pop<-retirees%>%table()
active_pop<-table(active_pop)
payroll<-c(get_act_sal(ea,a_sgr)*active_pop,rep(0,length(c(active_pop,retire_pop))-length(active_pop)))
nc<-c(get_NC(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)*active_pop,rep(0,length(c(active_pop,retire_pop))-length(active_pop)))
asset<-c(active_pop,retire_pop)*get_median_asset(ea,median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting)
aal<-c(active_pop,retire_pop)*get_AAL(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
adc<-get_ARC(c(active_pop,retire_pop),ea,median_p,median_dr,median_sgr,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting,amortization)
uaal<-(aal-asset)
dat<-data.frame(Age=seq(ea,max_age),Payroll=payroll,"Normal Cost"=nc,Assets=asset,UAAL=uaal,
           AAL=aal,ADC=adc)
total_payroll<-sum(payroll)
total_nc<-sum(nc)
total_assets<-sum(asset)
total_aal<-sum(aal)
total_uaal<-sum(uaal)
total_adc<-sum(adc)
stat<-split(dat,cut(dat$Age,age_grp_labels(ea,max_age),include.lowest = TRUE))
stats<-NULL

for(i in 1:length(stat)){
    stats<-rbind(stats,data.frame(names(stat)[i], 
                sum(stat[[i]]$Payroll)/total_payroll*100,
                sum(stat[[i]]$Normal.Cost)/total_nc*100,
                sum(stat[[i]]$Assets)/total_assets*100,
                sum(stat[[i]]$UAAL)/total_uaal*100,
                sum(stat[[i]]$AAL)/total_aal*100,
                sum(stat[[i]]$ADC)/total_adc*100))
}
names(stats)<-c("Age Group","Payroll (%)","Normal Cost (%)","Assets (%)","UAAL (%)","AAL (%)","ADC (%)")
return(stats)
}
##################################################################################
############################## NC for cost methods ##############################
#################################################################################
get_NC <- function(ea,retire,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting) {
  age_information(ea,retire)
  rPVFBx <- get_rPVFBx(ea,i,a_sgr,sgr,cola,afc,bf,mort)
  
  pvtc <- as.numeric(length(age))
  for (l in 1:(length(age) - 1))
    pvtc[l] <-
    c(get_PVTC(ea,i,a_sgr,sgr,cola,afc,bf,mort,vesting,age[l]))
  pvtc <- c(pvtc,pvtc[length(pvtc)])
  
  exp_sal <- get_exp_sal(ea,sgr)
  
  k <-
    (rPVFBx[1] + pvtc[1]) / (exp_sal[1] * get_tla_t(ea,i,sgr,mort)[1])
  
  if (cm == 'EAN')
    nc <- (k) * exp_sal
  else
    nc <- ((rPVFBx + pvtc) / (age[length(age)] - age[1]))
  
  return(nc)
}


get_nc_pop<-function(pop,ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting){
  dat<-data.frame(Age=seq(ea,retire),nc=pop*get_NC(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting))
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
  function(ea,median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting) {
    i <- median_dr
    sgr <- median_sgr
    median_asset <-
      median_p * get_AAL(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
    return(median_asset)
  }

############################################################################
############################## Funding ratio ###############################
############################################################################
get_FR <-
  function(pop,ea,retire,median_p,median_dr,median_sgr,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting) {
    age_information(ea,retire)
    median_asset<-get_median_asset(ea,median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting)
    aal<-get_AAL(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
    fr <- sum(pop * median_asset) / sum(pop * aal)
    
    return(fr)
  }

################################################################################################
############################### Annual Required Contribution ###################################
################################################################################################
get_ARC <-
  function(pop,ea,retire,median_p,median_dr,median_sgr,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting,amortization) {
    age_information(ea,retire)
    pop_all<-pop
    pop<-as.vector(pop)[1:length(age)]
    nc<- get_NC(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
    aal<-get_AAL(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
    median_asset<-get_median_asset(ea, median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting)
    aal_comp<-((pop_all * aal - pop_all * median_asset)/ sum(pop_all) * get_am(ea,i,amortization))
    
    
    arc <-c((pop * nc)+aal_comp[1:length(pop)],aal_comp[(length(pop)+1):length(aal_comp)]) 
    return((arc))
  }
