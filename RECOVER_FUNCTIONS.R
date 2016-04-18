################################################################################################
################################ Basic functions ###############################################
################################################################################################

rm(list = ls())

###################### Load winklevoss and mortality tables #################################
load('winklevossdata.RData')
load('Mortality.RData')
load('Termination.RData')

############################## Age information for simulation ###################################

age_information <<- function(i_ea) {
  ea <<- i_ea
  age <<- seq(i_ea,65)   # ages 30-65
  age_xr <<- seq(66,120)  # ages 66-120
  yos_xy <<- seq(ea:65) - 1  # years of service
  rx <<- c(65 - age)  # future years before retirement
}

get_age <- function(ea) {
  age_information(ea)
  return(age)
}

################################ Actual salary and expected salary ########################

get_act_sal <- function(a_sgr) {
  act_sal <- c((1 + a_sgr) ^ yos_xy) # actual salary at age x
  return(act_sal)
}

get_exp_sal <- function(sgr) {
  exp_sal <- c((1 + sgr) ^ yos_xy) # actual salary at age x
  return(exp_sal)
}

get_acc_sal <- function(a_sgr) {
  act_sal <- get_act_sal(a_sgr)
  acc_sal <- c(cumsum(act_sal)) #acculumated salary at age x
  return(acc_sal)
}

get_exp_sal_r <- function(a_sgr,sgr) {
  act_sal <- get_act_sal(a_sgr)
  exp_sal_r <- numeric(length(act_sal))
  for (i in 1:length(act_sal)) {
    exp_sal_r[i] <-
      (act_sal[i] * ((1 + sgr) ^ rx[i])) # expected salary at age r in year x
  }
  return(na.omit(exp_sal_r))
}

get_sal_vector_ea<-function(ea,sal,inflation,sgr)
{
#   ages<-seq(ea,retire)
#   inflation<-cumprod(rep(1+inflation,length(ages)-1))
#   salary<-c(sal,sal*inflation)
  
  salary<- ((1+((inflation+sgr)/100))^yos_xy)/((1+(inflation/100))^yos_xy)
  
  return(salary)
}

###############################Mortality table ###################################################

get_mort<-function(m){return(mort_tables[[m]])}

############################### Survival probabilities ###########################################

get_xpmx <- function(mort) {
  xpmx <- (1 - mort_tables[[mort]])
  # Survival probablity from x to x+1
  return(xpmx)
}

get_rpmx <- function(mort) {
  xpmx <- get_xpmx(mort)
  rpmx <- c(rev(cumprod(rev(xpmx[1:(length(age) - 1)]))),1)
  # Survival probablity from x to 65
  return(rpmx)
}


get_xpmr <- function(mort) {
  xpmx <- get_xpmx(mort)
  rpmx <- get_rpmx(mort)
  xpmr <- numeric(length((length(age) + 1):(length(xpmx) - 1)))
  xpmr[1] <- xpmx[length(age)] * rpmx[length(rpmx)]
  for (i in 1:54)
    xpmr[i + 1] = xpmx[length(age) + i] * xpmr[i]
  # Survival probablity from 65 to certain age (120 max)#########
  return(xpmr)
}


########################### Benefit calculations ############################################

get_acc_benefits <- function(a_sgr,afc,bf) {
  acc_sal <- get_acc_sal(a_sgr)
  benefit_factor <- bf
  afc_limit<-(length(acc_sal) - afc)
  acc_benefits <-
    c(0,sapply((acc_sal[2:(afc+1)] - acc_sal[1]),function(b)
      b * benefit_factor),
      sapply(((
        acc_sal[(afc+2):length(acc_sal)] - acc_sal[2:afc_limit]
      ) * yos_xy[(afc+2):length(yos_xy)]),
      function(c)
        (c * benefit_factor) / afc))
  # accumulated benefit at age x
  return(acc_benefits)
}


get_acc_benefit_r <- function(a_sgr,sgr,afc,bf) {
  exp_sal_r <- as.numeric(get_exp_sal_r(a_sgr,sgr))[1:(length(age) - afc)]
  
  # accumulated benefit at age r
  exp_sal <- get_exp_sal(sgr)
  
  act_sal <- get_act_sal(a_sgr)
  index_upper = length(age) - afc
  index_lower = length(age)
  acc_benefit_r <-(exp_sal_r*(sum((1+sgr)^seq(0,(-afc+1))))/afc) * bf * (length(age) - 1)

  for (i in index_upper:index_lower)
    acc_benefit_r1 <- na.omit(cumsum(act_sal[index_upper:i + 1]))
  acc_benefit_r2 <-
    rev(cumsum(rev(na.omit(exp_sal[index_upper + 1:index_lower]))))
  acc_benefit_r3 <- acc_benefit_r1[1:(afc-1)] + acc_benefit_r2[2:afc]
  acc_benefit_r3 <-
    c((acc_benefit_r3 / afc) * bf * (length(age) - 1),acc_benefit_r1[afc] * bf *
        (length(age) - 1) / afc)
  acc_benefit_r <- c(acc_benefit_r,acc_benefit_r3)
  return(acc_benefit_r)
}


get_acc_benefit_65 <- function(a_sgr,sgr,afc,bf) {
  acc_benefit_65 <-
    as.numeric(get_acc_benefit_r(a_sgr,sgr,afc,bf)[length(get_acc_benefit_r(a_sgr,sgr,afc,bf))])
  return(acc_benefit_65)
}

############################ Discount factor ###############################################
get_vrx <- function(i) {
  vrx <- c(1 / ((1 + i) ^ (65 - age)))
  # discount factor from x till r
  return(vrx)
}

get_vxr <- function(i) {
  vxr <- c(get_vrx(i),c(1 / ((1 + i) ^ (seq(
    66:120
  )))))
  # discount factor from x till r and r to 120
  return(vxr)
}

####################### term rates ##########################
get_qxt <- function() {
  #qxt<-term_rates$`30`[11:length(term_rates$`30`)]
  qxt <- term_rate_xl$`Termination rate` / 100
  # rate of termination
  return(qxt)
}

############################# Probablity to continue to next year ################################

get_xpxT <- function(mort) {
  xpmx <- c(get_xpmx(mort)[1:length(age)])
  xpxT <- xpmx * (1 - get_qxt())
  return(xpxT)
}

############################# Probablity to continue to next n years ################################

get_npxT <- function(mort) {
  xpxT <- get_xpxT(mort)
  
  npxT <- numeric(length(xpxT))
  npxT <- cumprod(rev(xpxT))
  # probablity for a y = 30 year old to continue to 65
  return(npxT)
}
############################# Probablity to continue to 65 ################################

get_rxpxT <- function(mort) {
  #rxpxT<-c(rxpxT$`book tab 3-1`[5:9],rxpxT$'ea30'[10:40])
  # probablity for a y = 30 year old to continue to 65
  qxt <- get_qxt()
  rxpxT <- get_rpmx(mort)[1:length(qxt)] * (1 - qxt)
  return(rxpxT)
}

################################ Annuity at r ##############################################

############ Straight Life Annuity ##############
get_ar <- function(i,cola,mort) {
  ar <-
    sum(get_vxr(i)[length(get_vrx(i)):length(get_vxr(i))] * c(get_rpmx(mort)[length(get_rpmx(mort))],get_xpmr(mort)) *
          (1 + cola) ^ c(age[length(age)] - 65,age_xr - 65))
  return(ar)
}

################# n annuity #########################
get_an <- function(i,cola,mort,amortization) {
  an <-
    sum(
      get_vxr(i)[length(get_vrx(i)):(length(get_vrx(i)) + amortization)] * c(get_rpmx(mort)[length(get_rpmx(mort))],get_xpmr(mort))[1:(amortization -
                                                                                                                                         1)] * (1 + cola) ^ c(age[length(age)] - amortization,age_xr - amortization)
    )
  return(an)
}


############ period certain Life Annuity ##############
get_am <- function(i,amortization) {
  vxr <- get_vxr(i)
  #vrx <- get_vrx(i)
  #  am<-sum(vxr[length(vrx):(length(vrx)+amortization)])+(get_rpmx(mort)[amortization]*vxr[amortization]*get_ar(i,cola,mort)[amortization])
  #am <- sum(vxr[length(vrx):(length(vrx) + amortization - 1)])
  am <- sum(vxr[1: amortization - 1])
  
  return(am)
}

############# Temporary Annuity ##############
get_tla <- function(i,sgr,mort,a)
  # tla for certain entry age
{
  age <- age[1:(length(age) - 1)]
  np <- numeric(length(age))
  v <- numeric(length(age))
  sts <- numeric(length(age))
  np <- v <- sts <- c(rep.int(0,(length(age))))
  xpmx <- get_xpmx(mort)[1:length(age)]
  
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

get_tla_t <- function(i,sgr,mort) {
  tla_t <- as.numeric(length(age))
  for (k in 1:(length(age) - 1))
    tla_t[k] <- c(get_tla(i,sgr,mort,age[k]))
  return(tla_t)
}


########################  Annuity after r ##############################
get_a_after_r<-function(i,cola,mort) {
  after_ar <-
    rev(cumsum(rev(get_vxr(i)[length(get_vrx(i)):length(get_vxr(i))] * c(get_rpmx(mort)[length(get_rpmx(mort))],get_xpmr(mort)) *
          (1 + cola) ^ c(age[length(age)] - 65,age_xr - 65))))[1:36]
  return(after_ar)
  }



############################ Present value for future benefits ############################
get_rPVFBx <- function(i,a_sgr,sgr,cola,afc,bf,mort) {
  #  rPVFBx<-sapply(get_vrx(i)*get_rxpxT(), function(d) d*get_acc_benefit_r(sgr,bf)*get_ar(i,cola,mort))
  rPVFBx <-
    get_vrx(i) * get_rxpxT(mort) * get_acc_benefit_r(a_sgr,sgr,afc,bf) * get_ar(i,cola,mort)
  
  return(rPVFBx)
}


############################ Future benefits after retirement  ############################
get_xPVFBr <- function(i,a_sgr,sgr,cola,afc,bf,mort) {
  a_after_r<-get_a_after_r(i,cola,mort)
  a_after_r<-a_after_r[2:length(a_after_r)]
  xPVFBr <-get_acc_benefit_65(a_sgr,sgr,afc,bf) * a_after_r
  
  return(xPVFBr[1:35])
}


######################### grading function #######################################
get_gxv <- function(vesting) {
  gxv <- c(rep(0,vesting), rep(100,(length(age) - vesting)))
  return(gxv)
}

################################ Vesting cost ####################################
get_vesting_cost <- function(i,a_sgr,cola,afc,bf,mort,vesting) {
  bx <- get_acc_benefits(a_sgr,afc,bf)
  vc <- (get_gxv(vesting) / 100) * bx * get_ar(i,cola,mort)
  return(vc)
}

################################ term cost ####################################
get_term_cost <- function(i,a_sgr,sgr,cola,afc,bf,mort,vesting) {
  rpmx <- get_rpmx(mort)
  rpmx <- c(rpmx[2:length(rpmx)],1)
  vc <- get_vesting_cost(i,a_sgr,cola,afc,bf,mort,vesting)
  tc <- vc * get_qxt() * rpmx * get_vrx(i)
  return(tc)
}


############################ PVTC #############################################
get_PVTC <- function(i,a_sgr,sgr,cola,afc,bf,mort,vesting,a)
{
  age <- age[1:(length(age) - 1)]
  tc <- get_term_cost(i,a_sgr,sgr,cola,afc,bf,mort,vesting)
  np <- numeric(length(age))
  v <- numeric(length(age))
  np <- v <- c(rep.int(0,(length(age))))
  xpmx <- get_xpmx(mort)[1:length(age)]
  if (a >= ea) {
    np[a - ea + 1] <- v[a - ea + 1] <- 1
    for (j in (a - ea + 1):(length(age))) {
      np[j + 1] = xpmx[j] * np[j]
      v[j + 1] = v[j] / (1 + i)
    }
  }
  pvtc <- sum((np * v * tc)[1:length(age)])
  return(pvtc)
}

get_PVTC_t <- function(i,a_sgr,sgr,cola,afc,bf,mort,vesting)
{
  pvtc_t <- as.numeric(length(age))
  for (k in 1:(length(age) - 1))
    pvtc_t[k] <-
      c(get_PVTC(i,a_sgr,sgr,cola,afc,bf,mort,vesting,age[k]))
  return(pvtc_t)
}

############################## AAL for cost methods ##############################
get_AAL <- function(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting) {
  age_information(ea)
  rPVFBx <- get_rPVFBx(i,a_sgr,sgr,cola,afc,bf,mort)
  pvtc <- as.numeric(length(age))
  for (l in 1:(length(age) - 1))
    pvtc[l] <- c(get_PVTC(i,a_sgr,sgr,cola,afc,bf,mort,vesting,age[l]))
  pvtc <- c(pvtc,pvtc[length(pvtc)])
  
  k <- (rPVFBx + pvtc)
  
  if (cm == 'EAN')
    aal <-
    k - (get_NC(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting) * c(get_tla_t(i,sgr,mort),get_tla(i,sgr,mort,age[length(age)])))
  else
    aal <- k * ((yos_xy) / (age[length(age)] - age[1]))
  return(c(aal,get_xPVFBr(i,a_sgr,sgr,cola,afc,bf,mort)))
}


############################## NC for cost methods ##############################
get_NC <- function(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting) {
  age_information(ea)
  rPVFBx <- get_rPVFBx(i,a_sgr,sgr,cola,afc,bf,mort)
  
  pvtc <- as.numeric(length(age))
  for (l in 1:(length(age) - 1))
    pvtc[l] <- c(get_PVTC(i,a_sgr,sgr,cola,afc,bf,mort,vesting,age[l]))
  pvtc <- c(pvtc,pvtc[length(pvtc)])
  
  exp_sal <- get_exp_sal(sgr)
    
  k <-
    (rPVFBx[1] + pvtc[1]) / (exp_sal[1] * get_tla_t(i,sgr,mort)[1])
  
  if (cm == 'EAN')
    nc <- (k) * exp_sal
  else
    nc <- ((rPVFBx + pvtc) / (age[length(age)] - age[1]))
  
  return(nc)
}

######################## Median asset conditions ##########################################

get_median_asset <-
  function(ea,median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting) {
    i <- median_dr
    sgr <- median_sgr
    median_asset <-
      median_p * get_AAL(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)
    return(median_asset)
  }


############################## Funding ratios ##############################

get_FR <-
  function(pop,ea,median_p,median_dr,median_sgr,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting) {
    age_information(ea)
    fr <-
      sum(pop*get_median_asset(ea,median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting)
      ) / sum(pop*get_AAL(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting))
    fr[is.na(fr)] <- 0
    
    return(fr)
  }

get_ARC <-
  function(pop,ea,median_p,median_dr,median_sgr,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting,amortization) {
    age_information(ea)
#     arc <-(as.vector(pop[1:(65-ea+1)])*(get_NC(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting)) + 
#           (((pop*(get_AAL(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting))) -
#            (pop*get_median_asset(ea, median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting)))
#           /sum(pop)*get_am(i,amortization))
#     )
    
    
    arc<-as.vector(pop[names(pop)<=65])*get_NC(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting) + 
    ((pop*get_AAL(ea,i,a_sgr,sgr,cola,afc,bf,cm,mort,vesting) - pop*get_median_asset(ea, median_p,median_dr,median_sgr,a_sgr,cola,afc,bf,cm,mort,vesting))
    /sum(pop)*get_am(i,amortization))
    
    
    
    return(sum(arc))
  }
