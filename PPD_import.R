############################# The PPD dataset###################################

# Aim: Accessing data for the Public Plans Database from the following API
# http://publicplansdata.org/public-plans-database/api/

######################### Setting environment ##################################
rm(list=ls())# To remove stored objects in environment


############################ Loading Libraries #################################
library(jsonlite)# to read json results from the API

########################### Setting File path ####################################
file_path <- paste0("E://SUNYALBANY/Pension_Simulation_Project/Code/Data/")

######################### API QUERIES ##########################################
api_query2<-NULL #character array for generating API queries to retrieve each dataset in PPD
api_query1 <- paste0("http://publicplansdata.org/api/?q=ListDataSets&format=json") #character string to store query for retrieving available datasets in PPD 


############## Retrieving available datasets from PPD ##########################
lookUp<-URLencode(api_query1)# Encoding URL
rd<-readLines(lookUp, warn=F)
datasets_available<- fromJSON(rd)
datasets_available<-c(datasets_available$'table_name'[2:29])

############## Generating API queries for each dataset #########################################################################
for (dataset in 1:length(datasets_available))
{
  api_query2[dataset]<-URLencode(paste0("http://publicplansdata.org/api/?q=QDataset&dataset=",datasets_available[dataset],"&format=json"))
}
  
############# Retrieving all datasets from the PPD and storing into dataframes ################################################
for (i in 1:length(datasets_available)){
  x<-URLencode(api_query2[i])
  assign(datasets_available[i],fromJSON(readLines(x, warn=F)))
}

datasets_available
############# Ranges from PPD ################################################################################################
##   <Remove ranges and put a function here to get range from PPD> 
discount_range.min=min(pensiongasbassumptions$BlendedDiscountRate,na.rm=TRUE)
discount_range.max=max(pensiongasbassumptions$BlendedDiscountRate,na.rm=TRUE)

cost_methods=c("PUC","EAN","AGG")

amortization_period<-rbind(subset(pensiongasbassumptions,ActCostMethCode_GASB==1,select = UAALAmortPeriod_GASB),subset(pensiongasbassumptions,ActCostMethCode_GASB==2,select = UAALAmortPeriod_GASB),subset(pensiongasbassumptions,ActCostMethCode_GASB==4,select = UAALAmortPeriod_GASB))
amortization_period.min<-min(amortization_period$UAALAmortPeriod_GASB,na.rm=TRUE)
amortization_period.max<-max(amortization_period$UAALAmortPeriod_GASB,na.rm=TRUE)

######################## Creating Rdata files for PPD Datasets##################################
save(pensiongasbschedules,pensiongasbassumptions,pensionincomestatement,pensionassetallocation,pensioninvestmentreturn,pensionactuarialcosts,pensionprovisions,pensionmembership,pensionreportingdates,pensioneegroupbasics,pensionplanbasics,pensiontierbasics,nationaldatappd,nationaldatacensus,ppdstatedata,pensionnormalretirementeligibility,pensionnormalretirementbenefit,pensionfinalaveragesalary,pensionbenefitcap,pensionearlyretirementeligibility,pensionearlyretirementreduction,pensionemployeecontributionrate,pensioncolabenefit,pensionretirementsystembasics,pensionfundingandmethods,pensionactuarialliabilities,pensionbenefitmin,pensionsystemdata,file = paste0(file_path, "PPD_datasets.RData"))
