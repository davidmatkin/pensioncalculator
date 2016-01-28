########################## winklevoss Import ##########################################
# This program imports the following data for the actuarial valuation model
# Decrement tables

rm(list=ls())

library(XLConnect) # slow but convenient because it reads ranges
library(magrittr)
library(dplyr)

cton <- function (cvar) as.numeric(gsub("[ ,$%]", "", cvar))  # character to numeric, eliminating "," "$" "%". chars will become NA


########################### Setting File path and loading workbook ####################################
file_path <- paste0("E://SUNYALBANY/Pension_Simulation_Project/Code/Data/")
wvxl <- "Winklevoss(6).xlsx"
wb<- XLConnect::loadWorkbook(paste0(file_path,wvxl))

########################## data table 2-3 select and ultimate Termination rates ###############
term_rates <- readWorksheet(wb, sheet = "Tab2-3TermRates", startRow = 3)
term_rates <- term_rates %>%mutate_each(funs(cton))
names(term_rates) <- c("agex", "20","25","30","35","40","45","50","55","60")

######################## data table 2-5 disabled life mortality rates ###############################
dis_mort_rate <- readWorksheet(wb, sheet = "Tab2-5DisbLife", startRow = 3)
dis_mort_rate <- dis_mort_rate %>%mutate_each(funs(cton))
names(dis_mort_rate) <- c("age", "qxmd")

########################## data table 2-7 disability rates ##########################################
disb_rate <- readWorksheet(wb, sheet = "Tab2-7Disb", startRow = 3)
disb_rate <- disb_rate %>%mutate_each(funs(cton))
names(disb_rate) <- c("age", "qxd")

###################### data table 2-9 early retirement rates ################################################
early_rtmt <- readWorksheet(wb, sheet = "Tab2-9EarlyRet", startRow = 3)
early_rtmt <- early_rtmt %>%mutate_each(funs(cton))
names(early_rtmt) <- c("age", "erqx")


################## data table 2-10 Merit Salary Scale ###########################################
merit <- readWorksheet(wb, sheet ="Tab2-10Merit", startRow = 3)
merit <- merit %>%mutate_each(funs(cton))
names(merit) <- c("age", "scale")

############### data table 4-6 hiring distribution table ##########################################
hire <- readWorksheet(wb, sheet = "Tab4-6HireDist", startRow = 2)
hire <- hire[-1, ]
hire <- hire %>%  mutate_each(funs(cton))
names(hire) <- c("eage", "dist", "salscale")


############### data table 4-6 hiring distribution table ##########################################
rxpxT <- readWorksheet(wb, sheet = "tab3-1 calcs", startRow = 4, startCol = 21)
rxpxT <- rxpxT %>%  mutate_each(funs(cton))
names(rxpxT) <- c("ea20", "ea30", "ea50","","book tab 3-1")
rxpxT[is.na(rxpxT)] <- 0 # setting NA in the data frame to 0

################### Save  data #############################################################
save(rxpxT,term_rates,dis_mort_rate,disb_rate,early_rtmt,merit,hire,file = paste0(file_path, "winklevossdata.RData"))
