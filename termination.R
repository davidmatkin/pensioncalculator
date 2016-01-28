########################## winklevoss Import ##########################################
# This program imports the following data for the actuarial valuation model
# Decrement tables

rm(list=ls())

library(XLConnect) # slow but convenient because it reads ranges
library(magrittr)


########################### Setting File path and loading workbook ####################################
file_path <- paste0("E://SUNYALBANY/Pension_Simulation_Project/Code/Data/")
term_rate_input <- "simulation Aug30_original.xlsm"
wb<- XLConnect::loadWorkbook(paste0(file_path,term_rate_input))

################## termination rate ###############
term_rate_xl <- readWorksheet(wb, sheet = "Termination cost")
names(term_rate_xl) <- c("Age","Service","Termination rate","gxv","Bx","qxt","p(r-(x+1))","vrx","annuity","Vesting cost","TC","Salary","TC/salary")


################### Save  data #############################################################
save(term_rate_xl,file = paste0(file_path, "Termination.RData"))
