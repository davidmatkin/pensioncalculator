#########################################################################################################
#########################################################################################################
#                                                                                                       #
#                                          DESCRIPTION                                                  #
#   termination.R contains code for extracting data from an excel sheet titled                          #
#  'soa_Summary_Tables.xls' and making an RData file conatining this data.                              #
#                                                                                                       #
#   This file is later accessed to by the functions.R file to utilize the termination rates             #
#                                                                                                       #
#########################################################################################################


############################# R COMMAND ##################################
# rm() removes a variable from the workspace. A list of all the variables 
# in the workspace is passed as a parameter to rm to clear all variables 
# in the workspace to ensure that the model uses only the updated values
# sent by the user for computations
##########################################################################

rm(list=ls())

############################ LIBRARY IMPORT ##############################
# XLConnect R library; Provides comprehensive functionality to read, 
# write and format Excel data
# REFERENCE: https://cran.r-project.org/web/packages/XLConnect/XLConnect.pdf
##########################################################################

library(XLConnect) # slow but convenient because it reads ranges



########################### Setting File path and loading workbook ####################################
file_path <- paste0("ExcelData/")
term_rate_input <- "soa_Summary_Tables.xls"
wb<- XLConnect::loadWorkbook(paste0(file_path,term_rate_input))

################## termination rate ###############
term_rate_xl <- readWorksheet(wb, sheet = "Select & Ultimate Table",startRow = 6,startCol = 1,endRow = 49,endCol = 6)
names(term_rate_xl) <- c("Age","Service<2","Service=2,3,4","Service=5-9","Service>=10")


################### Save  data #############################################################
save(term_rate_xl,file = paste0(file_path, "Termination.RData"))
load("E://SUNYALBANY/Pension_Simulation_Project/Code/Data/Termination.Rdata")
