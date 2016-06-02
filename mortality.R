#########################################################################################################
#########################################################################################################
#                                                                                                       #
#                                          DESCRIPTION                                                  #
#   mortality.R contains code for extracting data from an excel sheet titled                            #
#  'mortality-reference-table' and making an RData file conatining this data.                           #
#                                                                                                       #
#   This file is later accessed to by the functions.R file to utilize the mortality rates               #
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

# character to numeric, eliminating "," "$" "%". chars will become NA
cton <- function (cvar) as.numeric(gsub("[ ,$%]", "", cvar))

########################### Setting File path and loading workbook ####################################
file_path <- paste0("ExcelData/")
mort_tables_input <- "mortality-reference-table.xlsx"
wb<- XLConnect::loadWorkbook(paste0(file_path,mort_tables_input))

################## Mortality tables ###############
mort_tables <- readWorksheet(wb, sheet = "Sheet1")
#mort_tables <- mort_tables %>%mutate_each(funs(cton))
names(mort_tables) <- c("Age", "RP2014_Employee_total","RP2014_Annuitant_total","RP2000_Employee_total","RP2000_Annuitant_total","RP2010_Employee_total","RP2010_Annuitant_total")
mort_tables<-subset(mort_tables, Age<=100)


################### Save  data #############################################################
save(mort_tables,file = paste0(file_path, "Mortality.RData"))

