########################## winklevoss Import ##########################################
# This program imports the following data for the actuarial valuation model
# Decrement tables

rm(list=ls())

library(XLConnect) # slow but convenient because it reads ranges

cton <- function (cvar) as.numeric(gsub("[ ,$%]", "", cvar))  # character to numeric, eliminating "," "$" "%". chars will become NA


########################### Setting File path and loading workbook ####################################
file_path <- paste0("E://SUNYALBANY/Pension_Simulation_Project/Code/Data/")
mort_tables_input <- "simulation Aug30_original.xlsm"
wb<- XLConnect::loadWorkbook(paste0(file_path,mort_tables_input))

################## Mortality tables ###############
mort_tables <- readWorksheet(wb, sheet = "Mortality tables")
mort_tables <- mort_tables %>%mutate_each(funs(cton))
names(mort_tables) <- c("Age", "Male RP-2000 Rates","Female RP-2000 Rates","RP-2000 average","Male RP-2000 10 years projection","Female RP-2000 10 years projection","2010 average","Male RP-20114","Female RP-2014","RP-2014 average","GAM-1994 Male","GAM-1994 Female","GAM-1994 average")


################### Save  data #############################################################
save(mort_tables,file = paste0(file_path, "Mortality.RData"))
