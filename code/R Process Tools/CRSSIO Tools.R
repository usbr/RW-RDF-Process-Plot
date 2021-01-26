# library(devtools)
library(remotes)

#https://github.com/BoulderCodeHub/CoRiverNF - Needs update to generate new NF
#https://github.com/BoulderCodeHub/CRSSIO - Might need update to generate new NF
packageVersion("CoRiverNF")
packageVersion("CRSSIO")

# install_github("BoulderCodeHub/CoRiverNF",ref = "v0.7.0") #update ALL supporting packages (1 + Enter) 
install_github("BoulderCodeHub/CRSSIO") #update ALL supporting packages (1 + Enter) 

package <- "xts"
packageVersion(package)
remotes::install_github("joshuaulrich/xts", ref = "v0.11-2")
# install.packages(package)
library("xts")

install_github("BoulderCodeHub/CRSSIO") #update ALL supporting packages (1 + Enter) 
install_github('rabutler-usbr/CRSSIO',force = T)
##### RESTART R after installing/updating ##########

packageVersion("CRSSIO")

install_github("BoulderCodeHub/RWDataPlyr")
##### RESTART R after installing/updating ##########

library(CRSSIO)
library(RWDataPlyr) # i

#########create CRSS Input Files #### use for CRSS historical DNF 
CRSSDIR <- Sys.getenv("CRSS_DIR")
dmi_folder_nm <- "DNF"
dmi_folder_nm <- "DNF1931_2018"
dmi_folder_nm <- "DNF1988_2018"
oFolder <- file.path(CRSSDIR,"dmi",dmi_folder_nm) 
yy <- 2000:2019
record_start <- '1906-1'
record_start <- '1931-1'
record_start <- '1988-1'

#makes each folder then creates trace files in each
lapply(yy, function(x) {
  tmp <- file.path(oFolder, paste0("NFSinput_", x,"01"))
  dir.create(tmp)
  CRSSIO::crssi_create_dnf_files(
    "CoRiverNF",
    oFolder = tmp,
    startYear = x,
    endYear = 2060,
    recordToUse = zoo::as.yearmon(c(record_start,'2018-12')), #zero out for whole record 
    overwriteFiles = T
  )
})





#change the start date of traces
CRSSIO::crssi_change_nf_start_date(folder = "C:/Users/cfelletter/Documents/crss.offc/dmi/NFSinput",
                                   startDate = "2018-1-31",nTrace = 110)

CRSSIO::crssi_change_nf_start_date(folder = "C:/Users/cfelletter/Documents/crss.offc/dmi/NFS_2015_2Lay",
                                   startDate = "2018-1-31",nTrace = 110)




#or use for easy interface 
CRSSIO:::crss_input_addin() #this function is currently not exported 

#other cool functions
CRSSIO::crssi_change_evap_files()
CRSSIO::crssi_create_hist_nf_xlsx()


#### Recreating historical CRSS input files ####
# CRSSIO has a function called crssi_change_nf_file_names() that will do the trick
# And, if you install the latest dev version of CRSSIO, 
#  paste0(nf_gage_abbrv(), "NF.Inflow")
CRSSDIR <- Sys.getenv("CRSS_DIR")
oFolder <- file.path(CRSSDIR,'dmi/DNF/')
oFolder <- file.path(CRSSDIR,'dmi/DNF1931_2018/')


yy <- 2000:2019

for (yr in yy){
  tmp_out <- file.path(oFolder, paste0("NFSinput_", yr,"01"))
  tmp_out <- paste0(tmp_out, "/") #code is using paste() instead of file.path() somewhere in the code, so it was constructing the file path correctly when there is not a trailing slash on the folders. 
  dir.create(tmp_out)#, recursive = T)
  CRSSIO::crssi_create_dnf_files(
    "CoRiverNF",
    oFolder = tmp_out,
    startYear = yr,
    endYear = 2060,
    recordToUse = zoo::as.yearmon(c('1906-1','2018-12')), #zero out for whole record 
    overwriteFiles = T
  )
}


###Name change & moving to a new folder###
#this will NOT move MWD_ICS.SacWYType, #HydrologyParameters.SupplyScenario or 
#Better to create these along with NF using the code above 
yy <- 2000:2019
iFolder <- file.path(CRSSDIR,'dmi/DNF/')
oFolder <- file.path(CRSSDIR,'dmi/DNF_new/')
oldFileNames <- CRSSIO::nf_file_names(version = 4)
newFileNames <- CRSSIO::nf_file_names(version = 5) #

for (yr in yy){
  tmp_in <- file.path(iFolder, paste0("NFSinput_", yr,"01")) #adding / won't work in place of lower 2 lines
  tmp_out <- file.path(oFolder, paste0("NFSinput_", yr,"01"))
  tmp_out <- paste0(tmp_out, "/") #code is using paste() instead of file.path() somewhere in the code, so it was constructing the file path correctly when there is not a trailing slash on the folders. 
  tmp_in <- paste0(tmp_in, "/")
  dir.create(tmp_out)#, recursive = T)
  CRSSIO::crssi_create_dnf_files(
    "CoRiverNF",
    oFolder = tmp_out,
    startYear = yr,
    endYear = 2060,
    recordToUse = zoo::as.yearmon(c('1906-1','2018-12')), #zero out for whole record 
    overwriteFiles = T
  )
  CRSSIO::crssi_change_nf_file_names(tmp_in, tmp_out, 113,oldFileNames, newFileNames)
}

paste0(CRSSIO::nf_gage_abbrv(),"NF.Inflow")
CRSSIO::nf_file_names(version = 4)