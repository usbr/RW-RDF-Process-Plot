# library(devtools)
library(remotes)
#https://github.com/BoulderCodeHub/CoRiverNF - Needs update to generate new NF
#https://github.com/BoulderCodeHub/CRSSIO - Might need update to generate new NF
packageVersion("CoRiverNF")
packageVersion("CRSSIO")
# package <- "xts"
# packageVersion(package)
# remotes::install_github("joshuaulrich/xts", ref = "v0.11-2")
# # install.packages(package)
# library("xts")
# install_github("BoulderCodeHub/CRSSIO") #update ALL supporting packages (1 + Enter) 
# install_github('rabutler-usbr/CRSSIO',force = T)
# install_github("BoulderCodeHub/RWDataPlyr")
##### RESTART R after installing/updating ##########
library(CRSSIO)
library(RWDataPlyr) # i

#########create Single set of trace files################################

# CRSSDIR <- Sys.getenv("CRSS_DIR")
# CRSSDIR = "C://Users//cfelletter//Documents//CRSS"
CRSSDIR = "C://Users//cfelletter//Documents//crss.2023TRIRVW"
CRSSDIR #check CAREFUL!!!!
dmi_folder_nm <- "StressTest"
oFolder <- file.path(CRSSDIR,"dmi",dmi_folder_nm) 
record_start <- '1988-1'
nf_file <- 'NaturalFlows1906-2020_2023USGSLBNoPhreat.xlsx'  
# nf_file <- 'NaturalFlows1906-2020_USGSLB_WITHPhreat.xlsx'
iFile <- file.path('C:/Users/cfelletter/Documents/natflowsaltmodel/results',nf_file)
# iFile <- file.path('M:/Shared/CRSS/2021/results_dev/PowellElVol_NewInactCap',nf_file)
# dir.create(oFolder)
CRSSIO::crssi_create_dnf_files(
  iFile,
  oFolder = oFolder,
  startYear = 2023,
  endYear = 2055, ## for ST don't forget to update this a year!!! 
  recordToUse = zoo::as.yearmon(c(record_start,'2020-12')), #zero out for whole record
  overwriteFiles = T
)

dmi_folder_nm <- "NFSinput"
oFolder <- file.path(CRSSDIR,"dmi",dmi_folder_nm) 
record_start <- '1906-1'
nf_file <- 'NaturalFlows1906-2020_20221115.xlsx'  #'NaturalFlows1906-2019_NvjE-V_20211026.xlsx'
iFile <- file.path('C:/Users/cfelletter/Documents/natflowsaltmodel/results',nf_file)
# iFile <- file.path('M:/Shared/CRSS/2021/results_dev/PowellElVol_NewInactCap',nf_file)
# dir.create(oFolder)
CRSSIO::crssi_create_dnf_files(
  iFile,
  oFolder = oFolder,
  startYear = 2023,
  endYear = 2070,
  recordToUse = zoo::as.yearmon(c(record_start,'2020-12')), #zero out for whole record
  overwriteFiles = T
)


#########create CRSS Input Files #### use for CRSS historical DNF 
CRSSDIR <- Sys.getenv("CRSS_DIR")
dmi_folder_nm <- "DNF"
dmi_folder_nm <- "DNF1931_2019"
dmi_folder_nm <- "DNF1988_2019"
oFolder <- file.path(CRSSDIR,"dmi",dmi_folder_nm) 
yy <- 2000:2019
record_start <- '1906-1'
record_start <- '1931-1'
record_start <- '1988-1'

#############
#Single set of trace files 
tmp <- file.path(oFolder, paste0("NFSinput_", x,"01"))
tmp <- "C:\\Users\\cfelletter\\Documents\\crss.2023TRIRVW/dmi/Test"
dir.create(tmp)
CRSSIO::crssi_create_dnf_files(
  "CoRiverNF",
  oFolder = tmp,
  startYear = 2023,
  endYear = 2053,
  recordToUse = zoo::as.yearmon(c(record_start,'2018-12')), #zero out for whole record
  overwriteFiles = T
)

#############
## cont'd #######create CRSS Input Files #### use for CRSS historical DNF 

#makes each folder then creates trace files in each
lapply(yy, function(x) {
  tmp <- file.path(oFolder, paste0("NFSinput_", x,"01"))
  dir.create(tmp)
  CRSSIO::crssi_create_dnf_files(
    "CoRiverNF",
    oFolder = tmp,
    startYear = x,
    endYear = 2030,
    recordToUse = zoo::as.yearmon(c(record_start,'2019-12')), #zero out for whole record 
    overwriteFiles = T
  )
})

#change the start date of traces################################
CRSSIO::crssi_change_nf_start_date(folder = "C:/Users/cfelletter/Documents/CRSS/dmi/Verification2000",
                                   start_year = 2000,nTrace = 1)

CRSSIO::crssi_change_nf_start_date(folder = "C:/Users/cfelletter/Documents/crss.2023TRIRVW/dmi/NFSinput_2018_binational_CRSSv5",
                                   start_year = 2023,nTrace = 88)

#or use for easy interface ################################
CRSSIO:::crss_input_addin() #this function is currently not exported 

#other cool functions################################
CRSSIO::crssi_change_evap_files()
CRSSIO::crssi_create_hist_nf_xlsx()


# #### Recreating historical CRSS input files ####################################
# # CRSSIO has a function called crssi_change_nf_file_names() that will do the trick
# # And, if you install the latest dev version of CRSSIO, 
# #  paste0(nf_gage_abbrv(), "NF.Inflow")
# CRSSDIR <- Sys.getenv("CRSS_DIR")
# oFolder <- file.path(CRSSDIR,'dmi/DNF/')
# oFolder <- file.path(CRSSDIR,'dmi/DNF1931_2018/')
# 
# 
# yy <- 2000:2019
# 
# for (yr in yy){
#   tmp_out <- file.path(oFolder, paste0("NFSinput_", yr,"01"))
#   tmp_out <- paste0(tmp_out, "/") #code is using paste() instead of file.path() somewhere in the code, so it was constructing the file path correctly when there is not a trailing slash on the folders. 
#   dir.create(tmp_out)#, recursive = T)
#   CRSSIO::crssi_create_dnf_files(
#     "CoRiverNF",
#     oFolder = tmp_out,
#     startYear = yr,
#     endYear = 2060,
#     recordToUse = zoo::as.yearmon(c('1906-1','2018-12')), #zero out for whole record 
#     overwriteFiles = T
#   )
# }
# 

# ###Name change & moving to a new folder###################################
# #this will NOT move MWD_ICS.SacWYType, #HydrologyParameters.SupplyScenario or 
# #Better to create these along with NF using the code above 
# yy <- 2000:2019
# iFolder <- file.path(CRSSDIR,'dmi/DNF/')
# oFolder <- file.path(CRSSDIR,'dmi/DNF_new/')
# oldFileNames <- CRSSIO::nf_file_names(version = 4)
# newFileNames <- CRSSIO::nf_file_names(version = 5) #
# 
# for (yr in yy){
#   tmp_in <- file.path(iFolder, paste0("NFSinput_", yr,"01")) #adding / won't work in place of lower 2 lines
#   tmp_out <- file.path(oFolder, paste0("NFSinput_", yr,"01"))
#   tmp_out <- paste0(tmp_out, "/") #code is using paste() instead of file.path() somewhere in the code, so it was constructing the file path correctly when there is not a trailing slash on the folders. 
#   tmp_in <- paste0(tmp_in, "/")
#   dir.create(tmp_out)#, recursive = T)
#   CRSSIO::crssi_create_dnf_files(
#     "CoRiverNF",
#     oFolder = tmp_out,
#     startYear = yr,
#     endYear = 2060,
#     recordToUse = zoo::as.yearmon(c('1906-1','2018-12')), #zero out for whole record 
#     overwriteFiles = T
#   )
#   CRSSIO::crssi_change_nf_file_names(tmp_in, tmp_out, 113,oldFileNames, newFileNames)
# }


### Change CRSS v4 NFS inputs to v5 names ###################################
# iFolder<-"C:/Users/cfelletter/Documents/NatSalt_RCode/RWfiles"
iFolder<-"C:/Users/cfelletter/Documents/crss.trirvw2020/dmi/NFSinput_2018_binational"
oFolder <-"C:/Users/cfelletter/Documents/crss.2023TRIRVW/dmi/NFSinput_2018_DirectRegressabvPwll"
oldFileNames <- c(CRSSIO::nf_file_names(version = 4),CRSSIO::natsalt_file_names(version = 4))
newFileNames <- c(CRSSIO::nf_file_names(version = 5),CRSSIO::natsalt_file_names(version = 5))
CRSSIO::crssi_change_nf_file_names(iFolder, oFolder, 88,oldFileNames, newFileNames)
