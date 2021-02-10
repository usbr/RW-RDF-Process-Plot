########################
#Moving certain files and maintain their sub folders  
#######################

# identify the folders
## Manoa to MyPC
# current.folder <- "Z:/Shared/MTOM_CRSS_5-yrComparison/Analysis/data/Scenario_CRSS_9015" 
current.folder <- "Z:/felletter/CRSS_MTOM_scens/Full" #or delete this 
new.folder <- "C:/Users/cfelletter/Documents/CRSS_MTOM/testbedanalysis/data/Scenario"
## BA to Manoa
current.folder <- "C:/Users/fellette/Documents/GIT/CRSS/Scenario" #BA
new.folder <- "//128.138.214.42/bor/felletter/GREAT_Test/" #Manoa

# # create the folders if needed  
folders.needed <- list.files(current.folder)
setwd(new.folder)

for (dir_check in folders.needed) {
  dir.create(file.path(dir_check), showWarnings = FALSE)
}

# find the files that you want
## CRSS-MTOM files
rdf <- c("SystemConditions","Res","UBRes","xtraRes")  #"UBRch.rdf"

## required for Process CRSS tool 
rdf <- c("KeySlots","Check","MPPE","MWDICS","SystemConditions","Res","CSD_ann","CSD_mon")  

## GREAT files
rdf <- c("DailyFlows","Res","UBRes","UBDO","Drought")  #"UBRch.rdf"

for (i in 1:length(rdf)) {
  list.of.files <- list.files(current.folder, rdf[i],recursive = TRUE)
  # list.of.files
  # copy the files to the new folder
  from_files <- file.path(current.folder, list.of.files)
  to_files <- file.path(new.folder, list.of.files)
  file.copy(from_files, to_files,overwrite = T)
  print(paste("Copied",rdf[i]))
}



# ########################
# #Renaming folders with files in them - won't work for scenario names with . 
# #######################

# current.folder <- "C:/Users/cfelletter/Documents/CRSS_MTOM/testbedanalysis/data/Test"
list.files(current.folder,full.names = TRUE)

current.folder <- "Z:/felletter/CRSS_MTOM_scens/Stress" #or delete this
setwd(current.folder)
runrange = 2000:2018

file.rename(sprintf("NF_%s_%s,2016Dems_v1.6,CRSS_VerificationModel_9015,IGDCP.v4.4.0.9004", "Full",runrange),     # to
            sprintf("NF_%s_%s,2016Dems_v1.6,CRSS_VerificationModel_9015,IGDCP.v4.4.0.9004", "StressTest",runrange))
            # sprintf("NF_%s_%s,2016Dems_v1.6,CRSS_VerificationModel_9015,IGDCP.v4.4.0.9004", "pluvialRem",runrange))
