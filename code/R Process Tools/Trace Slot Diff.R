library(RWDataPlyr)

## Directory Set Up
#Set folder where studies are kept as sub folders. 
CRSSDIR <- Sys.getenv("CRSS_DIR")

# where scenarios are folder are kept
scen_dir <- file.path(CRSSDIR,"Scenario") 
#containing the sub folders for each ensemble

results_dir <- file.path(CRSSDIR,"results") 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#list scenarios folders in scen_dir
list.dirs(scen_dir) #list dirs in set folder folder for us in next input

#scens you want to compare, list as "your plot name" = "folder name"
scens <- list(
  "RW902_mdl9006" = "CRSS.V6.0.1.2024.Mar2023.9006,CMIP3,2016Dems,IGDCPnoUBDRO.v6.0.1.9005,CRMMS_Most",
  "RW905_mdl9007" = "CRSS.V6.0.1.2024.Mar2023.9007,CMIP3,2016Dems,IGDCPnoUBDRO.v6.0.1.9006,CRMMS_Most"
# "RW902_mdl9006" = "CRSS.V6.0.1.2024.Mar2023.9006,ISM1988_2020,2016Dems,IGDCPnoUBDRO.v6.0.1.9005,CRMMS_Most",
# "RW905_mdl9007" = "CRSS.V6.0.1.2024.Mar2023.9007,ISM1988_2020,2016Dems,IGDCPnoUBDRO.v6.0.1.9006,CRMMS_Most"
)

#list rdf files in dir
list.files(file.path(scen_dir,scens[1])) 

## could revise this into a list of slots which are associated with multiple variables 
rdffile <- c("KeySlots.rdf") #rdf file with slot you want
# rdffile <- c("Res.rdf") #rdf file with slot you want

# #list slots in rdf
# rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],rdffile[1])))

variable <- rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],rdffile[1])))[10]
# variable <- c("BlueMesa.Outflow","FlamingGorge.Outflow") #RW Object.Slot
variable <- c("Powell.Pool Elevation") #keyslots.rdf
variable <- c("Mead.Outflow") #keyslots.rdf

variable <- c("Mead.Outflow") #keyslots.rdf


i=1
# for (i in 1:length(rdffile)) {
  # 

  ub_old <- read_rdf(iFile = file.path(scen_dir,scens[1],rdffile[i]))
  ub_new <- read_rdf(iFile = file.path(scen_dir,scens[2],rdffile[i]))

  all.equal(ub_old,ub_new) # just do this, no need to go through the below unless there are diffs
  
  
  
  # for (j in 1:length(variable)) {
  #   #alan's diffing slot script 
  j=1
    diffM <- rdf_get_slot(ub_old, variable[j]) - rdf_get_slot(ub_new, variable[j])
    which(diffM != 0)
  #   
  #   plot(apply(abs(diffM), 2, max), type = 'h')
  #   
  #   trace <- 61
  #   
  #   plot(diffM[,trace], type = 'h')
  #   
  # }
  # 
# }

    variable <- c("Powell.Pool Elevation") #keyslots.rdf
    
    all.equal(rdf_get_slot(ub_old, variable[j]),rdf_get_slot(ub_new, variable[j])) # just do this, no need to go through the below unless there are diffs
    
    
    variable <- c("Mead.Pool Elevation") #keyslots.rdf
    
    all.equal(rdf_get_slot(ub_old, variable[j]),rdf_get_slot(ub_new, variable[j])) # just do this, no need to go through the below unless there are diffs
    