# library('tidyverse') #ggplot2,dplyr,tidyr
# library('devtools')
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

#### Normally You'll Only Change The Below ####
#scens you want to compare, list as "your plot name" = "folder name"
scens <- list(
  "Apr2021_2022" = "Apr2021_2022,ISM1988_2019,2016Dems,IG_DCP,MTOM_Most", #what I compared in my first anlysis
  "CRSS.V5.1.3.2022.Apr2021" = "CRSS.V5.1.3.2022.Apr2021,ISM1988_2019,2016Dems,IG_DCP,MTOM_Most" #Verify 
)

#list rdf files in dir
list.files(file.path(scen_dir,scens[1])) 

#SHOULD only be one so only slots in that rdf are defined in variables
## could revise this into a list of slots which are associated with multiple variables 
rdffile <- c("KeySlots.rdf") #rdf file with slot you want
rdffile <- c("Res.rdf") #rdf file with slot you want


# #list slots in rdf
rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],rdffile[1])))

variable <- rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],rdffile[1])))[10]
# variable <- c("BlueMesa.Outflow","FlamingGorge.Outflow") #RW Object.Slot
variable <- c("Powell.Pool Elevation") #keyslots.rdf
variable <- c("Mead.Outflow") #keyslots.rdf

variable <- c("Powell.Inflow") #RW Object.Slot


i=1
# for (i in 1:length(rdffile)) {
  
  # 
  ub_old <- read_rdf(iFile = file.path(scen_dir,scens[1],rdffile[i]))
  ub_new <- read_rdf(iFile = file.path(scen_dir,scens[2],rdffile[i]))
  
  for (j in 1:length(variable)) {
    #alan's diffing slot script 
    diffM <- rdf_get_slot(ub_old, variable[j]) - rdf_get_slot(ub_new, variable[j])
    
    plot(apply(abs(diffM), 2, max), type = 'h')
    
    trace <- 61
    
    plot(diffM[,trace], type = 'h')
    
  }
  
# }

