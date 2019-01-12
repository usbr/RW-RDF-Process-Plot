# library('tidyverse') #ggplot2,dplyr,tidyr
# library('devtools')
library(RWDataPlyr)

## Directory Set Up
#Set folder where studies are kept as sub folders. 

#should I impliment the below in Process&Plot and eliminate the need for 
#two different Generic Processes?

model <- "MTOM" #"CRSS" or "MTOM"

model <- "CRSS" #"CRSS" or "MTOM"

if (model == "MTOM"){
  
  scen_dir = file.path(getwd(),"scenarios") 
  #containing the sub folders for each ensemble
  
  scens <- list(
    "PreviousRun" = "PreviousRun",
    "CurrentRun" = "CurrentRun"
  )
  
  results_dir <- file.path(getwd(),"results") 
  
} else if (model == "CRSS"){
  
  CRSSDIR <- Sys.getenv("CRSS_DIR")
  
  # where scenarios are folder are kept
  scen_dir <- file.path(CRSSDIR,"Scenario")
  
  # scens <- list( ########update every time##########
  #                "Aug 2018 9000" = "Aug2018_2019_9000,DNF,2007Dems,IG_9000,Most_BM_FGltsp",
  #                "Aug 2018 9002 (+ CC Fix)" = "Aug2018_2019_v2.8.0.9002,DNF,2007Dems,IG_v2.7.0.9002,Most" #Verify
  # )
  
  #CC compare 
  scens <- list( ########update every time##########
                 "9600" = "Aug2018_2019_v2.8.0.9600,VIC,2007Dems_ScheduleSlots,IG_v2.7.0.9600,Most",
                 "9700 + Fix" = "Aug2018_2019_v2.8.0.9700,VIC,2007Dems_ScheduleSlots,IG_v2.7.0.9700,Most" #Verify
  )
  
  results_dir <- file.path(CRSSDIR,"results")
  
} else {stop("Set Model")}


figname <<- F

ofigs <- file.path(results_dir,mainScenGroup) 
if (!file.exists(ofigs)) {
  message(paste('Creating folder:', ofigs))
  dir.create(ofigs)
}

width <<- 9 #inches
height <<- 6

imgtype <<- "pdf" #supports pdf, png, jpeg. pdf looks the best 
  
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#list scenarios folders in scen_dir
list.dirs(scen_dir) #list dirs in set folder folder for us in next input

# #### Normally You'll Only Change The Below ####
# #scens you want to compare, list as "your plot name" = "folder name"
# scens <- list(
#   "PreviousRun" = "PreviousRun",
#   "CurrentRun" = "CurrentRun"
# )

#list rdf files in dir
list.files(file.path(scen_dir,scens[1])) 

rdffiles <- c("UBRes.rdf") #rdf file with slot you want
# rdffiles <- c("Res.rdf") #rdf file with slot you want

# #list slots in rdf
rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],rdffiles[1])))
# 
variables <- c("FlamingGorge.Pool Elevation","FlamingGorge.Outflow") #RW Object.Slot
# variables <- c("Powell.Pool Elevation") #RW Object.Slot

# trace_to_plot <- c(66,66) #length(trace_to_plot) must be equal to length(variables)
trace_to_plot <- c(NA,NA) #use defualt max diff traces 

timestartstop <-c(NA,NA) #use whole record
timestartstop <-c(1,496) #drop last 8 months for FG


ub_old <- read_rdf(iFile = file.path(scen_dir,scens[1],rdffiles[1]))
ub_new <- read_rdf(iFile = file.path(scen_dir,scens[2],rdffiles[1]))

if(saveplots == T){
  pdf(file.path(ofigs,paste0(figname,".pdf")), width=width, height=height)
}

#this could be used to loop through multiple plots 
for(i in 1:length(variables)){
  
#alan's diffing slot script 
  diffM <- rdf_get_slot(ub_old, variables[i]) - rdf_get_slot(ub_new, variables[i])
  
  if(!is.na(timestartstop[1])){
    diffM <- diffM[c(timestartstop[1]:timestartstop[2]),] #drop last 8 months for FG
  }
    
  plot(apply(abs(diffM), 2, max), type = 'h', main = paste("Diff",variables[i]),xlab = "Trace",ylab = variables[i])
  
  maxtrace <- which.max(apply(diffM, 2, max)) #54
  message(paste("Max diff trace",maxtrace,"of",apply(diffM, 2, max)[maxtrace],"in",variables[i]))
  mintrace <- which.min(apply(diffM, 2, min)) #31
  message(paste("Min diff trace",mintrace,"of",apply(diffM, 2, min)[mintrace],"in",variables[i]))
  
  if(which.max(c(apply(diffM, 2, max)[maxtrace],apply(diffM, 2, min)[mintrace])) == 1){
    plot(diffM[,maxtrace], type = 'h', main = paste("Diff",variables[2],"trace",maxtrace),xlab = "Timestep",ylab = variables[i])
  } else {
    plot(diffM[,mintrace], type = 'h', main = paste("Diff",variables[2],"trace",mintrace),xlab = "Timestep",ylab = variables[i])
  }
  
  #OR just user specified 
  if(!is.na(trace_to_plot[i])){
    trace <- trace_to_plot[i] #trace your interested in plotting 
    
    plot(diffM[,trace], type = 'h', main = paste(variables[2],"trace",trace),xlab = "Timestep",ylab = variables[i])
    
    
  }
}

dev.off()
