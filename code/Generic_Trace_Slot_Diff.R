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
  
  scens <- list(
    "woMin10wMaxDivSalt" = "woMin10wMaxDivSalt",
    "9004wConverg" = "9004wConverg" 
  )
  
  results_dir <- file.path(CRSSDIR,"results")
  
} else {stop("Set Model")}

mainScenGroup <<- names(scens)[2] #name of the subfolder this analysis will be stored

figname <<- "SlotDiffFigs"

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

#list rdf files in dir
# list.files(file.path(scen_dir,scens[1])) 

rdffiles <- c("UBRes.rdf") #rdf file with slot you want

# #list slots in rdf
rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],rdffiles[1])))
# 
variables <- c("FlamingGorge.Pool Elevation","FlamingGorge.Outflow") #RW Object.Slot
# variables <- c("Powell.Pool Elevation") #RW Object.Slot

# # Repeat rdf file name if all variables are in same rdf 
# rdffiles <- rep(x = "UBRch.rdf" ,times = length(variables))

# trace_to_plot <- c(66,66) #length(trace_to_plot) must be equal to length(variables)
trace_to_plot <- c(NA,NA) #use defualt max diff traces 

timestartstop <-c(NA,NA) #use whole record 
# timestartstop <-c(1,496) #drop last 8 months for FG

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
  # message(paste("Max diff trace",maxtrace,"of",apply(diffM, 2, max)[maxtrace],"in",variables[i]))
  mintrace <- which.min(apply(diffM, 2, min)) #31
  # message(paste("Min diff trace",mintrace,"of",apply(diffM, 2, min)[mintrace],"in",variables[i]))
  
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
  
  if(i==1){
    data <- data.frame("Slot" = variables[i], "Max_diff_trace" = maxtrace,"Max_diff" = apply(diffM, 2, max)[maxtrace],
                       "Min_diff_trace" = mintrace, "Min_diff" = apply(diffM, 2, min)[mintrace])
  } else{
    data <- rbind.data.frame(data,data.frame("Slot" = variables[i], "Max_diff_trace" = maxtrace,"Max_diff" = apply(diffM, 2, max)[maxtrace],
                       "Min_diff_trace" = mintrace, "Min_diff" = apply(diffM, 2, min)[mintrace])
    )
  }
  
}

write.csv(data,file = paste0(ofigs,'/SlotDiff.csv'))


dev.off()
