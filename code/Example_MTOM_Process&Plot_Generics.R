##############################################################################
#This script creates annual plots of Outflow and PE to compare two CRSS runs

#DEVELOPMENT IS ON GOING ON THIS

#Contents 
## 1. Set Up ##
## 2. User Input ##
## 3. Process Results ## 
## 4. Plot ## 

#   Created by C. Felletter 8/2018
#   Updated by CF on 10/2018 to include logic for adapting for development of
#   multiple plot types in one pdf
##############################################################################

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls()) #clear the enviornment 

library(RWDataPlyr)

## Directory Set Up
# where scenarios are folder are kept
scen_dir <- file.path(getwd(),"scenarios") 
#containing the sub folders for each ensemble

results_dir <- file.path(getwd(),"results") 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

############# RDF File Loactions - Change For New Study ########################

#list scenarios folders in scen_dir
list.dirs(scen_dir) #list dirs in set folder folder for us in next input

#scens you want to compare, list as "your plot name" = "folder name"
scens <- list(
  "PreviousRun" = "PreviousRun",
  "CurrentRun" = "CurrentRun"
)

#list rdf files in dir
list.files(file.path(scen_dir,scens[1])) #list files in scen folder for next input

############# Specify Plots ########################

# Process Variables ##
rdffiles <- c("Res.rdf","Res.rdf","Res.rdf") #rdf file with slot you want
variables <- c("Powell.Inflow","Powell.Pool Elevation","Mead.Inflow") #RW Object.Slot
timesteps <- c("monthly","annual","annual") #"annual" or "monthly" or "daily"
# #WARNING: Daily Plots are still under development and all results should be cross checked. 
# #Avoid using figuretypes 2 is Bxplt of Traces & 3 is Exceedance until further developement is complete. 
# #https://github.com/usbr/RW-RDF-Process-Plot/issues/17
floworpes <- c("flow","pe","flow") #"flow" or "pe"
cyorwys <- c("cy","cy","wy") #"cy" or "wy". wy not tested for all plot configurations
mainScenGroup <<- names(scens)[2] #name of the subfolder this analysis will be stored
## Plot Variables ##
combineplots <<- F #F for individual files saved, true to combineplots multiple file
#Note: creating a series of seperate files
figuretypes <- c(1,2,3) #1 is Trace Mean, 2 is Bxplt of Traces, 3 is Exceedance
exc_months <- c(NA,NA,9) #1 - Jan, 12 - Dec rep(NA,times = length(variables))
#Note: must specify exc_month for Exceedance, since we want wy this is 9/sept
startyrs <<- rep(2019,times = length(variables)) #c(2019,2019,2019,2019,2019) #filter out all years > this year
endyrs <<- rep(2022,times = length(variables)) #c(2019,2026,2026,2026) #filter out all years > this year
#Note: start year same as end year for Daily
customcaptions <-  c(NA,NA,"Inflow Exceedance") #NA or this will over write the default caption on boxplots rep(NA,times = length(variables))
custom_y_labs <- c(NA,NA,"Inflow Exceedance") #NA gives defaults, enter if want soemthing different rep(NA,times = length(variables))
# Note: use of custom caption and labels
figname <<- "ExampleFigs"
model <<- "MTOM" #"CRSS" or "MTOM"
first_ensemble <<- c(4,4) #filter out Most,Min,Max. For 38 trace offical = 4,
minmaxchk <- T #only works for monthly currently (12/6/18)

############# Standard Plot Variables - Don't Change ########################

#output image parameters 
width <<- 9 #inches
height <<- 6

imgtype <<- "png" #supports pdf, png, jpeg. pdf looks the best 
#only works when individual plots are selected 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                               END USER INPUT
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source('code/Generic_Input_Check.r')
source('code/Generic_Scen_Process.r')
source('code/generic_minmax_check.r')
source('code/Generic_Daily_Plot.r')
source('code/Generic_annual_plot.r')
source('code/Generic_monthly_plot.r')

ofigs <- file.path(results_dir,mainScenGroup) 
if (!file.exists(ofigs)) {
  message(paste('Creating folder:', ofigs))
  dir.create(ofigs)
}

message('Figures will be saved to: ', ofigs)

if(combineplots == T){
  pdf(file.path(ofigs,paste0(figname,".pdf")), width=width, height=height)
}

#this could be used to loop through multiple plots 
for(i in 1:length(variables)){
  ## Process Variables ##
  rdffile <<- rdffiles[i] 
  variable <<- variables[i]
  floworpe <<- floworpes[i]
  cyorwy <<- cyorwys[i]
  timestep <<- timesteps[i]
  ## Plot Variables ##
  figuretype <<- figuretypes[i]
  exc_month <<- exc_months[i]
  startyr <<- startyrs[i] 
  endyr <<- endyrs[i]
  customcaption <<- customcaptions[i]
  custom_y_lab <<- custom_y_labs[i]
  
  # some sanity checks that UI is correct:
  generic_input_check(scen_dir,scens,timestep) 
  
  vars <- list(rdffiles,variables,floworpes,cyorwys,timesteps,figuretypes,
               exc_months,startyrs,endyrs,customcaptions,custom_y_labs)
  # check to make sure
  if(length(unique(unlist(lapply(X = vars,FUN = length)))) > 1){ 
    stop('variables must all be the same length')
  }
  
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 3. Process Results 
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  scen_res <- generic_scen_process(scen_dir,scens,timestep) 
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 4. Plot Choosen Timestep Type 
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
  
  if(timestep == "annual"){
    generic_annual_plot(scen_res)
  } else if(timestep == "monthly"){
    
    if(timestep == "monthly" && minmaxchk == T){ #only works for monthly currently (12/6/18)
      #check minmax, outputs a .csv
      minmaxchk <- generic_minmax_check(scen_dir,scens,timestep) 
    }
    
    generic_monthly_plot(scen_res) 
  } else if(timestep == "daily"){
    generic_daily_plot(scen_res)
  } else {
    stop(paste0("Plot type ",timestep," not supported"))
  }

} #close process and plot loop i 

if(combineplots == T){
  dev.off()
}
