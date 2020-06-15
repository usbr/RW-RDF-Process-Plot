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

#  Updated by CF on 6/15/2020 to use Generic_Scen_Process_2.R which is built to 
# process multiple slots in the same rdf. NOT setup to handle more than two rdf files
# NOT tested for daily flow 
##############################################################################

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls()) #clear the enviornment 

library(RWDataPlyr)

## Directory Set Up
#Set folder where studies are kept as sub folders. 
CRSSDIR <- Sys.getenv("CRSS_DIR")

# where scenarios are folder are kept
scen_dir <- file.path(CRSSDIR,"Scenario") 
#containing the sub folders for each ensemble

results_dir <- file.path(CRSSDIR,"results") 

model <- "CRSS" 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#list scenarios folders in scen_dir
# list.dirs(scen_dir) #list dirs in set folder folder for us in next input

#### Normally You'll Only Change The Below ####
#scens you want to compare, list as "your plot name" = "folder name"
scens <- list(
  "Aug 2018" = "2007Dems,DNF,9001,IG v 2.7 9000",
  "Aug 2018 + MaxDivSalt" = "2007Dems,DNF,9002 wMaxDivSalt,IG v 2.7 9000"
)

# #list rdf files in dir
# list.files(file.path(scen_dir,scens[1])) #list files in scen folder for next input
############# Standard Plot Variables - Don't Change ########################

#output image parameters 
width <<- 9 #inches
height <<- 6

imgtype <<- "pdf" #supports pdf, png, jpeg. pdf looks the best 
#only works when individual plots are selected 

############# Plot Type Variables - Change For New Plot ########################

#files, variables, floworpes, cyorwys, figuretypes, exc_months (if using exceed
# on PE), & custom captions/y_lab should be set to a single value
#but could be used to loop through multiple plots if all c() variable were extended 
#see example below 

## Process Variables ##

# # WARNING: Generic_Scen_Process_2.R NOT tested for daily flow 
# #WARNING: Daily Plots are still under development and all results should be cross checked. 
# #Avoid using figuretypes 2 is Bxplt of Traces & 3 is Exceedance until further developement is complete. 
# #https://github.com/usbr/RW-RDF-Process-Plot/issues/17

############# Example of Multi Slot Process ########################

#Example 4: Standard Powell-Mead Annual Ops
# Process Variables ##
rdffiles <- c("Res.rdf","Res.rdf","Res.rdf","Res.rdf","Res.rdf") #rdf file with slot you want
variables <- c("Powell.Inflow","Powell.Pool Elevation","Mead.Inflow","Mead.Pool Elevation","Mead.Outflow") #RW Object.Slot
timesteps <- c("annual","annual","annual","annual","annual")
floworpes <- c("flow","pe","flow","pe","flow") #"flow" or "pe"
cyorwys <- c("cy","cy","cy","cy","cy") #"cy" or "wy". wy not tested for all plot configurations
mainScenGroup <<- names(scens)[2] #name of the subfolder this analysis will be stored
## Plot Variables ##
combineplots <<- T #F for individual files saved, true to combineplots multiple file
#Note: creating a series of seperate files
figuretypes <- c(2,2,2,2,2) #1 is Trace Mean, 2 is Bxplt of Traces, 3 is Exceedance
exc_months <- rep(NA,times = length(variables)) #c(NA,NA,NA,NA,NA) #1 - Jan, 12 - Dec
#Note: must specify exc_month for Exceedance, since we want wy this is 9/sept
startyrs <<- rep(2019,times = length(variables)) #c(2019,2019,2019,2019,2019) #filter out all years > this year
endyrs <<- rep(2026,times = length(variables)) #c(2019,2026,2026,2026) #filter out all years > this year
#Note: start year same as end year for Daily
customcaptions <- rep(NA,times = length(variables)) #c(NA,NA,"Inflow Exceedance",NA) #NA or this will over write the default caption on boxplots
custom_y_labs <- rep(NA,times = length(variables)) #c(NA,NA,"Inflow Exceedance",NA) #NA gives defaults, enter if want soemthing different
# Note: use of custom caption and labels
figname <<- paste0('Example_3_Std_PowellMead_Fig',startyrs[1],endyrs[1])
model <<- "CRSS" #"CRSS" or "MTOM"
minmaxchk <- T #only works for monthly currently (12/6/18)

# Annual UB Plots ## USE Repeats ##
variables <<- c("Fontenelle.Inflow","Fontenelle.Pool Elevation","Fontenelle.Outflow","FlamingGorge.Pool Elevation","FlamingGorge.Outflow","TaylorPark.Inflow","BlueMesa.Pool Elevation","BlueMesa.Outflow","Crystal.Pool Elevation","Crystal.Outflow","Navajo.Inflow","Navajo.Pool Elevation","Navajo.Outflow") #RW Object.Slot
rdffiles <- rep("UBres.rdf",times = length(variables))  #rdf file with slot you want
timesteps <- rep("annual",times = length(variables))
floworpes <- c("flow","pe","flow","pe","flow","flow","pe","flow","pe","flow","flow","pe","flow") #"flow" or "pe"
cyorwys <- rep("cy",times = length(variables))  #"cy" or "wy". wy not tested for all plot configurations
mainScenGroup <<- names(scens)[2] #name of the subfolder this analysis will be stored
combineplots <<- T #F for individual files saved, true to combineplots multiple file
figuretypes <- rep(2,times = length(variables))  #1 is Trace Mean, 2 is Bxplt of Traces, 3 is Exceedance
exc_months <- rep(NA,times = length(variables)) #c(NA,NA,NA,NA,NA) #1 - Jan, 12 - Dec
startyrs <<- rep(2045,times = length(variables)) #c(2019,2019,2019,2019,2019) #filter out all years > this year
endyrs <<- rep(2060,times = length(variables)) #c(2019,2026,2026,2026) #filter out all years > this year
customcaptions <- rep(NA,times = length(variables)) #c(NA,NA,"Inflow Exceedance",NA) #NA or this will over write the default caption on boxplots
custom_y_labs <- rep(NA,times = length(variables)) #c(NA,NA,"Inflow Exceedance",NA) #NA gives defaults, enter if want soemthing different
# Note: use of custom caption and labels
figname <<- paste0('UBRes_PEOut_Annual',startyrs[1],endyrs[1])
minmaxchk <- F #only works for monthly currently (12/6/18)

#### End Examples #### 


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                               END USER INPUT
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source('code/Generic_Input_Check.r')
source('code/Generic_Scen_Process_2.r')
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

# some sanity checks that UI is correct:
generic_input_check(scen_dir,scens,timestep) 

same_rdf <- which(rdffiles == rdffiles[1])
diff_rdf <- which(rdffiles != rdffiles[1])

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

scen_res <- generic_scen_process_2(scen_dir,scens,timestep,same_rdf) 


for (j in same_rdf) {
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 4. Plot Choosen Timestep Type 
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
  variable <<- variables[j]
  floworpe <<- floworpes[j]
  cyorwy <<- cyorwys[j]
  timestep <<- timesteps[j]
  ## Plot Variables ##
  figuretype <<- figuretypes[j]
  exc_month <<- exc_months[j]
  startyr <<- startyrs[j] 
  endyr <<- endyrs[j]
  customcaption <<- customcaptions[j]
  custom_y_lab <<- custom_y_labs[j]
  
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
  
}

if (length(diff_rdf) >= 1) {
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 5. Process and plot second .rdf file  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  scen_res <- generic_scen_process_2(scen_dir,scens,timestep,diff_rdf) 
  
  for (j in diff_rdf) {
    
    variable <<- variables[j]
    floworpe <<- floworpes[j]
    cyorwy <<- cyorwys[j]
    timestep <<- timesteps[j]
    ## Plot Variables ##
    figuretype <<- figuretypes[j]
    exc_month <<- exc_months[j]
    startyr <<- startyrs[j] 
    endyr <<- endyrs[j]
    customcaption <<- customcaptions[j]
    custom_y_lab <<- custom_y_labs[j]
    
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
    
    if (length(unique(rdffiles)) > 2) {
      warnings("Not setup to handle more than two rdf files")
    }
    
  }

} #close process and plot loop i 

if(combineplots == T){
  dev.off()
}
