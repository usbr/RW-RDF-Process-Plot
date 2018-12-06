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

# rdffiles <- c("res.rdf") #rdf file with slot you want
# # rdffiles <- c("daily.rdf") #rdf file with slot you want
# 
# #list slots in rdf
# rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],rdffiles[1])))
# 
# variables <- c("Powell.Inflow") #RW Object.Slot
# # variables <- c("Powell.Pool Elevation") #RW Object.Slot
# # variables <- c("FlamingGorge.Pool Elevation") #RW Object.Slot
# # variables <- c("KNN_MTOM.FlamingGorgeDaily") #RW Object.Slot
# 
# # timesteps <- c("annual") #"annual" or "monthly"
# timesteps <- c("monthly") #"annual" or "monthly"
# # timesteps <- c("daily") #"annual" or "monthly" or "daily"
# #WARNING: Daily Plots are still under development and all results should be cross checked. 
# #Avoid using figuretypes 2 is Bxplt of Traces & 3 is Exceedance until further developement is complete. 
# #https://github.com/usbr/RW-RDF-Process-Plot/issues/17
# 
# floworpes <- c("flow") #"flow" or "pe"
# # floworpes <- c("pe") #"flow" or "pe"
# 
# cyorwys <- c("cy") #"cy" or "wy". wy not tested for all plot configurations
# #daily only supports cy
# 
# mainScenGroup <<- names(scens)[2] #name of the subfolder this analysis will be stored
# 
# first_ensemble <<- c(2,2) #filter out Most,Min,Max. For 38 trace offical = 4, 
# #36 trace month w Most = 2. Same order as for scenarios  
# 
# model <<- "MTOM" #"CRSS" or "MTOM"
# 
# ## Plot Variables ##
# 
# combineplots <<- F #F for individual files saved, true to combineplots multiple files
# 
# figuretypes <- c(1) #1 is Trace Mean, 2 is Bxplt of Traces, 3 is Exceedance
# # IF PICKING "monthly" 3 you must specify a month
# exc_months <- c(12) #1 - Jan, 12 - Dec
# #Note: exceedance month is only needed for monthly pe exceedance (3)
# 
# startyrs <- c(2019) #filter out all years > this year
# endyrs <- c(2022) #filter out all years > this year
# #these must match if doing daily slot
# 
# customcaptions <- c(NA) #NA or this will overwrite the default captions
# # customcaptions <- c("May") #NA or this will overwrite the default captions
# #set for mean (1) and boxplot (2)
# 
# custom_y_labs <- c(NA) #NA gives defaults, enter if want soemthing different
# # custom_y_labs <- c("May") #NA gives defaults, enter if want soemthing different
# ## daily = "Daily Flow (cfs)"
# ## monthly = "Monthly Flow (ac-ft/mo)" OR "End of Month PE (ft)"
# ## annual = "Annual C/WY Flow (ac-ft/mo)" OR "EOC/WY PE (ft)" it knows CYorWY
# 
# #file names
# figname <<- 'Example_1_Fig'
# #if combineplots is T figs saved as figname.pdf w/o captions
# #if F figs saved individually as figname_timestep_cyorwy_variable_figuretype.imgtype
# 
# minmaxchk <- F #only works for monthly currently (12/6/18)

############# Examples of Code Use ############################################

## Select the region and hit ctrl+c to enable #####
# # Note: Examples 1-3 use the above input parameters.any parameters that are 
# #listed twice will be set as the latest (furtherst down) value

# # Example 1: Single Trace w/ above parameters. 

#Example 2: Bad UI, slot not in rdf

# figuretypes <- 4

#Example 3: Bad UI, slot not in rdf

# variables <- "Fake.News"

# # Note: Examples 4 uses the below input parameters.

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
figname <<- 'Example_3_Std_PowellMead_Fig'
model <<- "MTOM" #"CRSS" or "MTOM"
first_ensemble <<- c(2,2) #filter out Most,Min,Max. For 38 trace offical = 4,
minmaxchk <- T #only works for monthly currently (12/6/18)

#### End Examples #### 

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
