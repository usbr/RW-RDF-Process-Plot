##############################################################################
#This masters script creates WQAnn and SaltMassBal figures by calling 
# other R code code/Custom_WQAnn & Custom_MassBalAnn.R
# Use this script to select a scenario and plot figure titles 
# No need to modify individual plot scripts 
##########################################################################

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls()) #clear the enviornment 

### Directory Set Up
CRSSDIR <- Sys.getenv("CRSS_DIR")

# where scenarios are folder are kept
scen_dir <- file.path(CRSSDIR,"Scenario")
#containing the sub folders for each ensemble

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Compare 17 vs 19 model ICs with 2012 NFS and 2015 NFS (A vs B)
scens <- list(
  "Jan17mdl_06-12NFS_TriRvw17Scen3" = "DNF,Jan2017,WQIP_Senario3",
  "Jan19mdl_06-12NFS_TriRvw17Scen3" = "2007Dems,MTOM_Most,DNF with Salinity 2012,Jan2019_2020,IG,WQIP_Senario3"
)
#file names
Model.Step.Name <- "2017 to 2019 Model and ICs" #plot title and results/folder name
Figs <- '_mdl_17vs19'  #[plot type] identifying name .pdf 
startyr <- 2017 #filter out all years > this year
endyr <- 2040

##########################################################################
# Compare 2015 NFS vs 2017 NFS with 19 model (B vs C)
scens <- list(
  "06-12NFS_Jan19mdl_TriRvw17Scen3" = "2007Dems,MTOM_Most,DNF with Salinity 2012,Jan2019_2020,IG,WQIP_Senario3",
  "06-17NFS_Jan19mdl_TriRvw17Scen3" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Senario3"
)
#file names
Model.Step.Name <- "NFS 06-12 to 06-17" #plot title and results/folder name
Figs <- '_NFS_06-12vs06-17'  #[plot type] identifying name .pdf 
startyr <- 2020 #filter out all years > this year
endyr <- 2040

##########################################################################
#Compare 06-2017 NFS vs 88-2017 NFS Stress Test with 19 model (C vs D)
scens <- list(
  "06-17NFS_Jan19mdl_TriRvw17Scen3" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Senario3",
  "88-17NFS_Jan19mdl_TriRvw17Scen3" = "2007Dems,MTOM_Most,DNF with Salinity Stress Test 88_17,Jan2019_2020,IG,WQIP_Senario3"
)
#file names
Model.Step.Name <- "NFS 06-17 to 88-17" #plot title and results/folder name
Figs <- '_NFS_06-17vs88-17'  #[plot type] identifying name .pdf 
startyr <- 2020 #filter out all years > this year
endyr <- 2040

##########################################################################
#Compare 2017 Scen 3 vs 2020 Scen 2 WQIP controls, 06-2017 NFS with 19 model (C vs E)
scens <- list(
  "TriRvw17Scen3_Jan19mdl_06-17NFS" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Senario3",
  "TriRvw20Scen2_Jan19mdl_06-17NFS" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario2_2020"
)
#file names
Model.Step.Name <- "Lmtd Funds 2017 vs 2020" #plot title and results/folder name
Figs <- '_controls_17Scen3vs20Scen2'  #[plot type] identifying name .pdf 
startyr <- 2020 #filter out all years > this year
endyr <- 2040

##########################################################################
#Compare 2020 Scen 2 vs 2020 Scen 3 Max WQIP controls, 06-2017 NFS with 19 model 
scens <- list(
  "TriRvw20Scen1_Jan19mdl_06-17NFS" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario1_2020",
  "TriRvw20Scen2_Jan19mdl_06-17NFS" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario2_2020",
  "TriRvw20Scen3_Jan19mdl_06-17NFS3" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario3_2020"
)
#file names
Model.Step.Name <- "2020 Control Scenarios" #plot title and results/folder name
Figs <- '_controls_20Scens'  #[plot type] identifying name .pdf 
startyr <- 2020 #filter out all years > this year
endyr <- 2040

##########################################################################
#Compare 2020 Scen 2 vs 2020 Scen 3 Max WQIP controls, 06-2017 NFS with 19 model 
scens <- list(
  "A.No Additional Controls TriRvw17" = "DNF,Jan2017,WQIP_Senario1",
  "A.No Additional Controls TriRvw20" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario1_2020",
  "B.Current Funding TriRvw17" = "DNF,Jan2017,WQIP_Senario2",
  "B.Current Funding TriRvw20" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario2_2020",
  "C.Expanded Funding TriRvw17" = "DNF,Jan2017,WQIP_Senario3",
  "D.Max Potential Controls TriRvw20" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario3_2020"
)
#file names
Model.Step.Name <- "2017 vs 2020 Control Scenarios" #plot title and results/folder name
Figs <- '_controls_17v20Scens'  #[plot type] identifying name .pdf 
startyr <- 2017 #filter out all years > this year
endyr <- 2040

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                               END USER INPUT
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Additional plotting functions and libraries 
library('tidyverse') #ggplot2,dplyr,tidyr
library('devtools')
library(RWDataPlyr)
#see RWDATPlyr Workflow for more information 
library(CRSSIO)
# plotEOCYElev() and csVarNames()

source("/code/plottingFunctions.R")

# check folders
if(!file.exists(file.path(scen_dir, scens[1])) 
   | !file.exists(file.path(scen_dir, scens[2])))
  stop('Scenarios folder(s) do not exist or scen_dir is set up incorrectly. 
       Please ensure Scenarios is set correctly.')

oFigs <- file.path(getwd(),'results') 
if (!file.exists(oFigs)) {
  message(paste('Creating folder:', oFigs))
  dir.create(oFigs)
}

oFigs <- file.path(oFigs,Model.Step.Name) 
if (!file.exists(oFigs)) {
  message(paste('Creating folder:', Model.Step.Name))
  dir.create(oFigs)
}

message('Figures and tables will be saved to: ', oFigs)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#WQAnn
source("code/Custom_WQAnn.R")
#SaltMassBal
source("code/Custom_MassBalAnn.R")
#FlowMassBal
source("code/Custom_FlowBalAnn.R")


