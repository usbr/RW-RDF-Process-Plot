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
# CRSSDIR <- Sys.getenv("CRSS_DIR")
CRSSDIR <- "C:/Users/cfelletter/Documents/crss.offc"

# where scenarios are folder are kept
scen_dir <- file.path(CRSSDIR,"Scenario")
#containing the sub folders for each ensemble

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Plot Parameters ##

width=9# 10 #9
height=6 #6.67 #6

startyr <- 2020 #filter out all years > this year
endyr <- 2040

# Scenarios ##
# .Jun19/No Additional Controls 1.31M Tons change if Paradox is On (Aug15) vs Off (Aug19) for NFS full and stress test records
scens <- list(
  "Full Record w Paradox" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario1_2020_2019Aug15,DCP_Cons",
  "Full Record wo Paradox" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario1_2020_2019Aug19,DCP_Cons",
  "Stress Test w Paradox" = "2007Dems,MTOM_Most,DNF with Salinity Stress Test 88_17,Jun2019_9000,IG_DCP,WQIP_Scenario1_2020_2019Aug15,DCP_Cons",
  "Stress Test wo Paradox" = "2007Dems,MTOM_Most,DNF with Salinity Stress Test 88_17,Jun2019_9000,IG_DCP,WQIP_Scenario1_2020_2019Aug19,DCP_Cons"
)

#file names
Model.Step.Name <- "Paradox OnOff" #plot title and results/folder name
Figs <- '_ParadoxOnOff'  #[plot type] identifying name .pdf

# # # ##########################################################################
# # # # 2020 Scenarios for Aug 2019 MTG Preliminary Results 
# scens <- list(
#   "1.Jun19/No Additional Controls 1.31M Tons" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario1_2020_20190908,DCP_Cons",
#   "2.Jun19/Current Funding 1.58M Tons" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario2_2020_20190908,DCP_Cons",
#   "3.Jun19/Max Potential Controls 2.24M Tons" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario3_2020_20190908,DCP_Cons"
# )
# 
# #file names
# Model.Step.Name <- "Aug 2019 WG Results" #plot title and results/folder name
# Figs <- '_Aug19WGmtg'  #[plot type] identifying name .pdf

# # ##########################################################################

# # # 2020 Scenarios Aug 2019 w/ Jun19 mdl vs Apr MTG w/ Apr19 mdl  
# scens <- list(
#   "1.Jan19/No Additional Controls 1.42M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario1_2020",
#   "1.Jun19/No Additional Controls 1.31M Tons" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario1_2020_20190908,DCP_Cons",
#   "2.Jan19/Current Funding 1.68M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario2_2020",
#   "2.Jun19/Current Funding 1.58M Tons" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario2_2020_20190908,DCP_Cons",
#   "3.Jan19/Max Potential Controls 2.42M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario3_2020",
#   "3.Jun19/Max Potential Controls 2.24M Tons" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario3_2020_20190908,DCP_Cons"
# )
# 
# #file names
# Model.Step.Name <- "Aug vs Apr Results" #plot title and results/folder name
# Figs <- '_AugvsApr'  #[plot type] identifying name .pdf

# # ##########################################################################

# Hydrologies: NFS full, binational and stress test records.Jun19/No Additional Controls 1.31M Tons 

scens <- list(
  "Full Hydrology" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario1_2020_2019Aug19,DCP_Cons",
  "Early Pluvial Removed Hydrology" = "2007Dems,MTOM_Most,DNF with Salinity Binational,Jun2019_9000,IG_DCP,WQIP_Scenario1_2020_2019Aug19,DCP_Cons",
  "Stress Test Hydrology" = "2007Dems,MTOM_Most,DNF with Salinity Stress Test 88_17,Jun2019_9000,IG_DCP,WQIP_Scenario1_2020_2019Aug19,DCP_Cons"
)


#file names
Model.Step.Name <- "Hydrology Compare" #plot title and results/folder name
Figs <- '_HydrologyCompare'  #[plot type] identifying name .pdf

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

source(file.path(getwd(),"/code/plottingFunctions.R"))

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


if(length(scens) == 6 | length(scens) == 4){ #3 colors, first scen is dashed (old), second solid
  lt_scale <- rep(c(2, 1), 3)
  pt_scale <- rep(c(1, 19), 3)
  mycolors <- c("#1F78B4","#1F78B4","#33A02C","#33A02C","#E31A1C","#E31A1C") # brewer.pal(6, "Paired")
  # #Replace  scale_color_brewer(palette=pallette) + with scale_color_manual(values = mycolors) +
  # palette="Paired" #RColorBrewer https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
} else if (length(scens) == 2){ #1 color, first scen dashed, second solid 
  lt_scale <- rep(c(2, 1), 1)
  pt_scale <- rep(c(1, 19), 1)
  mycolors <- c("#1F78B4","#1F78B4")
} else if (length(scens) == 3){ #3 colors, solid
  lt_scale <- rep(1, 3)
  pt_scale <- rep(1, 3)
  mycolors <- c("#1F78B4","#33A02C","#E31A1C")
} else {
  stop("Not setup for correct Scens Number (6, 3, 2)")
}


#WQAnn
source("code/Custom_WQAnn.R") #means only w exceed
source("code/Custom_WQAnn_Clouds.R") #clouds for FWAAC

#SaltMassBal
source("code/Custom_MassBalAnn.R")
#FlowMassBal
source("code/Custom_FlowBalAnn.R")

#This script can only be used with data processed using the new output.control file 6/27/2019
# source("code/Custom_InOutMass.R")
source("code/Custom_PowellMead_3Panel.R") #still need to fix issues Jim suggested

# Powell Mead PE
source("code/Custom_ResPE.R")






