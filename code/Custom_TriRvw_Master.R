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

#Additional plotting functions and libraries 
library('tidyverse') #ggplot2,dplyr,tidyr
library('devtools')
library(RWDataPlyr)
#see RWDATPlyr Workflow for more information 
library(CRSSIO)
# plotEOCYElev() and csVarNames()

source(file.path(getwd(),"/code/plottingFunctions.R"))

### Directory Set Up
# CRSSDIR <- Sys.getenv("CRSS_DIR")
# CRSSDIR <- "C:/Users/cfelletter/Documents/crss.offc"
CRSSDIR <- "C:/Users/cfelletter/Documents/crss.trirvw2020"


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
endyr <- 2040 #60

# # # ##########################################################################
# # Scenarios ##
# # # ##########################################################################


# # ##########################################################################

# # # 2020 Scenarios Oct 2019 MTG w/ Jun19 mdl and 4 Scens  (3-TriennialReviewScenarios_2019Oct15)
# scens <- list(
#   "No Additional Beyond 2020 1.31M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jun2019_9002_4Scens,IG_DCP,WQIP_Scen1_2019Oct15,DCP_Cons",
#   "Limited Funding 1.58M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jun2019_9002_4Scens,IG_DCP,WQIP_Scen2_2019Oct15,DCP_Cons",
#   "Plan of Implementation 1.70M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jun2019_9002_4Scens,IG_DCP,WQIP_Scen3_2019Oct15,DCP_Cons",
#   "Max Potential Controls 2.34M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jun2019_9002_4Scens,IG_DCP,WQIP_Scen4_2019Oct15,DCP_Cons"
# )
# 
# #file names
# Model.Step.Name <- "Oct 4 Scen Results" #plot title and results/folder name
# Figs <- '_OctResults'  #[plot type] identifying name .pdf
# startyr <- 2020 #filter out all years > this year
# 
# customcolorltpt <- F

# # ##########################################################################

# # # #Compare 2020 Scenarios Oct 2019 MTG w/ Aug 2019 MTG Scenarios


# scens <- list(
#   "Aug: No Additional Beyond 2020 1.31M Tons" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario1_2020_20190908,DCP_Cons",
#   "Oct: No Additional Beyond 2020 1.31M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jun2019_9002_4Scens,IG_DCP,WQIP_Scen1_2019Oct15,DCP_Cons",
#   "Aug: Limited Funding 1.58M Tons" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario2_2020_20190908,DCP_Cons",
#   "Oct: Limited Funding 1.58M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jun2019_9002_4Scens,IG_DCP,WQIP_Scen2_2019Oct15,DCP_Cons",
#   # "Plan of Implementation 1.70M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jun2019_9002_4Scens,IG_DCP,WQIP_Scen3_2019Oct15,DCP_Cons",
#   "Aug: Max Potential Controls 2.24M Tons" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario3_2020_20190908,DCP_Cons",
#   "Oct: Max Potential Controls 2.34M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jun2019_9002_4Scens,IG_DCP,WQIP_Scen4_2019Oct15,DCP_Cons"
# )
# 
# #file names
# Model.Step.Name <- "Oct vs Aug Results" #plot title and results/folder name
# Figs <- '_OctvsAug'  #[plot type] identifying name .pdf
# startyr <- 2020 #filter out all years > this year
# 
# 
# ### Paired Scens #### - 4 colors, first scen is dashed (old), second solid
# customcolorltpt <- T
# lt_scale <- c(rep(c(2, 1), 3),1)
# pt_scale <- c(rep(c(1, 19), 3),19)
# mycolors <- c("#407ec9" ,"#407ec9", "#6b8f00","#6b8f00", "#9a3324" ,"#9a3324" ,"#FECB00","#FECB00") #Reclamation blue, green, red, yellow

# # ##########################################################################
# # # RW 76 Snapshot Changes COMPLETE BUT NOT YET REVIEWED
# 
# scens <- list(
#   # "Baseline" = "2007Dems,MTOM_Most,DNF with Salinity,Jun2019_9000,IG_DCP,WQIP_Scenario2_2020_2019Sept6,DCP_Cons",
#   "RW76 Snapshot" = "2007Dems,MTOM_Most,DNF with Salinity,RW96_Jun2019_9000,IG_DCP,WQIP_Scenario2_2020_2019Sept6,DCP_Cons",
#   "RW8 Prerelease" = "2007Dems,MTOM_Most,DNF with Salinity,RW98_Jun2019_9000,IG_DCP,WQIP_Scenario2_2020_2019Sept6,DCP_Cons"
# )
# 
# #file names
# # Model.Step.Name <- "RW78Snapshot" #plot title and results/folder name
# # Figs <- '_RW76Snapshot'  #[plot type] identifying name .pdf
# Model.Step.Name <- "RW76Snapshotvs78" #plot title and results/folder name
# Figs <- '_RW76vs78'  #[plot type] identifying name .pdf
# 
# customcolorltpt <- F
# 
# # # ##########################################################################
# 
# 
# # # Aaron PUV Hydro Analysis
# scens <- list(
#   "Full w PUV" = "2007Dems,MTOM_Most,DNF with Salinity,Jun2019_9002_4Scens,IG_DCP,WQIP_Scen3_2019Oct15,DCP_Cons",
#   "Full wo PUV" = "2007Dems,MTOM_Most,DNF with Salinity,Jun2019_9002_4Scens,IG_DCP,NoParadox_WQIP_Scen3_2019Oct15,DCP_Cons",
#   "PulRmvd w PUV" = "2007Dems,MTOM_Most,DNF with Salinity Binational,Jun2019_9002_4Scens,IG_DCP,WQIP_Scen3_2019Oct15,DCP_Cons",
#   "PulRmvd wo PUV" = "2007Dems,MTOM_Most,DNF with Salinity Binational,Jun2019_9002_4Scens,IG_DCP,NoParadox_WQIP_Scen3_2019Oct15,DCP_Cons"
# )
# 
# #file names
# Model.Step.Name <- "PUV Hydro Results" #plot title and results/folder name
# Figs <- '_PUVHydro'  #[plot type] identifying name .pdf
# startyr <- 2020 #filter out all years > this year
# 
# customcolorltpt <- F


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Custom Color/Pt Types/Scales ### 

#### Paired Scens #### - 4 colors, first scen is dashed (old), second solid
#customcolorltpt <- T 
# lt_scale <- rep(c(2, 1), 4)
# pt_scale <- rep(c(1, 19), 4)
# mycolors <- c("#407ec9" ,"#407ec9", "#6b8f00","#6b8f00", "#9a3324" ,"#9a3324" ,"#FECB00","#FECB00") #Reclamation blue, green, red, yellow

#### Dashed Only for Old Scens #### - 4 colors, dashed (old)
#customcolorltpt <- T 
# lt_scale <- rep(1, 4)
# pt_scale <- rep(19, 4) #dashed 
# mycolors <- c("#407ec9" , "#6b8f00", "#9a3324" , "#FECB00") #Reclamation blue, green, red, yellow



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                               END USER INPUT
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


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

#colors taken from Reclamation visual identity https://intra.usbr.gov/vip/III-colors.html

### Standard Color/Pt Types/Scales ### #customcolorltpt <- F 

if(customcolorltpt == F && length(scens) == 2){ #1 color, first scen dashed, second solid 
  lt_scale <- rep(c(2, 1), 1)
  pt_scale <- rep(c(1, 19), 1)
  mycolors <- c("#ba9e66","#407ec9") #reclamation sand (old/baseline rslts), rec blue )

} else if (customcolorltpt == F && length(scens) <= 4){ #4 colors, solid
  lt_scale <- rep(1, 4)
  pt_scale <- rep(19, 4)
  mycolors <- c("#407ec9" , "#6b8f00", "#9a3324" , "#FECB00") #Reclamation blue, green, red, yellow
} else if (customcolorltpt == F && length(scens) > 4) {
  stop("customcolorltpt not setup or too many Scens")
} 

#WQAnn
source("code/Custom_WQAnn.R") #means only w exceed
# source("Custom_WQAnn_OldControl.R")
source("code/Custom_WQAnn_CloudswHist.R") #clouds for FWAAC - UNDER DEVELOPMENT
#MinMax must be run after a code has created scen_res but could work any scen_res
source("code/MinMax_Check_NoFct.R")


#SaltMassBal
source("code/Custom_MassBalAnn.R")
# #MinMax must be run after a code has created scen_res but could work any scen_res
# source("code/MinMax_Check_NoFct.R")

#FlowMassBal
source("code/Custom_FlowBalAnn.R")

#This script can only be used with data processed using the new output.control file 6/27/2019
# source("code/Custom_InOutMass.R")
source("code/Custom_PowellMead_3Panel.R") #still need to fix issues Jim suggested

# Powell Mead PE
source("code/Custom_ResPE.R")
source("code/Custom_PE_CloudswHist.R") #clouds for PE - UNDER DEVELOPMENT
source("code/MinMax_Check_NoFct.R")






