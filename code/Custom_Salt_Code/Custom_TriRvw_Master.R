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
CRSSDIR <- Sys.getenv("CRSS_DIR")
# CRSSDIR <- "C:/Users/cfelletter/Documents/crss.offc"
# CRSSDIR <- "C:/Users/cfelletter/Documents/crss.trirvw2020"


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

#### Compare Models for Feb WORKGROUP #### - Ran on April 9th - can't do 3 panel since Nov19 CRSS did not have inflow 
#
#file names
Model.Step.Name <- Figs <- "Apr20_vs_Feb20" #plot title and results/folder name #[plot type] identifying name .pdf

customcolorltpt <- F
startyr <- 2020 #filter out all years > this year

scens <- list(
  "Feb2020 1.69M Tons" = "2007Dems,MTOM_Most,DNF with Salinity Binational,Nov2019_9007,IG_DCP_4.3.0,WQIP_Scenario3_2020_20200113",
  "Apr2020 1.70M Tons" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200409"
)


# # ##########################################################################

# # #Compare 2017 and 2020 April MTG Scenarios - TriRvw17 Scen3 Reran and plots generated on April 9th
scens <- list(
  "TriRvw17 Limited Funding 1.66M Tons" = "DNF,Jan2017,WQIP_Senario3",
  "Apr2020 Plan of Implemenation 1.70M Tons" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200409"
)

#### Compare Models for Feb WORKGROUP ####

#file names
Model.Step.Name <- Figs <- "TriRvw17_vs_Apr2020" #plot title and results/folder name #[plot type] identifying name .pdf
customcolorltpt <- F
startyr <- 2017 #filter out all years > this year

# # ##########################################################################

# # # Feb MTG Hydrologies - Ran on April 9th
scens <- list(
   "ISM 1906-2018" = "2007Dems,MTOM_Most,DNF with Salinity 19062018 Reg8918,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200409",
  "ISM 1931-2018" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200409"
)


#file names
Model.Step.Name <- Figs <- "HydroCompare" #plot title and results/folder name #[plot type] identifying name .pdf
customcolorltpt <- F
startyr <- 2020 #filter out all years > this year

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#### ParadoxOnOffRamp ####

#file names
Model.Step.Name <- Figs <- "ParadoxOnOffRamp_0409" #plot title and results/folder name
customcolorltpt <- F
startyr <- 2020 #filter out all years > this year



# scens <- list(
#   "Paradox On100k" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200113_Paradox100",
#   "Padadox Off" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200113_ParadoxOff",
#   "Paradox Off20Half2126100k27" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200113_ParadoxRamp"#,
# )


scens <- list(
  "Paradox On100k" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200409_Paradox100",
  "Padadox Off" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200409_ParadoxOff",
  "Paradox Ramp" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200409"#,
)


# # # ##########################################################################

# #### Updated figure for April 2020 meeting - only 3 #### - Ran on April 9th
# 
#file names
Model.Step.Name <- Figs <- "Draft_FigUpdates_3Fig" #plot title and results/folder name
customcolorltpt <- F
startyr <- 2020 #filter out all years > this year


scens <- list(
  "Scenario 1 - 1.22M Tons" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario1_2020_20200409",
  "Scenario 2 - 1.59M Tons" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario2_2020_20200409",
  "Scenario 3 - 1.70M Tons" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200409"#,
)
# 
# 
#  #### Updated figure for April 2020 meeting - all 4 graphs #### - Ran on April 9th
# 
#file names
Model.Step.Name <- Figs <- "Draft_FigUpdates_4Fig" #plot title and results/folder name
customcolorltpt <- F
startyr <- 2020 #filter out all years > this year

scens <- list(
  "Scenario 1 - 1.22M Tons" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario1_2020_20200409",
  "Scenario 2 - 1.59M Tons" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario2_2020_20200409",
  "Scenario 3 - 1.70M Tons" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200409",
  "Scenario 4 - 2.35M Tons" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario4_2020_20200409"#,
)

# # ##########################################################################




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
  # mycolors <- c("#407ec9" , "#6b8f00", "#9a3324" , "#FECB00") #Reclamation blue, green, red, yellow
  # mycolors <- c("#9a3324" , "#FECB00", "#6b8f00" , "#407ec9") #TRY 1 red, yellow, red (stop light), blue
  mycolors <- c("#D55E00" , "#F0E442", "#009E73" , "#407ec9") #TRY 2 - color blind red, yellow, red (stop light), blue
  
} else if (customcolorltpt == F && length(scens) > 4) {
  stop("customcolorltpt not setup or too many Scens")
} 

#WQAnn
source("code/Custom_WQAnn.R") #means only w exceed
# source("code/Custom_WQAnn_OldControl.R") #for 2017 Review compare
# source("code/Custom_WQAnn_CloudswHist.R") #clouds for FWAAC - has titles
source("code/Custom_WQAnn_CloudswHist_woTitles.R") #clouds for Review, no title and larger margins

#MinMax must be run after a code has created scen_res but could work any scen_res
# source("code/MinMax_Check_NoFct.R") #RUNS REALLY SLOW WITH MONTHLY so only use with WQAnn

#SaltMassBal
source("code/Custom_MassBalAnn.R")
#Doesn't include UB Salt Mass Balance.ExportSaltMassExtra since this needs to be recreated in CRSS.OFFC
# source("code/Custom_MassBalAnn_OldControl.R")

# #This script can only be used with data processed using the new output.control file 6/27/2019
source("code/Custom_PowellMead_3Panel.R") #still need to fix issues Jim suggested - Jan2020 model now has Pow/Mead Inflow FWAAC
#need to add AnnualSalinity.PowellInflow_FWAAC to Nov 2019 model to use new (also add to WQAnn.rdf in output.control)
# source("code/Custom_PowellMead_3Panel_OldControlFile.R") #using this Jan 2020 model this shouldn't be an issue any more
 
# # #Saltstorage
# source("code/Custom_SaltStorage.R")

#FlowMassBal
source("code/Custom_FlowBalAnn.R")
# 
# # Powell Mead PE
source("code/Custom_ResPE.R")
source("code/Custom_PE_CloudswHist.R") #clouds for PE - UNDER DEVELOPMENT
# source("code/MinMax_Check_NoFct.R") #RUNS REALLY SLOW WITH MONTHLY so only use with WQAnn






