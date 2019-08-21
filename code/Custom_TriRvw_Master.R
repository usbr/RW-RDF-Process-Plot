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

## Scenarios ##
# # ##########################################################################
# # #Compare 2017 and 2020 Scenarios and 2017 model with 2012 NFS vs 2019 model with 2017 NFS
# scens <- list(
#   "A.No Additional Controls TriRvw17 1.33M Tons" = "DNF,Jan2017,WQIP_Senario1",
#   "A.No Additional Controls TriRvw20 1.42M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario1_2020",
#   "B.Current Funding TriRvw17 1.66M Tons" = "DNF,Jan2017,WQIP_Senario3",
#   "B.Current Funding TriRvw20 1.68M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario2_2020",
#   "C.Expanded Funding TriRvw17 1.79M Tons" = "DNF,Jan2017,WQIP_Senario4",
#   "D.Max Potential Controls TriRvw20 2.42M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario3_2020"
# )
# 
# #file names
# Model.Step.Name <- "2017 vs 2020 Control Scenarios" #plot title and results/folder name
# Figs <- '_controls_17v20Scens'  #[plot type] identifying name .pdf
# startyr <- 2017 #filter out all years > this year
# endyr <- 2040
# #
# # # ##########################################################################
# # #Compare 2017 and 2020 Scenarios and 2017 model with 2012 NFS vs 2019 model with 2017 NFS
# # scens <- list(
# #   "Current Funding TriRvw17 1.66M Tons" = "DNF,Jan2017,WQIP_Senario3",
# #   "Current Funding TriRvw20 1.68M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario2_2020"
# # )
# # 
# # #file names
# # Model.Step.Name <- "2017 vs 2020 Limited Funding Control Scenario" #plot title and results/folder name
# # Figs <- '_Trivw17v20_LmtdFunds'  #[plot type] identifying name .pdf
# # startyr <- 2017 #filter out all years > this year
# # endyr <- 2040
# # #
# # 
# # # ##########################################################################
# #Compare 2020 Scenarios between full 06-2017 (88-17 reg) NFS  with Stress Test 88-2017 (88-17 reg) NFS with 19 model
# scens <- list(
#   "A.No Additional Controls TriRvw20_06-17NFS" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario1_2020",
#   "A.No Additional Controls TriRvw20_88-17NFS" = "2007Dems,MTOM_Most,DNF with Salinity Stress Test 88_17,Jan2019_2020,IG,WQIP_Scenario1_2020",
#   "B.Current Funding TriRvw20_06-17NFS" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario2_2020",
#   "B.Current Funding TriRvw20_88-17NFS" = "2007Dems,MTOM_Most,DNF with Salinity Stress Test 88_17,Jan2019_2020,IG,WQIP_Scenario2_2020",
#   "C.Max Potential Controls TriRvw20_06-17NFS" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario3_2020",
#   "C.Max Potential Controls TriRvw20_88-17NFS" = "2007Dems,MTOM_Most,DNF with Salinity Stress Test 88_17,Jan2019_2020,IG,WQIP_Scenario3_2020"
# )
# 
# #file names
# Model.Step.Name <- "Observed NF vs Stress Test" #plot title and results/folder name
# Figs <- '_17NFSvs17NFSST_w_controls_20Scens'  #[plot type] identifying name .pdf
# 
# # # ##########################################################################
# #Compare allow neg storage Salt Change wo Min10 , 2020 Scenarios, full 06-2017 (88-17 reg) NFS  
# # # May 13: New Files for RW 7.5 DM fixes
# scens <- list(
#   # "Scen1" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario1_2020",
#   "wMin10" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario2_2020",
#   "woMin10" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019.9001.NoMin10,IG,WQIP_Scenario2_2020"#,
#   # "Scen3" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario3_2020",
# )
# #file names
# Model.Step.Name <- "w vs wo Min10 (RW 7.5)" #plot title and results/folder name
# Figs <- '_wvswoMin10'  #[plot type] identifying name .pdf

# # # ##########################################################################
# #Compare NIPP and MasBal updates , 2020 Scenarios, full 06-2017 (88-17 reg) NFS  
# scens <- list(
#   "Scen2_woMin10" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019.9001.NoMin10,IG,WQIP_Scenario2_2020",#,
#   "Scen2_woMin10_wMassBal" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019.9002.MassBal,IG,WQIP_Scenario2_2020"#,
#   # "2007Dems,MTOM_Most,DNF with Salinity,Jan2019.9002.MassBal_wNIPPnoPICKUP,IG,WQIP_Scenario2_2020"
#   )
# #file names
# Model.Step.Name <- "9002 w Mass Bal" #plot title and results/folder name
# Figs <- '_wvswoMassBal'  #[plot type] identifying name .pdf
# 
# # # ##########################################################################
# #Compare Convergance change updates , 2020 Scenarios, full 06-2017 (88-17 reg) NFS  
# scens <- list(
#   "Scen2" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019.9002.MassBal,IG,WQIP_Scenario2_2020",#,
#   "Scen2_wConverg0.001" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019.9003.Converg,IG,WQIP_Scenario2_2020"#,
#   )
# #file names
# Model.Step.Name <- "9003 w Converg" #plot title and results/folder name
# Figs <- '_wvswoMassBal'  #[plot type] identifying name .pdf
# 
# # # ##########################################################################
# #Compare changes of removing BLM from NFS and future controls 
# scens <- list(
#   "Baseline" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019.9003.Converg,IG,WQIP_Scenario2_2020",#,
#   "BLMWellPlugOnly" = "2007Dems,MTOM_Most,DNF with Salinity BLM Only,Jan2019.9003.Converg,IG,WQIP_Scenario2_2020_BLM"#,
#   # "Scen2_woallResampBLM" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jan2019.9003.Converg,IG,WQIP_Scenario2_2020_BLM"#,
# )
# #Scen2_woBLM has only AboveLees Salt change
# #Scen2_woallResampBLM has all residuals changed in UB
# #file names
# Model.Step.Name <- "wo BLM" #plot title and results/folder name
# Figs <- '_wvswoBLM'  #[plot type] identifying name .pdf

# # # ##########################################################################
# #Compare changes of changing the salt files going from PR to FWAC at Powell 
# #and changing WMx to 2 Lay w/ Jan 2019 IC's based on projection of Oct 2018 
# scens <- list(
#   "WMx_wWMxSalts" = "2007Dems,MTOM_Most,DNF with Salinity,9004.PowWMx,IG,WQIP_Scenario2_2020_BLM",#,
#   "2Lay_wFWACPwl" = "2007Dems,MTOM_Most,2Lay FWAC Salts,9004.Pow2Layer,IG,WQIP_Scenario2_2020_BLM"#,
#   # "Scen2_woallResampBLM" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jan2019.9003.Converg,IG,WQIP_Scenario2_2020_BLM"#,
# )
# Model.Step.Name <- "WMx vs 2Lay Mthd & Salts" #plot title and results/folder name
# Figs <- '_WMxvs2Lay'  #[plot type] identifying name .pdf
# 
# # # ##########################################################################
# 
# #Compare changes of changing the salt files going from PR to FWAC at Powell 
# #and changing WMx to 2 Lay w/ Jan 2019 IC's based on projection of Oct 2018 
# scens <- list(
#   "1_WMx_wWMxSalts" = "2007Dems,MTOM_Most,DNF with Salinity,9004.PowWMx,IG,WQIP_Scenario2_2020_BLM",#,
#   "2_WMx_wFWACPwl" = "2007Dems,MTOM_Most,2Lay FWAC Salts,9004.PowWMx,IG,WQIP_Scenario2_2020_BLM",#,
#   "4_2Lay_wFWACPwl" = "2007Dems,MTOM_Most,2Lay FWAC Salts,9004.Pow2Layer,IG,WQIP_Scenario2_2020_BLM",
#   "3_2Lay_wWMxSalts" = "2007Dems,MTOM_Most,DNF with Salinity,9004.Pow2Layer,IG,WQIP_Scenario2_2020_BLM"#,
# )
# Model.Step.Name <- "WMx vs 2Lay All Mthd & Salts" #plot title and results/folder name
# Figs <- '_AllWMxvs2Lay'  #[plot type] identifying name .pdf


# # ##########################################################################

# #Compare changes of changing IC 30mg/l in 2 layer model 
# scens <- list(
#   "IC" = "2007Dems,MTOM_Most,2Lay FWAC Salts,9004.Pow2Layer,IG,WQIP_Scenario2_2020_BLM",#,
#   "IC-30" = "2007Dems,MTOM_Most,2Lay FWAC Salts,Down30mgl,IG,WQIP_Scenario2_2020_BLM",#,
#   "IC+30" = "2007Dems,MTOM_Most,2Lay FWAC Salts,Up30mgl,IG,WQIP_Scenario2_2020_BLM"
# )
# Model.Step.Name <- "2 Lay IC Sensitivity" #plot title and results/folder name
# Figs <- '_2LayIC'  #[plot type] identifying name .pdf
# 
# # # ##########################################################################
# 
# #Jan to June IC 
# scens <- list(
#   "Jan2019" = "2007Dems,MTOM_Most,DNF with Salinity BLM Only,Jan2019.9003.Converg,IG,WQIP_Scenario2_2020_BLM",#,
#   "Jun2019_NoDCP" = "NV300,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG,WQIP_Scenario2_2020_BLM,No_DCP_Cons"#,
#   # "Jun2019_DCP" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario2_2020_BLM,DCP_Cons"
# )
# Model.Step.Name <- "Jan 19 to Jun 19 IC" #plot title and results/folder name
# Figs <- '_JantoJunIC'  #[plot type] identifying name .pdf
# 
# # # ##########################################################################
# 
# #Moving to DCP model  
# scens <- list(
#   # "Jan2019" = "2007Dems,MTOM_Most,DNF with Salinity BLM Only,Jan2019.9003.Converg,IG,WQIP_Scenario2_2020_BLM",#,
#   "Jun2019_NoDCP" = "NV300,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG,WQIP_Scenario2_2020_BLM,No_DCP_Cons",
#   "Jun2019_DCP" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario2_2020_BLM,DCP_Cons"
# )
# Model.Step.Name <- "No DCP to DCP" #plot title and results/folder name
# Figs <- '_NoDCPtoDCP'  #[plot type] identifying name .pdf
# 
# # # ##########################################################################
# 
# #Updating USDA controls 
# scens <- list(
#   "Baseline" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario2_2020_BLM,DCP_Cons",
#   "USDA_Update" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario2_2020_20190908,DCP_Cons"
#   )
# Model.Step.Name <- "August19 USDA Update" #plot title and results/folder name
# Figs <- '_20190908USDA'  #[plot type] identifying name .pdf

# # ##########################################################################


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

# # ##########################################################################
# # # 2020 Scenarios for Aug 2019 MTG Preliminary Results 
scens <- list(
  "1.Jun19/No Additional Controls 1.31M Tons" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario1_2020_20190908,DCP_Cons",
  "2.Jun19/Current Funding 1.58M Tons" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario2_2020_20190908,DCP_Cons",
  "3.Jun19/Max Potential Controls 2.24M Tons" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario3_2020_20190908,DCP_Cons"
)

#file names
Model.Step.Name <- "Aug 2019 WG Results" #plot title and results/folder name
Figs <- '_Aug19WGmtg'  #[plot type] identifying name .pdf

# # ##########################################################################

# # # 2020 Scenarios Aug 2019 w/ Jun19 mdl vs Apr MTG w/ Apr19 mdl  
scens <- list(
  "1.Jan19/No Additional Controls 1.42M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario1_2020",
  "1.Jun19/No Additional Controls 1.31M Tons" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario1_2020_20190908,DCP_Cons",
  "2.Jan19/Current Funding 1.68M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario2_2020",
  "2.Jun19/Current Funding 1.58M Tons" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario2_2020_20190908,DCP_Cons",
  "3.Jan19/Max Potential Controls 2.42M Tons" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Scenario3_2020",
  "3.Jun19/Max Potential Controls 2.24M Tons" = "2007Dems,MTOM_Most,DNF with Salinity BLM,Jun2019_9000,IG_DCP,WQIP_Scenario3_2020_20190908,DCP_Cons"
)

#file names
Model.Step.Name <- "Aug vs Apr Results" #plot title and results/folder name
Figs <- '_AugvsApr'  #[plot type] identifying name .pdf



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
source("code/Custom_WQAnn.R")
#SaltMassBal
source("code/Custom_MassBalAnn.R")
#FlowMassBal
source("code/Custom_FlowBalAnn.R")

#This script can only be used with data processed using the new output.control file 6/27/2019
source("code/Custom_InOutMass.R")
# Powell Mead PE
source("code/Custom_ResPE.R")





