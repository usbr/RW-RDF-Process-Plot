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
# install.packages('devtools')
# library(devtools)
# devtools::install_github('BoulderCodeHub/CRSSIO')

library(CRSSIO)
# plotEOCYElev() and csVarNames()

# install.packages('gridExtra') #need for ploting functions
source(file.path(getwd(),"/code/plottingFunctions.R"))

### Directory Set Up
CRSSDIR <- Sys.getenv("CRSS_DIR")
# CRSSDIR <- "C:/Users/cfelletter/Documents/crss.offc"
# CRSSDIR <- "C:/Users/cfelletter/Documents/crss.trirvw2020"
# CRSSDIR <- "C:/Users/cfelletter/Documents/crss.2023TRIRVW"

# where scenarios are folder are kept
scen_dir <- file.path(CRSSDIR,"Scenario")
#containing the sub folders for each ensemble

# scen_dir <- 'C:/Users/cfelletter/Documents/crss.2023TRIRVW/Scenario'

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Plot Parameters ##

width=11.29 #9
height=7.5 #6 #I've been using a bigger slide 6.67 #6

startyr <- 2023 #filter out all years > this year
endyr <- 2040 #60

customcolorltpt <- F

# # # ##########################################################################
# # Scenarios ##
# # # ##########################################################################

# # ##########################################################################

# ## All the following can use non custom colors 
# 
# Model.Step.Name <- Figs <- "2023Scens_ST_20230505_ParadoxOff" #plot title and results/folder name #[plot type] identifying name .pdf
# 
# scens <- list(
#   "Scenario 1" = "Obs_Jan23,Stress Test 88_20_LBUSGSnoPhreat,2023TriRvw.10.2022SaltIC,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000,WQIP_Scenario1_2023_OctForum_OffParadox",
#   "Scenario 2" = "Obs_Jan23,Stress Test 88_20_LBUSGSnoPhreat,2023TriRvw.10.2022SaltIC,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000,WQIP_Scenario2_2023_OctForum_OffParadox",
#   "Scenario 3" = "Fixed_Obs_Jan23,Stress Test 88_20_LBUSGSnoPhreat,2023TriRvw.10.2022SaltIC,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000,WQIP_Scenario3_2023_OctForum_OffParadox"#,
#   # "Scenario 4" = "Obs_Jan23,Stress Test 88_20_LBUSGSnoPhreat,2023TriRvw.10.2022SaltIC,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000,WQIP_Scenario4_2023_OctForum"
# )



Model.Step.Name <- Figs <- "CRSSv5_vs_v6.9011" #plot title and results/folder name #[plot type] identifying name .pdf
Model.Step.Name <- Figs <- "CRSSv5_vs_v6.9011vs9003" #plot title and results/folder name #[plot type] identifying name .pdf


scens <- list(
  "V5.TriRvw" = "Obs_Jan23,Stress Test 88_20_LBUSGSnoPhreat,2023TriRvw.10.2022SaltIC_MBfix,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000,WQIP_Scenario3_202 fixf__OffPFfaradox",
  "V6.9003MB" = "2016Dems,Scenario_3_20221014_ParadoxOff,ISM1988_2020_1115CameoRedux,Salt9003MB.2023,IG_DCPnoUBDRO",
  "V6.9011" = "2016Dems,Scenario_3_20221014_ParadoxOff,ISM1988_2020_wSalt,CRSS.V6.2.0.2024.Aug2023.9011.RW921.2023start,IG.v6.1.0.9006.RW921"
)



Model.Step.Name <- Figs <- "Nov23_v_Feb24salts_mdl9011" #plot title and results/folder name #[plot type] identifying name .pdf
scens <- list(
  # "V5.TriRvw" = "Obs_Jan23,Stress Test 88_20_LBUSGSnoPhreat,2023TriRvw.10.2022SaltIC_MBfix,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000,WQIP_Scenario3_202 fixf__OffPFfaradox",
  "Nov23.Salt" = "2016Dems,Scenario_3_20221014_ParadoxOff,ISM1988_2020_wSalt,CRSS.V6.2.0.2024.Aug2023.9011.RW921.2023start,IG.v6.1.0.9006.RW921",
  "Feb24.Salt" = "2016Dems,Scenario_3_20221014_ParadoxOff,RedoNoCameo_ISM1988_2020_wSalt,CRSS.V6.2.0.2024.Aug2023.9011.RW921.2023start,IG.v6.1.0.9006.RW921"
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

oFigs <- file.path(CRSSDIR,'results') 
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

fig_dir <- file.path(oFigs,"png_figures") 
if (!file.exists(fig_dir)) {
  message(paste('Creating folder:', "png_figures"))
  dir.create(fig_dir)
}
  
data_dir <- file.path(oFigs,"csv_data") 
if (!file.exists(data_dir)) {
  message(paste('Creating folder:', "csv_data"))
  dir.create(data_dir)
}  
  
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
  mycolors <- c("#D55E00" , "#F0E442", "#009E73" , "#407ec9") #TRY 2 - color blind red, yellow, red (stop light), blue

} else if (customcolorltpt == F && length(scens) > 4) {
  stop("customcolorltpt not setup or too many Scens")
} 

#WQAnn
source("code/Salt1_WQAnn.R") #means only w exceed
## MUST RUN WQAnn.R before running any of the cloud plots scripts 
# source("code/Custom_WQAnn_OldControl.R") #for 2020 Review compare
source("code/Salt1_WQAnn_CloudswHist.R") #clouds for FWAAC - has titles
# source("code/Custom_WQAnn_CloudswHist_woTitles.R") #clouds for Review, no title and larger margins

source("code/Salt1_SaltMassAnn_CloudswHist.R") #new mass and flow plots
source("code/Salt1_FlowAnn_CloudswHist.R") 

source("code/Salt1_CalcMeanDiff.R") # after running Salt1_WQAnn.R use this code to compare scenario 2-1 mean over entire run period and report out diff 

#MinMax must be run after a code has created scen_res but could work any scen_res
# source("code/MinMax_Check_NoFct.R") #RUNS REALLY SLOW WITH MONTHLY so only use with WQAnn

#SaltMassBal
source("code/Salt2_MassBalAnn.R")
source("code/Salt2_SaltMassBalance_Clouds.R")
source("code/Salt2_MassBalAnn_ByTrib.R") 

#Doesn't include UB Salt Mass Balance.ExportSaltMassExtra since this needs to be recreated in CRSS.OFFC
# source("code/Custom_MassBalAnn - CRSSv4 vs v5.R") #only works for 2 scens 

source("code/Salt3_PowellMead_3Panel.R") #grouped by flow, mass, conc
# source("code/Custom_PowellMead_3Panel_Clouds.R") # NOT WORKING #grouped by Pow.In, Pow.Stor, Pow.Out


source("code/Salt3_PowellMead_3Panel.R") #grouped by flow, mass, conc

#salt gains (inputs) 
source("code/GainsSaltMass_InputPlots.R")

# # #Saltstorage
# source("code/Custom_SaltStorage.R")

#FlowMassBal
source("code/Custom_FlowBalAnn.R")
source("code/FlowMassBalance_Clouds.R")
source("code/LBFlowPlots_OnlyLatestControlFile.R")

### UB PE and Outflow
source("code/Custom_UBResPEandOut.R")


# # Powell Mead PE
source("code/Custom_ResPE.R")
source("code/Custom_PE_CloudswHist.R") #clouds for PE - UNDER DEVELOPMENT
# source("code/MinMax_Check_NoFct.R") #RUNS REALLY SLOW WITH MONTHLY so only use with WQAnn






