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
# CRSSDIR <- "C:/Users/cfelletter/Documents/crss.trirvw2023"

# where scenarios are folder are kept
scen_dir <- file.path(CRSSDIR,"Scenario")
#containing the sub folders for each ensemble

scen_dir <- 'C:/Users/cfelletter/Documents/crss.2023TRIRVW/Scenario'

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Plot Parameters ##

width=9# 10 #9
height=6 #6.67 #6

startyr <- 2020 #filter out all years > this year
endyr <- 2040 #60

customcolorltpt <- F

# # # ##########################################################################
# # Scenarios ##
# # # ##########################################################################

# # ##########################################################################

#### Compare Models for Feb WORKGROUP #### - Ran on April 9th - can't do 3 panel since Nov19 CRSS did not have inflow 
#
# #file names
# Model.Step.Name <- Figs <- "PartialDirectUBCompare" #plot title and results/folder name #[plot type] identifying name .pdf
# 
# scens <- list(
#   # "2020TriRvw_Scen3" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200409",
#   "Jan2022_2020Scen3" = "CRMMS_Most,DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.0,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0",
#   "DirectUB" = "CRMMS_Most,DNF with Salinity 19312018,NoResamp.2023TriRvw,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0",
#   "DirectUBExceptAbvLF" = "CRMMS_Most,DNF with Salinity 19312018,NFSinput_2018_DirectRegressabvPwll,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0",
#   "DirectUBExceptAbvLFBluffCiscoGRUT" = "CRMMS_Most,Direct Minus 4,NFSinput_2018_DirectRegressabvPwll,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0"
# )
# 
# Model.Step.Name <- Figs <- "2023vs2020Scens" #plot title and results/folder name #[plot type] identifying name .pdf
# 
# scens <- list(
#   # "2020TriRvw_Scen3" = "CRMMS_Most,DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.3,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000.rls,WQIP_Scenario1_2023_20220818",
#   "Jan2022_2020Scen3" = "CRMMS_Most,DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.0,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0",
#   # "Jan2022_2023Scen3" = "CRMMS_Most,DNF with Salinity 19312018,NFSinput_2018_DirectRegressabvPwll,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0",
#   "Jan2022_2023Scen3" = "CRMMS_Most,DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.3,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000.rls,WQIP_Scenario3_2023_20220818"
# )

# Model.Step.Name <- Figs <- "2023Scens_20220818" #plot title and results/folder name #[plot type] identifying name .pdf
# 
# scens <- list(
#   "Jan2022_2023Scen1" = "DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.5_Scen1,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000",
#   "Jan2022_2023Scen2" = "DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.5_Scen2,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000",
#   "Jan2022_2023Scen3" = "DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.5_Scen3,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000",
#   "Jan2022_2023Scen4" = "DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.5_Scen4,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000"
# )
# 
# Model.Step.Name <- Figs <- "Compare2020vs2023final" #plot title and results/folder name #[plot type] identifying name .pdf
# scens <- list(
#   "2020Rvw_Scen2" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario2_2020_20200409",
#   "2020Rvw_Scen3" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200409",
#   "Jan2022_2023Scen2" = "DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.5_Scen2,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000",
#   "Jan2022_2023Scen3" = "DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.5_Scen3,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000"
# )

Model.Step.Name <- Figs <- "TestOctoberChanges" #plot title and results/folder name #[plot type] identifying name .pdf
scens <- list(
    "2020Rvw_Scen3" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200409",
    "SeptWrkGrp_PulRmv_Scn3" = "DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.5_Scen3,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000",
    "2021FWAAC_PulRmv_Scn3" = "DNF with Salinity 19312018,2023TriRvw.6_2021FWAACics,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000,WQIP_Scenario3_2023_20220926",
    "2021FWAAC_ST_Scn3" = "DNF with Salinity Stress Test 88_17,2023TriRvw.6_2021FWAACics,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000,WQIP_Scenario3_2023_20220926"
)

# Model.Step.Name <- Figs <- "CompareICs" #plot title and results/folder name #[plot type] identifying name .pdf
# scens <- list(
#   "2020Rvw_Scen1" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario1_2020_20200409",
#   "2020RvwSaltIC_2023Scen1" = "CRMMS_Most,DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.3,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000.rls,WQIP_Scenario1_2023_20220818",
#   "UpdatedSaltIC_2023Scen1" = "CRMMS_Most,DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.4_Scen1,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000",
#   "UpdatedStor&SaltIC_2023Scen1" = "DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.5_Scen1,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000"
# )


# Model.Step.Name <- Figs <- "Compare2020vs2023_mdl&ICupdates" #plot title and results/folder name #[plot type] identifying name .pdf
# scens <- list(
#   "CRSSv4_2020ICs_2020Scen3" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200409",
#   "CRSSv5_2022ICs_2020Scen3" ="DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.5.has2022ICs,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0.9000,WQIP_Scenario3_2020_20210526"
# )



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
## MUST RUN WQAnn.R before running any of the cloud plots scripts 
# source("code/Custom_WQAnn_OldControl.R") #for 2017 Review compare
source("code/Custom_WQAnn_CloudswHist.R") #clouds for FWAAC - has titles
# source("code/Custom_WQAnn_CloudswHist_woTitles.R") #clouds for Review, no title and larger margins

#MinMax must be run after a code has created scen_res but could work any scen_res
# source("code/MinMax_Check_NoFct.R") #RUNS REALLY SLOW WITH MONTHLY so only use with WQAnn

#SaltMassBal
source("code/Custom_MassBalAnn.R")
#Doesn't include UB Salt Mass Balance.ExportSaltMassExtra since this needs to be recreated in CRSS.OFFC
# source("code/Custom_MassBalAnn - CRSSv4 vs v5.R") #only works for 2 scens 

source("code/Custom_PowellMead_3Panel.R") #grouped by flow, mass, conc

# source("code/Custom_PowellMead_3Panel_Clouds.R") # NOT WORKING #grouped by Pow.In, Pow.Stor, Pow.Out

 
# # #Saltstorage
# source("code/Custom_SaltStorage.R")

#FlowMassBal
source("code/Custom_FlowBalAnn.R")
# 
# # Powell Mead PE
source("code/Custom_ResPE.R")
source("code/Custom_PE_CloudswHist.R") #clouds for PE - UNDER DEVELOPMENT
# source("code/MinMax_Check_NoFct.R") #RUNS REALLY SLOW WITH MONTHLY so only use with WQAnn






