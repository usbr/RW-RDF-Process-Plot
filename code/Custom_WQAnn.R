##############################################################################
#This script creates monthly boxplots of Outflow and PE to compare two MTOM runs
##############################################################################

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls()) #clear the enviornment 

### Directory Set Up
# where scenarios are folder are kept
scen_dir = file.path(getwd(),"scenarios") 
#containing the sub folders for each ensemble

CRSSDIR <- Sys.getenv("CRSS_DIR")

# where scenarios are folder are kept
scen_dir <- file.path(CRSSDIR,"Scenario")
#containing the sub folders for each ensemble
# 
# results_dir <- file.path(CRSSDIR,"results") 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#generic scenario locations 
# scens <- list(
#   "Without Additional Controls 2017" = "DNF,Jan2017,WQIP_Senario1",
#   "Plan of  Implementation 2035" = "DNF,Jan2017,WQIP_Senario3"
# )

#Compare All
# scens <- list(
#   "Jan17mdl_TriRvw17Scen3" = "DNF,Jan2017,WQIP_Senario3",
#   "Jan19mdl_TriRvw17Scen3" = "2007Dems,MTOM_Most,DNF with Salinity 2015,Jan2019_2020,IG,WQIP_Senario3",
#   "Jan19mdl_2017NFS_TriRvw17Scen3" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Senario3"
# )
# #file names 
# Figs <- 'SaltMassBalGrph_AllDevs.pdf'
# startyr = 2017 #filter out all years > this year
# endyr = 2040

#Compare 2017 NFS vs 2017 NFS Stress Test with 19 model (C vs D)
# scens <- list(
#   "Jan19mdl_2017NFS_TriRvw17Scen3" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Senario3",
#   "Jan19mdl_2017NFS_88-17_TriRvw17Scen3" = "2007Dems,MTOM_Most,DNF with Salinity Stress Test 88_17,Jan2019_2020,IG,WQIP_Senario3"
# )
# #file names 
# Figs <- 'WQAnn_17to17NFS_StressTest.pdf'
# startyr = 2020 #filter out all years > this year
# endyr = 2040

# #Compare 2015 NFS vs 2017 NFS with 19 model (B vs C)
# scens <- list(
#   "Jan19mdl_2015NFS_TriRvw17Scen3" = "2007Dems,MTOM_Most,DNF with Salinity 2015,Jan2019_2020,IG,WQIP_Senario3",
#   "Jan19mdl_2017NFS_TriRvw17Scen3" = "2007Dems,MTOM_Most,DNF with Salinity,Jan2019_2020,IG,WQIP_Senario3"
# )
# #file names 
# Figs <- 'SaltMassBalGrph_15to17NFS.pdf'
# startyr = 2020 #filter out all years > this year
# endyr = 2040

#Compare 17 vs 19 model ICs with 2012 NFS and 2015 NFS (A vs B)
scens <- list(
  "Jan17mdl_2012NFS_TriRvw17Scen3" = "DNF,Jan2017,WQIP_Senario3",
  "Jan19mdl_2015NFS_TriRvw17Scen3" = "2007Dems,MTOM_Most,DNF with Salinity 2015,Jan2019_2020,IG,WQIP_Senario3"
)
#file names
Figs <- 'WQAnn_17to19mdl.pdf'
startyr = 2017 #filter out all years > this year
endyr = 2040

mainScenGroup <- names(scens)[2] #"CurrentRun"

#agg file specifying which slots
rw_agg_file <- "WQAnn.csv" #latest including Price River fix 


# yrs2show <- startyr:endyr # can't use this until your run extends to end of 2023

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

# source('Output Data/RDF Process/plottingFunctions.R') 
source("C:/Users/ealexander/Documents/Process-MTOM-R/code/plottingFunctions.R")

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

oFigs <- file.path(oFigs,mainScenGroup) 
if (!file.exists(oFigs)) {
  message(paste('Creating folder:', oFigs))
  dir.create(oFigs)
}

message('Figures and tables will be saved to: ', oFigs)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# list.files(file.path(scen_dir,scens[2]))
# 
rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[2],"WQANN.rdf")))

#read agg file specifying which slots
# # NEW files are annual slots so use AsIs
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE))
# # # Old files from 2017 review are monthly so use EOCY 

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)

unique(scen_res$Variable) #check variable names 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## create a pdf  
pdf(file.path(oFigs,Figs), width=9, height=6)

# ++++++++++++++++++++++++++DoloresExp_OutSaltMass+++++++++++++++++++++++++++++++++++++

### Means ###

variable = "AnnlSlntyLsFrry_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River at Lees Ferry" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(350,550)


df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 
p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)
print(p)

write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))

#-------------------------------------------------------------------------------------

NumCrit <- data.frame(yintercept=723)
variable = "AnnlSlntyHvr_FWAAC"
# variable = "AnnlSlntyPrkr_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River below Hoover Dam" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(550,750)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 
p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) +
  ylim(ylims) +
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)
print(p)

write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))

#-------------------------------------------------------------------------------------

NumCrit <- data.frame(yintercept=747)
variable = "AnnlSlntyPrkr_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River below Parker Dam" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(550,750)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 
p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) +
  ylim(ylims) +
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)
print(p)

write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))

#-------------------------------------------------------------------------------------

NumCrit <- data.frame(yintercept=879)
variable = "AnnlSlntyImprl_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River above Imperial Dam" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(700,900)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 
p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) +
  ylim(ylims) +
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)
print(p)

write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))

#-------------------------------------------------------------------------------------
### Exceedence ###
#-------------------------------------------------------------------------------------
variable = "Exc_AnnlSlntyHvr_FWAAC"
y_lab = "Probability of Exceedance"
title = "Colorado River below Hoover Dam" 
subtitle = "Probability of Exceeding (>=) 723 mg/l"
ylims <- c(0,0.5)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 
p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  labs(title = title, y = y_lab, x = "Year" , subtitle = subtitle)
print(p)

write.csv(df,file = paste0(oFigs,'/','Exc_',variable,'.csv'))

#-------------------------------------------------------------------------------------
variable = "Exc_AnnlSlntyPrkr_FWAAC"
y_lab = "Probability of Exceedance"
title = "Colorado River below Parker Dam" 
subtitle = "Probability of Exceeding (>=) 747 mg/l"
ylims <- c(0,0.5)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 
p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  labs(title = title, y = y_lab, x = "Year" , subtitle = subtitle)
print(p)

write.csv(df,file = paste0(oFigs,'/','Exc_',variable,'.csv'))

#-------------------------------------------------------------------------------------
variable = "Exc_AnnlSlntyImprl_FWAAC"
y_lab = "Probability of Exceedance"
title = "Colorado River above Imperial Dam" 
subtitle = "Probability of Exceeding (>=) 879 mg/l"

ylims <- c(0,0.5)
df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 
p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  labs(title = title, y = y_lab, x = "Year" , subtitle = subtitle)
print(p)

write.csv(df,file = paste0(oFigs,'/','Exc_',variable,'.csv'))

dev.off()

