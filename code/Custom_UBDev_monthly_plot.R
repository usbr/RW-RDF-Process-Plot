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

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#generic scenario locations 
my_scens = c("PreviousRun", "CurrentRun") #names for your senarios to plot
# this is the order they will show up in the table & plot, so list the newest 
#run second there should only be 2 scenarios

# #custom scenario folders, select the below lines and ctrl + shift + c to use
# scenarios = my_scens =  c("dev", "FGdev") 

names(scenarios) = my_scens #naming 

mainScenGroup <- names(scenarios)[2] #"CurrentRun"

#### Normally You'll Only Change This ####
first_ensemble = c(2,2) #filter out Most,Min,Max. For 38 trace offical = 4, 
#36 trace month w Most = 2. Same order as for scenarios  
#### End of Normally You'll Only Change This ####

#agg file specifying which slots
rw_agg_file <- "rw_agg_UBdev_monthly.csv"

#file names 
Figs <- 'MonthlyResFigs.pdf' 

startyr = 2019 #filter out all years > this year
filteryrlessorequal = 2022

yrs2show <- startyr:filteryrlessorequal # can't use this until your run extends to end of 2023

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
source('Output Data/RDF Process/plottingFunctions.R') 

# check folders
if(!file.exists(file.path(scen_dir, scenarios[1])) 
   | !file.exists(file.path(scen_dir, scenarios[2])))
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
#read agg file specifying which slots
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) 

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scenarios,
  agg = rwa1,
  scen_dir = scen_dir
) %>% 
  # filter out Most,Min,Max
  dplyr::filter(
    (Scenario == scenarios[1] & TraceNumber >= first_ensemble[1]) |
    (Scenario == scenarios[2] & TraceNumber >= first_ensemble[2])
  ) 

unique(scen_res$Variable) #check variable names 

#get everything on a date 
scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
#get a numeric month number
scen_res$MonthNum = as.numeric(format.Date(scen_res$MonthNum, format = "%m"))

#filter out incomplete years
scen_res <- scen_res %>%
    dplyr::filter(Year >= first(yrs2show)) %>%
    dplyr::filter(Year <= last(yrs2show))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## create a pdf  
pdf(file.path(oFigs,Figs), width=9, height=6)

# variable = "Crystal.Outflow"
# y_lab = "Monthly Flow (ac-ft/mo)"
# title = paste(variable,first(yrs2show),"-",last(yrs2show))
# 
# p <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
#   dplyr::group_by(Scenario, MonthNum) %>%
#   ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
#   geom_boxplot() +
#   scale_x_discrete("Month",labels = month.abb) + #display abb. month names
#   labs(title = title, y = y_lab) 
# print(p)
# 
# 
# variable = "Crystal.Pool Elevation"
# y_lab = "End of Month PE (ft)"
# title = paste(variable,first(yrs2show),"-",last(yrs2show))
# PEtarget <- data.frame(yintercept=6753.04) 
# 
# p <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
#   dplyr::group_by(Scenario, MonthNum) %>%
#   ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
#   geom_boxplot() +
#   scale_x_discrete("Month",labels = month.abb) + #display abb. month names
#   geom_hline(aes(yintercept=yintercept), data=PEtarget) +
#   labs(title = title, y = y_lab) 
# print(p)
# 
# variable = "BlueMesa.Outflow"
# y_lab = "Monthly Flow (ac-ft/mo)"
# title = paste(variable,first(yrs2show),"-",last(yrs2show))
# 
# p <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
#   dplyr::group_by(Scenario, MonthNum) %>%
#   ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
#   geom_boxplot() +
#   scale_x_discrete("Month",labels = month.abb) + #display abb. month names
#   labs(title = title, y = y_lab) 
# print(p)
# 
# variable = "BlueMesa.Pool Elevation"
# y_lab = "End of Month PE (ft)"
# title = paste(variable,first(yrs2show),"-",last(yrs2show))
# Jan31PEtarget <- data.frame(yintercept=7487) 
# Feb31PEtarget <- data.frame(yintercept=7485) 
# Mar31PEtarget <- data.frame(yintercept=7484.5) 
# Apr31PEtarget <- data.frame(yintercept=7491) 
# MayNov31PEtarget <- data.frame(yintercept=7495) 
# JunJul31PEtarget <- data.frame(yintercept=7516.4) 
# Aug31PEtarget <- data.frame(yintercept=7507) 
# Sep31PEtarget <- data.frame(yintercept=7498) 
# Oct31PEtarget <- data.frame(yintercept=7496.5) 
# Dec31PEtarget <- data.frame(yintercept=7490) 
# 
# p <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
#   dplyr::group_by(Scenario, MonthNum) %>%
#   ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
#   geom_boxplot() +
#   scale_x_discrete("Month",labels = month.abb) + #display abb. month names
#   geom_hline(aes(yintercept=yintercept), data=Jan31PEtarget) +
#   geom_hline(aes(yintercept=yintercept), data=Feb31PEtarget) +
#   geom_hline(aes(yintercept=yintercept), data=Mar31PEtarget) +
#   geom_hline(aes(yintercept=yintercept), data=Apr31PEtarget) +
#   geom_hline(aes(yintercept=yintercept), data=MayNov31PEtarget) +
#   geom_hline(aes(yintercept=yintercept), data=JunJul31PEtarget) +
#   geom_hline(aes(yintercept=yintercept), data=Aug31PEtarget) +
#   geom_hline(aes(yintercept=yintercept), data=Sep31PEtarget) +
#   geom_hline(aes(yintercept=yintercept), data=Oct31PEtarget) +
#   geom_hline(aes(yintercept=yintercept), data=Dec31PEtarget) +
#   labs(title = title, y = y_lab) 
# print(p)
# 
# variable = "GunnisonNearGrandJunction.AFOutflow"
# y_lab = "Monthly Flow (ac-ft/mo)"
# title = paste(variable,first(yrs2show),"-",last(yrs2show))
# DryTarget <- data.frame(yintercept=55340) 
# AvgDryTarget <- data.frame(yintercept=496200)
# ModWetTarget <- data.frame(yintercept=882350) 
# 
# p <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
#   dplyr::group_by(Scenario, MonthNum) %>%
#   ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
#   geom_boxplot() +
#   scale_x_discrete("Month",labels = month.abb) + #display abb. month names
#   geom_hline(aes(yintercept=yintercept), data=DryTarget) +
#   geom_hline(aes(yintercept=yintercept), data=AvgDryTarget) +
#   geom_hline(aes(yintercept=yintercept), data=ModWetTarget) +
#   labs(title = title, y = y_lab) 
# print(p)
# 
# variable = "Fontenelle.Outflow"
# y_lab = "Monthly Flow (ac-ft/mo)"
# title = paste(variable,first(yrs2show),"-",last(yrs2show))
# 
# p <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
#   dplyr::group_by(Scenario, MonthNum) %>%
#   ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
#   geom_boxplot() +
#   scale_x_discrete("Month",labels = month.abb) + #display abb. month names
#   labs(title = title, y = y_lab) 
# print(p)
# 
# variable = "Fontenelle.Pool Elevation"
# y_lab = "End of Month PE (ft)"
# title = paste(variable,first(yrs2show),"-",last(yrs2show))
# Apr1PEtarget <- data.frame(yintercept=6468) 
# Jul1PEtarget <- data.frame(yintercept=6500) 
# Aug1PEtarget <- data.frame(yintercept=6505.5) 
# 
# p <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
#   dplyr::group_by(Scenario, MonthNum) %>%
#   ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
#   geom_boxplot() +
#   scale_x_discrete("Month",labels = month.abb) + #display abb. month names
#   geom_hline(aes(yintercept=yintercept), data=Apr1PEtarget) +
#   geom_hline(aes(yintercept=yintercept), data=Jul1PEtarget) +
#   geom_hline(aes(yintercept=yintercept), data=Aug1PEtarget) +
#   labs(title = title, y = y_lab) 
# print(p)

variable = "FlamingGorge.Outflow"
y_lab = "Monthly Flow (ac-ft/mo)"
title = paste(variable,first(yrs2show),"-",last(yrs2show))

p <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = title, y = y_lab) 
print(p)

variable = "FlamingGorge.Pool Elevation"
y_lab = "End of Month PE (ft)"
title = paste(variable,first(yrs2show),"-",last(yrs2show))
MaxPEtarget <- data.frame(yintercept=6039) 

p <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  geom_hline(aes(yintercept=yintercept), data=MaxPEtarget) +
  labs(title = title, y = y_lab) 
print(p)

dev.off()


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5. Check Min
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

minvals <- cbind.data.frame(
  #check min BM outflow from offc
  scen_res %>%
    dplyr::filter(Variable == "FlamingGorge.Outflow") %>%
    dplyr::filter(Scenario == my_scens[1])  %>% 
    dplyr::group_by(MonthNum) %>%
    summarise(min = min(Value)),
  
  #check min BM outflow from fix
  scen_res %>%
    dplyr::filter(Variable == "FlamingGorge.Outflow") %>%
    dplyr::filter(Scenario == my_scens[2])  %>% 
    dplyr::group_by(MonthNum) %>%
    summarise(min = min(Value)),
  
  #no mins in update 
  
  #check min BM outflow from offc
  scen_res %>%
    dplyr::filter(Variable == "FlamingGorge.Pool Elevation") %>%
    dplyr::filter(Scenario == my_scens[1])  %>% 
    dplyr::group_by(MonthNum) %>%
    summarise(min = min(Value)),
  
  #check min BM outflow from fix
  scen_res %>%
    dplyr::filter(Variable == "FlamingGorge.Pool Elevation") %>%
    dplyr::filter(Scenario == my_scens[2])  %>% 
    dplyr::group_by(MonthNum) %>%
    summarise(min = min(Value))
  #,
  #
  # scen_res %>%
  #   dplyr::filter(Variable == "Powell.Inflow") %>%
  #   dplyr::filter(Scenario == my_scens[1])  %>% 
  #   dplyr::group_by(MonthNum) %>%
  #   summarise(min = min(Value)),
  # 
  # #check min BM outflow from fix
  # scen_res %>%
  #   dplyr::filter(Variable == "Powell.Inflow") %>%
  #   dplyr::filter(Scenario == my_scens[2])  %>% 
  #   dplyr::group_by(MonthNum) %>%
  #   summarise(min = min(Value))
)

# print(minvals[,c(1,2,4,6,8,10,12)])
# write.csv(minvals[,c(1,2,4,6,8,10,12)],file = paste0(oFigs,'/BMFix_mins.csv'))

print(minvals[,c(1,2,4,6,8)])
write.csv(minvals[,c(1,2,4,6,8)],file = paste0(oFigs,'/FGDev_mins.csv'))


#check min BM outflow from offc
scen_res_FG <- scen_res %>%
  dplyr::filter(Variable == "FlamingGorge.Pool Elevation") #%>%
View(scen_res_FG)
