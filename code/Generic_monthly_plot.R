##############################################################################
#This script creates monthly boxplots of Outflow and PE to compare two CRSS runs

#CONSTRUCTION OF THIS HAS NOT BEEN COMPLETE

#Contents 
## 1. Set Up ##
## 2. User Input ##
## 3. Process Results ## 
## 4. Plot ## 

#   Created by C. Felletter 8/2018
##############################################################################

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls()) #clear the enviornment 
library(RWDataPlyr)

## b) Directory Set Up
MTOMDIR <- Sys.getenv("MTOM_DIR") #Use env variable 

# where scenarios are folder are kept
scen_dir = file.path(MTOMDIR,"Output Data","RDF Process") 
#containing the sub folders for each ensemble

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

scenarios <- list(
  "PreviousRun" = "PreviousRun", 
  "CurrentRun" = "CurrentRun"
)

first_ensemble = c(2,2) #filter out Most,Min,Max. For 38 trace offical = 4, 
#36 trace month w Most = 2. Same order as for scenarios  

#rdf file with slot you want 
file = "res.rdf" 

rdf_slot_names(read_rdf(iFile = file.path(iFolder,scens[1],file))) #check slots in rdf

variables = c("Mead.Pool Elevation")
y_lab = "End of Month PE (ft)"


variables = c("Powell.Outflow")
y_lab = "Monthly Flow (ac-ft/mo)"


#plot inputs 
y_lab = "Daily Flow (cfs)"
filteryrlessorequal = 2026 #filter out all years > this year
plotfoldertitle = "FG Dev" #folder to create for output in results dir
plotitle = "FG_Daily_Plots" #objectslot + .pdf will be added when creating plots 


#agg file specifying which slots
rw_agg_file <- "rw_agg_UBdev_monthly.csv"

#file names 
Figs <- 'Generic_MonthlyResFigs.pdf' 

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

if (!file.exists(file.path(scen_dir, 'results'))) {
  message(paste(file.path(scen_dir, 'results'),
                'does not exist. Creating this folder...'))
  dir.create(file.path(scen_dir, 'results'))
}

oFigs <- file.path(scen_dir,'results') 
if (!file.exists(oFigs)) {
  message(paste('Creating folder:', oFigs))
  dir.create(oFigs)
}

message('Figures and tables will be saved to: ', oFigs)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#read agg file specifying which slots
rwa1 <- rwd_agg(read.csv(file.path(MTOMDIR,"Output Data","RDF Process", rw_agg_file), stringsAsFactors = FALSE)) 

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

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## create a pdf  
pdf(file.path(oFigs,Figs), width=9, height=6)

variable = "Crystal.Outflow"
y_lab = "Monthly Flow (ac-ft/mo)"
title = paste(variable,"2019-2026")

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = title, y = y_lab) 

variable = "Crystal.Pool Elevation"
y_lab = "End of Month PE (ft)"
title = paste(variable,"2019-2026")
PEtarget <- data.frame(yintercept=6753.04) 

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  geom_hline(aes(yintercept=yintercept), data=PEtarget) +
  labs(title = title, y = y_lab) 

variable = "BlueMesa.Outflow"
y_lab = "Monthly Flow (ac-ft/mo)"
title = paste(variable,"2019-2026")

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = title, y = y_lab) 

variable = "BlueMesa.Pool Elevation"
y_lab = "End of Month PE (ft)"
title = paste(variable,"2019-2026")
Jan31PEtarget <- data.frame(yintercept=7487) 
Feb31PEtarget <- data.frame(yintercept=7485) 
Mar31PEtarget <- data.frame(yintercept=7484.5) 
Apr31PEtarget <- data.frame(yintercept=7491) 
MayNov31PEtarget <- data.frame(yintercept=7495) 
JunJul31PEtarget <- data.frame(yintercept=7516.4) 
Aug31PEtarget <- data.frame(yintercept=7507) 
Sep31PEtarget <- data.frame(yintercept=7498) 
Oct31PEtarget <- data.frame(yintercept=7496.5) 
Dec31PEtarget <- data.frame(yintercept=7490) 

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  geom_hline(aes(yintercept=yintercept), data=Jan31PEtarget) +
  geom_hline(aes(yintercept=yintercept), data=Feb31PEtarget) +
  geom_hline(aes(yintercept=yintercept), data=Mar31PEtarget) +
  geom_hline(aes(yintercept=yintercept), data=Apr31PEtarget) +
  geom_hline(aes(yintercept=yintercept), data=MayNov31PEtarget) +
  geom_hline(aes(yintercept=yintercept), data=JunJul31PEtarget) +
  geom_hline(aes(yintercept=yintercept), data=Aug31PEtarget) +
  geom_hline(aes(yintercept=yintercept), data=Sep31PEtarget) +
  geom_hline(aes(yintercept=yintercept), data=Oct31PEtarget) +
  geom_hline(aes(yintercept=yintercept), data=Dec31PEtarget) +
  labs(title = title, y = y_lab) 

variable = "GunnisonNearGrandJunction.AFOutflow"
y_lab = "Monthly Flow (ac-ft/mo)"
title = paste(variable,"2019-2026")
DryTarget <- data.frame(yintercept=55340) 
AvgDryTarget <- data.frame(yintercept=496200)
ModWetTarget <- data.frame(yintercept=882350) 

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  geom_hline(aes(yintercept=yintercept), data=DryTarget) +
  geom_hline(aes(yintercept=yintercept), data=AvgDryTarget) +
  geom_hline(aes(yintercept=yintercept), data=ModWetTarget) +
  labs(title = title, y = y_lab) 


variable = "Fontenelle.Outflow"
y_lab = "Monthly Flow (ac-ft/mo)"
title = paste(variable,"2019-2026")

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = title, y = y_lab) 


variable = "Fontenelle.Pool Elevation"
y_lab = "End of Month PE (ft)"
title = paste(variable,"2019-2026")
Apr1PEtarget <- data.frame(yintercept=6468) 
Jul1PEtarget <- data.frame(yintercept=6500) 
Aug1PEtarget <- data.frame(yintercept=6505.5) 

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  geom_hline(aes(yintercept=yintercept), data=Apr1PEtarget) +
  geom_hline(aes(yintercept=yintercept), data=Jul1PEtarget) +
  geom_hline(aes(yintercept=yintercept), data=Aug1PEtarget) +
  labs(title = title, y = y_lab) 

variable = "FlamingGorge.Outflow"
y_lab = "Monthly Flow (ac-ft/mo)"
title = paste(variable,"2019-2026")

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = title, y = y_lab) 


variable = "FlamingGorge.Pool Elevation"
y_lab = "End of Month PE (ft)"
title = paste(variable,"2019-2026")
MaxPEtarget <- data.frame(yintercept=6039) 

scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= 2026) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) + 
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  geom_hline(aes(yintercept=yintercept), data=MaxPEtarget) +
  labs(title = title, y = y_lab) 


dev.off()
