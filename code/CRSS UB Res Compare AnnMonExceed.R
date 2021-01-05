##############################################################################
#This script creates monthly boxplots of Outflow and PE to compare two MTOM runs
##############################################################################

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# rm(list=ls()) #clear the enviornment 
# 
# ### Directory Set Up
# # where scenarios are folder are kept
# scen_dir = file.path(getwd(),"scenarios") 
# #containing the sub folders for each ensemble

CRSSDIR <- Sys.getenv("CRSS_DIR")

# # where scenarios are folder are kept
scen_dir <- file.path(CRSSDIR,"Scenario")
# #containing the sub folders for each ensemble

results_dir <- file.path(CRSSDIR,"results") 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#generic scenario locations 

#Demand Redesign Model 
scens <- list(
  "v1_FGhistoricalfct_Full" = "FebRedesign9013,DNF,2016Dems,IG_DCP.9005,MTOM_Most - 1.FGhistoricalfct",
  # "v1_FGhistoricalfct_Stress" = "FebRedesign9013,ISM1988_2018,2016Dems,IG_DCP.9005,MTOM_Most - 1.FGhistoricalfct",
  "v2_RW81_Full" = "FebRedesign9013,DNF,2016Dems,IG_DCP.9006,MTOM_Most - 2.RW 8.1")
  # "v2_RW81_Stress" = "FebRedesign9013,ISM1988_2018,2016Dems,IG_DCP.9006,MTOM_Most - 2.RW 8.1"),
  # "v3_UBRes_Stress" = "FebRedesign9013_v3,ISM1988_2018,2016Dems,IG_DCP.9006_v3,MTOM_Most - 3.UBRes",
  # "v3_UBRes_Full" = "FebRedesign9013_v3,DNF,2016Dems,IG_DCP.9006_v3,MTOM_Most - 3.UBRes")#,
  # "v4_PowMon_Full" = "FebRedesign9013_v4,DNF,2016Dems,IG_DCP.9006_v3,MTOM_Most -4.PowMon")

mainScenGroup <<- names(scens)[2] #name of the subfolder this analysis will be stored

# Figs <- paste0(mainScenGroup,'_Checks.pdf') ## not used for anything 

#### Plot Controls #####
printfigs_monthly<-F#T#make png figures 
printfigs_daily<-F#T#make png figures since pdfs take FOREVER to open
printfigs_exceed<-F#T#make png figures 

# mylinetypes <- c("dashed","solid","solid")
#standard powerpoint figure sizes 
# first is for monthly plots, second is for daily plots 
widths <- c(9.5,9.5) #smaller looks really bad, better to just resize larger image
heights <- c(7,7)

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
source('C:/Users/cfelletter/Documents/RW-RDF-Process-Plot/code/Stat_emp_ExcCrv.r')
source('C:/Users/cfelletter/Documents/RW-RDF-Process-Plot/code/stat-boxplot-custom.r')

# check folders
if(!file.exists(file.path(scen_dir, scens[1]))
   | !file.exists(file.path(scen_dir, scens[2])))
  stop('Scenarios folder(s) do not exist or scen_dir is set up incorrectly.
       Please ensure Scenarios is set correctly.')

ofigs <- file.path(results_dir,mainScenGroup) 
if (!file.exists(ofigs)) {
  message(paste('Creating folder:', ofigs))
  dir.create(ofigs)
}

message('Figures will be saved to: ', ofigs)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#file names 
startyr = 2021 #filter out all years > this year
endyr = 2040 #2060

yrs2show <- startyr:endyr # can't use this until your run extends to end of 2023

#agg file specifying which slots
rw_agg_file <- "rw_agg_UBdev_monthly_CRSS.csv"
#read agg file specifying which slots
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) #ubres.rdf res.rdf

#load a feather if we already processed
if (any(list.files(ofigs) == "res.feather")){   #exists(file.path(ofigs,"res.feather"))) { - not sure why this doesn't work 
  scen_res <- feather::read_feather(file.path(ofigs,"res.feather"))
} else {
  scen_res <- rw_scen_aggregate(
    scens,
    agg = rwa1,
    scen_dir = scen_dir,
    file = file.path(ofigs,"res.feather")
  )
}

unique(scen_res$Variable) #check variable names 

#get everything on a date 
scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
#get a numeric month number
scen_res$MonthNum = as.numeric(format.Date(scen_res$MonthNum, format = "%m"))

#filter out incomplete years
# scen_res <- scen_res %>%
#     dplyr::filter(Year >= first(yrs2show)) %>%
#     dplyr::filter(Year <= last(yrs2show))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Plot UB PE/Out vs Month Boxplot Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Figs <- paste0('MonthlyResFigs_',startyr,endyr,'.pdf')

if(T){

## create a pdf  
pdf(file.path(ofigs,Figs), width=9, height=6)

  
  variable = "TaylorPark.Pool Elevation"
  y_lab = "End of Month PE (ft)"
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
  
  variable = "TaylorPark.Outflow"
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
  
variable = "BlueMesa.Pool Elevation"
y_lab = "End of Month PE (ft)"
title = paste(variable,first(yrs2show),"-",last(yrs2show))
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

p <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
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
print(p)

variable = "BlueMesa.Outflow"
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


variable = "MorrowPoint.Pool Elevation"
y_lab = "End of Month PE (ft)"
title = paste(variable,first(yrs2show),"-",last(yrs2show))
target <- data.frame(yintercept=7153.73)

p <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  geom_hline(aes(yintercept=yintercept), data=target) +
  labs(title = title, y = y_lab)
print(p)

variable = "MorrowPoint.Outflow"
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

variable = "Crystal.Pool Elevation"
y_lab = "End of Month PE (ft)"
title = paste(variable,first(yrs2show),"-",last(yrs2show))
PEtarget <- data.frame(yintercept=6753.04)

p <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  geom_hline(aes(yintercept=yintercept), data=PEtarget) +
  labs(title = title, y = y_lab)
print(p)

variable = "Crystal.Outflow"
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

# variable = "GunnisonNearGrandJunction.AFOutflow"
# y_lab = "Monthly Flow (ac-ft/mo)"
# title = paste(variable,first(yrs2show),"-",last(yrs2show))
# DryTarget <- data.frame(yintercept=55340)
# AvgDryTarget <- data.frame(yintercept=496200)
# ModWetTarget <- data.frame(yintercept=882350)

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

variable = "Fontenelle.Pool Elevation"
y_lab = "End of Month PE (ft)"
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

variable = "Fontenelle.Outflow"
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

p <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = title, y = y_lab)
print(p)

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

variable = "Navajo.Pool Elevation"
y_lab = "End of Month PE (ft)"
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

variable = "Navajo.Outflow"
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

variable = "Powell.Inflow"
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

variable = "Powell.Pool Elevation"
y_lab = "End of Month PE (ft)"
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

variable = "Powell.Outflow"
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

dev.off()

}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Plot UB PE/Out vs Year Lineplot Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

startyr = 2021 #filter out all years > this year
endyr = 2040 #2060
yrs2show <- startyr:endyr # can't use this until your run extends to end of 2023

Figs <- paste0('AnnualResFigs_',startyr,endyr,'.pdf')

if(T){
  
  ## create a pdf  
  pdf(file.path(ofigs,Figs), width=9, height=6)
  
  
  variable = "TaylorPark.Pool Elevation"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "EOCY Water Surface Elevation (ft)"
  exc_month = 12
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(MonthNum%in%exc_month) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
    dplyr::group_by(Scenario, Year) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
    geom_line() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    labs(title = paste("Average EOCY",title), y = y_lab, x = "Year")
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Mean EOCY",variable,".png")), width = widths[1],height = heights[1])}
  
  variable = "TaylorPark.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    ggplot(aes(x = Year, y = Value, color = Scenario)) +
    scale_x_continuous(breaks = 2021:2040) +
    geom_line() +
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = mycolors) +
    labs(title = paste("Average Annual",title), y = y_lab, x = "Year")
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}
  
  variable = "BlueMesa.Pool Elevation"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "EOCY Water Surface Elevation (ft)"
  exc_month = 12
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(MonthNum%in%exc_month) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
    dplyr::group_by(Scenario, Year) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
    geom_line() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    labs(title = paste("Average EOCY",title), y = y_lab, x = "Year")
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Mean EOCY",variable,".png")), width = widths[1],height = heights[1])}
  
  
  variable = "BlueMesa.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    ggplot(aes(x = Year, y = Value, color = Scenario)) +
    scale_x_continuous(breaks = 2021:2040) +
    geom_line() +
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = mycolors) +
    labs(title = paste("Average Annual",title), y = y_lab, x = "Year")
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}
  
  variable = "MorrowPoint.Pool Elevation"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "EOCY Water Surface Elevation (ft)"
  exc_month = 12
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(MonthNum%in%exc_month) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
    dplyr::group_by(Scenario, Year) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
    geom_line() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    labs(title = paste("Average EOCY",title), y = y_lab, x = "Year")
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Mean EOCY",variable,".png")), width = widths[1],height = heights[1])}
  
  variable = "MorrowPoint.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    ggplot(aes(x = Year, y = Value, color = Scenario)) +
    scale_x_continuous(breaks = 2021:2040) +
    geom_line() +
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = mycolors) +
    labs(title = paste("Average Annual",title), y = y_lab, x = "Year")
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}
  
  variable = "Crystal.Pool Elevation"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "EOCY Water Surface Elevation (ft)"
  exc_month = 12
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(MonthNum%in%exc_month) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
    dplyr::group_by(Scenario, Year) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
    geom_line() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    labs(title = paste("Average EOCY",title), y = y_lab, x = "Year")
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Mean EOCY",variable,".png")), width = widths[1],height = heights[1])}
  
  variable = "Crystal.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    ggplot(aes(x = Year, y = Value, color = Scenario)) +
    scale_x_continuous(breaks = 2021:2040) +
    geom_line() +
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = mycolors) +
    labs(title = paste("Average Annual",title), y = y_lab, x = "Year")
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}
  
  variable = "Fontenelle.Pool Elevation"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "EOCY Water Surface Elevation (ft)"
  exc_month = 12
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(MonthNum%in%exc_month) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
    dplyr::group_by(Scenario, Year) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
    geom_line() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    labs(title = paste("Average EOCY",title), y = y_lab, x = "Year")
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Mean EOCY",variable,".png")), width = widths[1],height = heights[1])}
  
  variable = "Fontenelle.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    ggplot(aes(x = Year, y = Value, color = Scenario)) +
    scale_x_continuous(breaks = 2021:2040) +
    geom_line() +
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = mycolors) +
    labs(title = paste("Average Annual",title), y = y_lab, x = "Year")
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}
  
  variable = "FlamingGorge.Pool Elevation"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "EOCY Water Surface Elevation (ft)"
  exc_month = 12
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(MonthNum%in%exc_month) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
    dplyr::group_by(Scenario, Year) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
    geom_line() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    labs(title = paste("Average EOCY",title), y = y_lab, x = "Year")
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Mean EOCY",variable,".png")), width = widths[1],height = heights[1])}
  
  variable = "FlamingGorge.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    ggplot(aes(x = Year, y = Value, color = Scenario)) +
    scale_x_continuous(breaks = 2021:2040) +
    geom_line() +
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = mycolors) +
    labs(title = paste("Average Annual",title), y = y_lab, x = "Year")
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}
  
  variable = "Navajo.Pool Elevation"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "EOCY Water Surface Elevation (ft)"
  exc_month = 12
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(MonthNum%in%exc_month) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
    dplyr::group_by(Scenario, Year) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
    geom_line() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    labs(title = paste("Average EOCY",title), y = y_lab, x = "Year")
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Mean EOCY",variable,".png")), width = widths[1],height = heights[1])}
  
  variable = "Navajo.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    ggplot(aes(x = Year, y = Value, color = Scenario)) +
    scale_x_continuous(breaks = 2021:2040) +
    geom_line() +
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = mycolors) +
    labs(title = paste("Average Annual",title), y = y_lab, x = "Year")
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}
  
  variable = "Powell.Inflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    ggplot(aes(x = Year, y = Value, color = Scenario)) +
    scale_x_continuous(breaks = 2021:2040) +
    geom_line() +
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = mycolors) +
    labs(title = paste("Average Annual",title), y = y_lab, x = "Year")
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}
  
  variable = "Powell.Pool Elevation"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "EOCY Water Surface Elevation (ft)"
  exc_month = 12
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(MonthNum%in%exc_month) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
    dplyr::group_by(Scenario, Year) %>%
    dplyr::summarise(Value = mean(Value)) %>%
    ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
    geom_line() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    labs(title = paste("Average EOCY",title), y = y_lab, x = "Year")
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Mean EOCY",variable,".png")), width = widths[1],height = heights[1])}
  
  variable = "Powell.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    ggplot(aes(x = Year, y = Value, color = Scenario)) +
    scale_x_continuous(breaks = 2021:2040) +
    geom_line() +
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = mycolors) +
    labs(title = paste("Average Annual",title), y = y_lab, x = "Year")
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}
  
  dev.off()
  
}


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Plot UB PE/Out Exceedance plot Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

startyr = 2021 #filter out all years > this year
endyr = 2040 #2060
yrs2show <- startyr:endyr # can't use this until your run extends to end of 2023
caption <- ''
expand <- F

Figs <- paste0('ExceedFigs_',startyr,endyr,'.pdf')
if(T){
  
  # #debug
  # write.csv(x = scen_res, file = file.path(ofigs,"scen_res.csv"))
  # write_rds(x = scen_res, path = file.path(ofigs,"scen_res.RDS"))
  # scen_res <- read_rds(file.path(ofigs,"scen_res.RDS"))
  
  ## create a pdf  
  pdf(file.path(ofigs,Figs), width=9, height=6)
  
  variable = "TaylorPark.Pool Elevation"
  # title = paste(variable,first(yrs2show),"-",last(yrs2show))
  # y_lab = "Max Water Surface Elevation (ft)"
  # caption <- 'Max PE for each Trace'
  # p <- scen_res %>%
  #   dplyr::filter(Variable == variable) %>%
  #   dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  #   dplyr::group_by(Scenario, TraceNumber) %>%
  #   mutate(Value = max(Value)) %>%
  #   dplyr::group_by(Scenario) %>%
  #   ggplot(aes(Value, color = Scenario)) +
  #   theme_light() + 
  #   stat_eexccrv() +
  #   # scale_color_manual(values = mycolors) +
  #   labs(title = title,
  #        y = y_lab, caption = caption) +
  #   # scale_y_continuous(breaks=seq(ylims[1],ylims[2],40)) + 
  #   scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
  #   theme(plot.caption = element_text(hjust = 0)) #left justify 
  # print(p)
  # if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 1',title,".png")), width = widths[1],height = heights[1])}

  variable = "TaylorPark.Pool Elevation"
  y_lab = "EOCY Water Surface Elevation (ft)"
  title = paste("EOCY",variable,first(yrs2show),"-",last(yrs2show))
  caption <- ''
  exc_month = 12
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() +
    # scale_color_manual(values = mycolors) +
    # scale_y_continuous(breaks=seq(ylims[1],ylims[2],40)) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    # ylim(ylims) +# geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 2',title,".png")), width = widths[1],height = heights[1])}
  
  variable = "TaylorPark.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    dplyr::group_by(Scenario) %>% 
    ggplot(aes(Value, color = Scenario)) +
    stat_eexccrv() +  
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify  
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 9',title,".png")), width = widths[1],height = heights[1])}
  
  variable = "BlueMesa.Pool Elevation"
  y_lab = "EOCY Water Surface Elevation (ft)"
  title = paste("EOCY",variable,first(yrs2show),"-",last(yrs2show))
  caption <- ''
  exc_month = 12
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() +
    # scale_color_manual(values = mycolors) +
    # scale_y_continuous(breaks=seq(ylims[1],ylims[2],40)) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    # ylim(ylims) +# geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 2',title,".png")), width = widths[1],height = heights[1])}
  
  variable = "BlueMesa.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    dplyr::group_by(Scenario) %>% 
    ggplot(aes(Value, color = Scenario)) +
    stat_eexccrv() +  
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify  
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 9',title,".png")), width = widths[1],height = heights[1])}
  
  variable = "MorrowPoint.Pool Elevation"
  y_lab = "EOCY Water Surface Elevation (ft)"
  title = paste("EOCY",variable,first(yrs2show),"-",last(yrs2show))
  caption <- ''
  exc_month = 12
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() +
    # scale_color_manual(values = mycolors) +
    # scale_y_continuous(breaks=seq(ylims[1],ylims[2],40)) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    # ylim(ylims) +# geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 2',title,".png")), width = widths[1],height = heights[1])}
  
  variable = "MorrowPoint.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    dplyr::group_by(Scenario) %>% 
    ggplot(aes(Value, color = Scenario)) +
    stat_eexccrv() +  
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify  
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 9',title,".png")), width = widths[1],height = heights[1])}
  
  variable = "Crystal.Pool Elevation"
  y_lab = "EOCY Water Surface Elevation (ft)"
  title = paste("EOCY",variable,first(yrs2show),"-",last(yrs2show))
  caption <- ''
  exc_month = 12
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() +
    # scale_color_manual(values = mycolors) +
    # scale_y_continuous(breaks=seq(ylims[1],ylims[2],40)) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    # ylim(ylims) +# geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 2',title,".png")), width = widths[1],height = heights[1])}
  
  variable = "Crystal.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    dplyr::group_by(Scenario) %>% 
    ggplot(aes(Value, color = Scenario)) +
    stat_eexccrv() +  
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify  
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 9',title,".png")), width = widths[1],height = heights[1])}
  
  variable = "Fontenelle.Pool Elevation"
  y_lab = "EOCY Water Surface Elevation (ft)"
  title = paste("EOCY",variable,first(yrs2show),"-",last(yrs2show))
  caption <- ''
  exc_month = 12
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() +
    # scale_color_manual(values = mycolors) +
    # scale_y_continuous(breaks=seq(ylims[1],ylims[2],40)) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    # ylim(ylims) +# geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 2',title,".png")), width = widths[1],height = heights[1])}
  
  variable = "Fontenelle.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    dplyr::group_by(Scenario) %>% 
    ggplot(aes(Value, color = Scenario)) +
    stat_eexccrv() +  
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify  
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 9',title,".png")), width = widths[1],height = heights[1])}
  
  variable = "FlamingGorge.Pool Elevation"
  y_lab = "EOCY Water Surface Elevation (ft)"
  title = paste("EOCY",variable,first(yrs2show),"-",last(yrs2show))
  caption <- ''
  exc_month = 12
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() +
    # scale_color_manual(values = mycolors) +
    # scale_y_continuous(breaks=seq(ylims[1],ylims[2],40)) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    # ylim(ylims) +# geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 2',title,".png")), width = widths[1],height = heights[1])}
  
  variable = "FlamingGorge.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    dplyr::group_by(Scenario) %>% 
    ggplot(aes(Value, color = Scenario)) +
    stat_eexccrv() +  
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify  
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 9',title,".png")), width = widths[1],height = heights[1])}
  
  variable = "Navajo.Pool Elevation"
  y_lab = "EOCY Water Surface Elevation (ft)"
  title = paste("EOCY",variable,first(yrs2show),"-",last(yrs2show))
  caption <- ''
  exc_month = 12
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() +
    # scale_color_manual(values = mycolors) +
    # scale_y_continuous(breaks=seq(ylims[1],ylims[2],40)) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    # ylim(ylims) +# geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 2',title,".png")), width = widths[1],height = heights[1])}
  
  variable = "Navajo.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    dplyr::group_by(Scenario) %>% 
    ggplot(aes(Value, color = Scenario)) +
    stat_eexccrv() +  
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify  
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 9',title,".png")), width = widths[1],height = heights[1])}
  
  variable = "Powell.Inflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  # exc_month <- c(10,11) # Oct - Nov
  # title <- paste(var_title_alt_name,month.name[first(exc_month)],'-',month.name[last(exc_month)])
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    dplyr::group_by(Scenario) %>% 
    ggplot(aes(Value, color = Scenario)) +
    stat_eexccrv() +  
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify  
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 9',title,".png")), width = widths[1],height = heights[1])}
  
  variable = "Powell.Pool Elevation"
  y_lab = "EOCY Water Surface Elevation (ft)"
  title = paste("EOCY",variable,first(yrs2show),"-",last(yrs2show))
  caption <- ''
  exc_month = 12
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() +
    # scale_color_manual(values = mycolors) +
    # scale_y_continuous(breaks=seq(ylims[1],ylims[2],40)) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    # ylim(ylims) +# geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 2',title,".png")), width = widths[1],height = heights[1])}
  
  variable = "Powell.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
    dplyr::group_by(Scenario) %>% 
    ggplot(aes(Value, color = Scenario)) +
    stat_eexccrv() +  
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify  
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 9',title,".png")), width = widths[1],height = heights[1])}
  
  dev.off()
  
}


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Plot Powell Monthly Outflow Exceedance plot Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Figs <- paste0('ExceedFigs_PowellMonthlyOut',startyr,endyr,'.pdf')
if(T){
  
  # #debug
  # write.csv(x = scen_res, file = file.path(ofigs,"scen_res.csv"))
  # write_rds(x = scen_res, path = file.path(ofigs,"scen_res.RDS"))
  # scen_res <- read_rds(file.path(ofigs,"scen_res.RDS"))
  
  ## create a pdf  
  pdf(file.path(ofigs,Figs), width=9, height=6)
  
  variable = "Powell.Outflow"
  y_lab = "Monthly Flow (1,000 ac-ft/yr)"
  
  for (exc_month in c(10:12,1:9)) {
    title = paste(variable,month.abb[exc_month],first(yrs2show),"-",last(yrs2show))
    p <- scen_res %>%
      dplyr::filter(Variable == variable) %>%
      mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
      dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
      dplyr::filter(MonthNum%in%exc_month) %>% #filter month 
      dplyr::group_by(Scenario,Year)  %>%
      summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
      dplyr::group_by(Scenario) %>% 
      ggplot(aes(Value, color = Scenario)) +
      stat_eexccrv() +  
      theme_light() + 
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
      labs(title = title,
           y = y_lab, caption = caption) +
      theme(plot.caption = element_text(hjust = 0)) #left justify  
    print(p)
  }
  

  dev.off()
  
}



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
############## H Class ########
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


startyr = 2021 #filter out all years > this year
endyr = 2060 #2060
yrs2show <- startyr:endyr # can't use this until your run extends to end of 2023

if(T){
rw_agg_file <- "rw_agg_UBdev_HClass.csv" # HClass 

rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) 

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
) #%>% 

unique(scen_res$Variable) #check variable names 
#get everything on a date 
scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
#get a numeric month number
scen_res$MonthNum = as.numeric(format.Date(scen_res$MonthNum, format = "%m"))


pdf(file.path(ofigs,paste0("FGAsp_HClass_",startyr,endyr,'.pdf')), width=9, height=6)

variable = "FlamingGorgeData.BaseFlowHClass" #1] "FlamingGorgeData.BaseFlowHClass" "FlamingGorgeData.SpringHClass"  
#[3] "FlamingGorgeData.YampaHClass"    "BlueMesaData.GunnisonHClass"    
y_lab = "H Class"
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

variable = "FlamingGorgeData.SpringHClass"  
#[3] "FlamingGorgeData.YampaHClass"    "BlueMesaData.GunnisonHClass"    
y_lab = "H Class"
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

variable = "FlamingGorgeData.YampaHClass"  
y_lab = "H Class"
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

variable = "BlueMesaData.GunnisonHClass"  
y_lab = "H Class"
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

dev.off()
}
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##############  ########
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5. Check Min
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

variables <- c("BlueMesa.Outflow","MorrowPoint.Outflow","Crystal.Outflow") #RW Object.Slot
variables <- c("Fontenelle.Pool Elevation",	"FlamingGorge.Pool Elevation",	"FlamingGorge.Outflow",	"Fontenelle.Outflow","Powell.Inflow") #RW Object.Slot
variables <- c("BlueMesa.Pool Elevation",	"Crystal.Pool Elevation",	"Fontenelle.Pool Elevation",	"FlamingGorge.Pool Elevation",	"FlamingGorge.Outflow",	"Fontenelle.Outflow",	"BlueMesa.Outflow",	"Crystal.Outflow",	"MorrowPoint.Pool Elevation",	"MorrowPoint.Outflow",	"GunnisonNearGrandJunction.AFOutflow",	"Navajo.Pool Elevation",	"Navajo.Outflow",	"Powell.Inflow") #RW Object.Slot


for(j in 1:length(variables)){
  
  variable <- variables[j]
  
  message(paste("Finding <= 0 values of",variable))
  
  for (i in 1:length(scens)){
    min <- scen_res %>% 
      filter(Scenario == names(scens[i]))  %>%
      filter(Variable == variable) %>% # may need to replace this row wit Variable == "your variable"
      group_by(Scenario, Month, TraceNumber) %>%
      summarise(min = min(Value)) %>%
      filter(min <= 0)  
    
    
    if (i == 1){
      min0chk <- cbind.data.frame(min)
    } else if (i > 1 | is.data.frame(min0chk)){
      min0chk <- rbind.data.frame(min0chk,
                                  cbind.data.frame(min))
    }
    
  } #closes scen loop
  
  var_nospacenocol <- gsub(":", ".", gsub("[[:space:]]", "", variable)) 
  #remove var name spaces and any colons  
  
  if(dim(min0chk)[1] != 0){
    write.csv(min0chk,file = paste0(ofigs,'/min0chk_',var_nospacenocol,'.csv'))
  } else {
    message(paste(variable,"contains no <= 0 values"))
  }
}






