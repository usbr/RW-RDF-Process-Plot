##############################################################################
#Replaced by Verification_GageDemands.R
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
# scen_dir <- file.path(CRSSDIR,"Scenario") 
# #containing the sub folders for each ensemble

results_dir <- file.path(CRSSDIR,"results") 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


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

# # check folders
# if(!file.exists(file.path(scen_dir, scens[1]))
#    | !file.exists(file.path(scen_dir, scens[2])))
#   stop('Scenarios folder(s) do not exist or scen_dir is set up incorrectly.
#        Please ensure Scenarios is set correctly.')
# 
# ofigs <- file.path(results_dir,mainScenGroup) 
# if (!file.exists(ofigs)) {
#   message(paste('Creating folder:', ofigs))
#   dir.create(ofigs)
# }
# 
# message('Figures will be saved to: ', ofigs)
message('Figures will be saved to: ', results_dir)


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#file names 
Figs <- paste0('VerificationRun_Residuals','.pdf')

#agg file specifying which slots
rw_agg_file <- "VerificationRun_rwagg.csv"
#read agg file specifying which slots
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) #ubres.rdf res.rdf

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
df_monthly <- rdf_aggregate(rwa1, 
  rdf_dir = results_dir
 ) #%>% 

# unique(df_monthly$Variable) #check variable names 

#get everything on a date 
df_monthly$Date = as.Date(paste0(df_monthly$Year,df_monthly$Month,"01"), format = "%Y%B%d")
#get a numeric month number
df_monthly$MonthNum = as.numeric(format.Date(df_monthly$Date, format = "%m"))

rw_agg_file <- "VerificationRun_rwagg_annual.csv"
#read agg file specifying which slots
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) #ubres.rdf res.rdf

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
df_annual <- rdf_aggregate(rwa1, 
                          rdf_dir = results_dir
) #%>% 


outflows <- c ("1_Simulated_UpperColoradoReach", "2_Simulated_UpperColoradoAboveCameo","3_Simulated_TaylorAboveBlueMesa",
            "6_Simulated_GunnisonRiverAboveGrandJunction","7_Simulated_DoloresRiver","8_Simulated_DoloresColorado",
            "9_Simulated_GreenRAboveFontenelle","10_Simulated_GreenRAboveGreenRiverWY","11_Simulated_GreenRAboveFlamingGorge",
            "12_Simulated_LittleSnakeRiver","13_Simulated_YampaRiver","14_Simulated_DuchesneBelowStarv",
            "15_Simulated_WhiteRiverAboveWatson","16_Simulated_GreenRWhiteToSanRafael","17_Simulated_SanRafaelRiver",
            "18_Simulated_SJAboveNavajo","19_Simulated_LowerSanJuanRiver","20_Simulated_SanJuanPowell")

gages <- c("1_Gage_ColoradoNearGlenwoodSprings","2_Gage_ColoradoNearCameo","3_Gage_BlueMesa", 
           "6_Gage_GunnisonNearGrandJunction",  "7_Gage_DoloresNearCisco", "8_Gage_ColoradoNearCisco", 
           "9_Gage_Fontenelle","10_Gage_GreenAtGreenRiverWY","11_Gage_FlamingGorge",
           "12_Gage_LittleSnakeNearLily","13_Gage_YampaNearMaybell","14_Gage_DuchesneNearRandlett",
           "15_Gage_WhiteNearWatson", "16_Gage_GreenAtGreenRiverUT","17_Gage_SanRafaelNearGreenRiverUT",
           "18_Gage_SanJuanNearArchuleta","19_Gage_SanJuanNearBluff","20_Gage_Powell")   

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## create a pdf  
# pdf(file.path(ofigs,Figs), width=9, height=6)
pdf(file.path(results_dir,Figs), width=9, height=6)



# # Get into KAF 
# df_monthly <- df_monthly %>%
#   dplyr::mutate(Value = Value/1000) 
# 
# df_annual <- df_annual %>%
#   dplyr::mutate(Value = Value/1000) 

y_lab_yr = "Flow (ac-ft/yr)"
y_lab_mon = "Flow (ac-ft/mo)"


for(i in 1:length(gages)){
# for(i in 7:8){
    

  # i=1
  
  title = paste(outflows[i])
  
  # which(df_annual$Variable == gages[i])
  
  
  
  #annual plot
  p <- df_annual %>%
    dplyr::filter(Variable == gages[i] | Variable == outflows[i]) %>%
    dplyr::group_by(Year, Variable) %>%
    ggplot(aes(x = Year, y = Value, color = Variable)) +
    geom_line() +
    # scale_x_discrete("Month",labels = month.abb) + #display abb. month names
    labs(title = title, y = y_lab_yr)
  print(p)
  
  #calculate residual
  gage <- df_annual %>%
    dplyr::filter(Variable == gages[i])
  simulated <- df_annual %>%
    dplyr::filter(Variable == outflows[i])
  diff <- gage
  diff$Value = simulated$Value - gage$Value
  diff$Variable = rep("Residual",times = length(diff$Variable))
  
  #annual residual
  p <- diff %>%
    dplyr::group_by(Year, Variable) %>%
    ggplot(aes(x = Year, y = Value, color = Variable)) +
    geom_line() +
    # scale_x_discrete("Month",labels = month.abb) + #display abb. month names
    labs(title = title, y = y_lab_yr)
  print(p)
  
  #annual metrics 
  mae <- sum(abs(diff$Value))/length(diff$Value)
  bias <- sum(diff$Value)/length(diff$Value)
  error_perc <- mae/mean(gage$Value)
  
  
  print(paste(title,"mae",mae,"bais",bias,"error % of gage",error_perc*100))
  
  
  #create a sperate matrix of annual stats to store the % of gage erorr 
  if(i==1){
    annstats <- array(c(outflows[i],mae,bias,error_perc*100)) #c(outflows[i],mae,bias,error_perc*100)
  } else {
    annstats <- rbind(annstats,c(outflows[i],mae,bias,error_perc*100))
  }


  #### monthly #####  
  
  #monthly flows
  p <- df_monthly %>%
    dplyr::filter(Variable == gages[i] | Variable == outflows[i]) %>%
    dplyr::group_by(Date, Variable) %>%
    # dplyr::mutate(Value = Value/1000) %>% 
    ggplot(aes(x = Date, y = Value, color = Variable)) +
    geom_line() +
    labs(title = title, y = y_lab_mon)
  print(p)
  
  #calculate residual
  gage <- df_monthly %>%
    dplyr::filter(Variable == gages[i])
  simulated <- df_monthly %>%
    dplyr::filter(Variable == outflows[i])
  diff <- gage
  diff$Value = simulated$Value - gage$Value
  diff$Variable = rep("Residual",times = length(diff$Variable))
  
  #monthly residual
  p <- diff %>%
    dplyr::group_by(Date, Variable) %>%
    ggplot(aes(x = Date, y = Value, color = Variable)) +
    geom_line() +
    # scale_x_discrete("Month",labels = month.abb) + #display abb. month names
    labs(title = title, y = y_lab_mon)
  print(p)
  
  
  metrics <- diff %>%
    dplyr::group_by(MonthNum) %>%
    summarise('MAE' = mean(abs(Value)),'Bias' = mean(Value)) #%>%
  # metrics  
  
  ann_metrics <- metrics[1,] 
  ann_metrics[1,] = as.list(c(0,mae,bias))
  # ann_metrics
  
  
  metrics = rbind.data.frame(metrics,ann_metrics)
  
  write.csv(metrics,file = file.path(results_dir,paste0(gages[i],".csv")))
  
} # close loop i through gages 

dev.off() 

colnames(annstats) <- c("Reach","MAE","Bais","Error % of gage")
write.csv(annstats,file = file.path(results_dir,paste0("AnnualVerificationStats.csv")))





filter

head(cowu)



### old below ##########


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
  dplyr::filter(Year <= last(yrs2show)) #%>% #one run has 2023 so filter that out so axis work
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





variable = "GunnisonNearGrandJunction.AFOutflow"
y_lab = "Monthly Flow (ac-ft/mo)"
title = paste(variable,first(yrs2show),"-",last(yrs2show))
DryTarget <- data.frame(yintercept=55340)
AvgDryTarget <- data.frame(yintercept=496200)
ModWetTarget <- data.frame(yintercept=882350)

p <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  geom_hline(aes(yintercept=yintercept), data=DryTarget) +
  geom_hline(aes(yintercept=yintercept), data=AvgDryTarget) +
  geom_hline(aes(yintercept=yintercept), data=ModWetTarget) +
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

pdf(file.path(ofigs,paste0("FGAsp_HClass_",startyr,endyr,'.pdf')), width=9, height=6)


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



dev.off()

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


startyr = 2021 #filter out all years > this year
endyr = 2060 #2060
yrs2show <- startyr:endyr # can't use this until your run extends to end of 2023

## create a pdf  
pdf(file.path(ofigs,paste0("FGOut_",startyr,endyr,'.pdf')), width=9, height=6)

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

dev.off()
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
############## H Class ########
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


startyr = 2021 #filter out all years > this year
endyr = 2060 #2060
yrs2show <- startyr:endyr # can't use this until your run extends to end of 2023


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






