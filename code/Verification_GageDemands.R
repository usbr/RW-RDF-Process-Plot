#### Combined Gage and Use Verificaiton Script ########
#CF,20210824: Improve to not use VerificationGage.rdf and instead take observed
# from a xlsx sheet. Use R to annualize the data rather than RWDataPlyr
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls()) #clear the enviornment 

#Additional plotting functions and libraries 
library('tidyverse') #ggplot2,dplyr,tidyr
library('devtools')
library(RWDataPlyr)
library(CRSSIO)
library(xml2)

CRSSDIR <- Sys.getenv("CRSS_DIR")

results_dir <- file.path(CRSSDIR,"results") 
# # where rdf results folder is kept

#easier to make folder from output in the results dir than to move it 
scen_dir <- file.path(CRSSDIR,"results") #file.path(CRSSDIR,"Scenario")
# #containing the sub folders for each ensemble
scens <- "Verification_ShoshoneAug9003" 
# scens <- "2021Verification_AugCRSS_ReRun"
# scens <- '2020Verification_2016UCRC_CUL' #must change df_obs length to do 2000-2020

#### Plot Controls #####

printfigs<-T#T#make png figures and dump data 

y_lab_yr = "Flow (ac-ft/yr)"
y_lab_mon = "Flow (ac-ft/mo)"

mycolors <- c("#61bd17","#009E73","#6bbd28","#0072B2") #for Sector plots mid dark green - schedule depl, dark green - depl rqst, light green - depletion, blue - CUL, 
mylinetypes <- c("dotdash","dashed","solid","solid","dotdash")  #schedule depl, depl rqst, depletion, CUL, 

nyears <- length(2000:2019) #currently only used one place to calculate average

#standard powerpoint figure sizes 
# gage an, an gage resid, gage mon, mon gage resid, total use  
gage_widths <- c(9.5,9.5,9.5,9.5,9.5)
gage_heights <- c(7,7,7,7,7)
# sect an, sect mon, mon sect dist  
sect_widths <- c(5,6.5,5)
sect_heights <- c(3,4.2,3)

# #whole basin experiment figure sizes 
# # sect an, sect mon, mon sect dist  
sect_widths <- c(3.33,5,3.33)
sect_heights <- c(2,3,2)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### File Checks #####

file_dir <- file.path(scen_dir, scens[1])

# check folders
if(!file.exists(file_dir))
  stop('Scenarios folder(s) do not exist or scen_dir is set up incorrectly.
       Please ensure Scenarios is set correctly.')

ofigs <- file.path(results_dir,scens[1])
if (!file.exists(ofigs)) {
  message(paste('Creating folder:', ofigs,'move results rdfs into this dir and then proceed with code'))
  dir.create(ofigs)
  stop() #if created folder need to move results rdfs into this dir and then proceed with code
}

################################################################################
#### #### A. Read flow data  ####  ####
################################################################################

#agg file specifying which slots
rw_agg_file <- "VerificationRun_rwagg.csv"
#read agg file specifying which slots
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) #ubres.rdf res.rdf

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
df_monthly <- rdf_aggregate(rwa1, rdf_dir = file_dir) #MUST be in Scenario FOLDER! not results folder

#get everything on a date 
df_monthly$Date = as.Date(paste0(df_monthly$Year,df_monthly$Month,"01"), format = "%Y%B%d")
#get a numeric month number
df_monthly$MonthNum = as.numeric(format.Date(df_monthly$Date, format = "%m"))

#easier to do this in r than with rw_agg_file <- "VerificationRun_rwagg_annual.csv"
df_annual <-df_monthly %>% group_by(Variable,Year) %>%
  summarise(Value = sum(Value))

# ## create obs from excel not crss run ##
df_obs <- readxl::read_xlsx('data/Verification_Obs_Gage_Data_Linked.xlsx',sheet = "R_Input",col_names = T, ) 
# df_obs <- df_obs[1:228,] #only if need to limit it to 2000-2018
df_obs = df_obs %>% pivot_longer(cols=names(df_obs)[6:23],names_to = 'Variable',values_to = 'Value')

df_monthly <- rbind.data.frame(df_monthly,df_obs)

#annual data
df_obs_annual<-df_obs %>% group_by(Variable,Year) %>%
  summarise(Value = sum(Value))

df_annual <- rbind.data.frame(df_annual[,names(df_obs_annual)],df_obs_annual)

#fix issues with class
df_monthly <- as.data.frame(df_monthly)
df_annual <- as.data.frame(df_annual)

#use this later to filter CUL
years <- unique(df_annual$Year)

outflows <- c ("1_Simulated_UpperColoradoReach", "2_Simulated_UpperColoradoAboveCameo","3_Simulated_TaylorAboveBlueMesa",
               "6_Simulated_GunnisonRiverAboveGrandJunction","7_Simulated_DoloresRiver","8_Simulated_DoloresColorado",
               "9_Simulated_GreenRAboveFontenelle","10_Simulated_GreenRAboveGreenRiverWY","11_Simulated_GreenRAboveFlamingGorge",
               "12_Simulated_YampaRiver","13_Simulated_LittleSnakeRiver","14_Simulated_DuchesneBelowStarv",
               "15_Simulated_WhiteRiverAboveWatson","16_Simulated_GreenRWhiteToSanRafael","17_Simulated_SanRafaelRiver",
               "18_Simulated_SJAboveNavajo","19_Simulated_LowerSanJuanRiver","20_Simulated_SanJuanPowell")

gages <- c("1_Gage_ColoradoNearGlenwoodSprings","2_Gage_ColoradoNearCameo","3_Gage_BlueMesa", 
           "6_Gage_GunnisonNearGrandJunction",  "7_Gage_DoloresNearCisco", "8_Gage_ColoradoNearCisco", 
           "9_Gage_Fontenelle","10_Gage_GreenAtGreenRiverWY","11_Gage_FlamingGorge",
           "12_Gage_YampaNearMaybell","13_Gage_LittleSnakeNearLily","14_Gage_DuchesneNearRandlett",
           "15_Gage_WhiteNearWatson", "16_Gage_GreenAtGreenRiverUT","17_Gage_SanRafaelNearGreenRiverUT",
           "18_Gage_Navajo","19_Gage_SanJuanNearBluff","20_Gage_Powell")  #"18_Gage_SanJuanNearArchuleta"   


################################################################################
#### #### B. Read water use data  ####  ####
################################################################################
#### read attributes data to make key ####
Attributes <- read_xml(x="data/2016_Attributes.xml")
# Attributes <- read_xml(x="C:/Users/cfelletter/Documents/CRSS working/DemandUseVerificationRun/2007_Attributes.xml")

source("code/get_demand_atts.R") 
# summary(df)

#### read in all WUs and format data ####
allWU <- rbind.data.frame(rdf_to_rwtbl2(file.path(file_dir,"COWU.rdf")),
                          rdf_to_rwtbl2(file.path(file_dir,"NMWU.rdf")),
                          rdf_to_rwtbl2(file.path(file_dir,"WYWU.rdf")),
                          rdf_to_rwtbl2(file.path(file_dir,"UTWU.rdf")),
                          rdf_to_rwtbl2(file.path(file_dir,"AZWU.rdf")))

#split off agg div
tmp <- str_split(allWU$ObjectSlot, ":", n = 2, simplify = TRUE)
allWU['agg_div'] = tmp[,1]

#split off object.slot
tmp2 <- str_split(allWU$ObjectSlot, ".Deplet", n = 2, simplify = TRUE)
#### this might be messing up some objects
# unique(tmp2[,2])
# head(tmp2)

allWU['object'] = tmp2[,1]

allWU['Slot'] = paste0(rep("Deplet",length(tmp2[,2])),tmp2[,2]) #fix the d 

rm(tmp,tmp2)

### join with objects from atrributes look up tables ####
#join on Node and Tributary Attributes
allWU <- allWU %>% 
  full_join(lookup_div, by="agg_div")

#join on WU Attributes 
allWU <- allWU %>% 
  full_join(lookup_wu, by="object")

#get everything on a date 
allWU$Date = as.Date(paste0(allWU$Year,allWU$Month,"01"), format = "%Y%B%d")
#get a numeric month number - NOT SURE I NEED THIS 
allWU$MonthNum = as.numeric(format.Date(allWU$Date, format = "%m"))

# nodes <- sort(unique(df$Node)) #sort these better 
nodes <- c("1 Glenwood","2 Cameo","4 Blue Mesa","6 Grand Junction","7 Dolores",
           "8 CO River at Cisco","9 Fontenelle","10 Green River WY","11 Greendale",
           "12 Yampa","13 Little Snake","14 Duchesne","15 White River","16 Green River UT",
           "17 San Rafael","18 Archuleta","19 Bluff","20 Lee's Ferry") 
#even though we are comparing outflow to Navajo.In and FG.In we need node name to be Arch and Greendale to match attributes

allWU <- allWU %>%
  dplyr::filter(Node %in% nodes) #filter out LB nodes 

# sectors <- unique(allWU$Sector)
onlythesesectors <- c("Agriculture","Evaporation","Energy","Exports","M & I","Minerals")        #"Minerals", "Environmental","Lease" ,"Fish & Wildlife"

################################################################################
#### #### C. Read CU&L data  ####  ####
################################################################################
allCUL <- readxl::read_excel(path = "C:/Users/cfelletter/Documents/natflowsaltmodel/results/Check/HistoricCUL_MasterCheck.xlsx",sheet = 'CULBySector' )

allCUL <- pivot_longer(allCUL, cols = names(allCUL)[3:length(names(allCUL))],names_to = 'Date',values_to = "Value")

#format date and make year month colums 
allCUL$Date = as.Date(as.numeric(allCUL$Date), origin = "1899-12-30")
allCUL$Year = as.numeric(format.Date(allCUL$Date, format = "%Y"))

#filter out any extra years
allCUL <- allCUL %>% filter(Year %in% years)

#get a numeric month number - NOT SURE I NEED THIS 
allCUL$MonthNum = as.numeric(format.Date(allCUL$Date, format = "%m"))
allCUL$Slot = rep("CUL",times=length(allCUL$Date))

allCUL <- allCUL %>%
  dplyr::filter(Sector != "ResReg") #remove from total demands  

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

i=j=1 #debug
if(!(length(outflows) == length(gages)))
  stop('Please ensure Nodes, Gages, Outflows are set correctly.')

# pdf(file.path(ofigs,"WUandCULbyCPbySector.pdf"), width=9, height=6) #disable below pdf if enabled

for (i in 1:length(nodes)) {
  # for (i in 5:length(nodes)) {
  # for (i in 1) {
  # for (i in c(1:4)) {
  
  
  #create a figure folder
  if(printfigs==T){
    if (!file.exists(file.path(ofigs,nodes[i]))) {
      message(paste('Creating folder:',nodes[i]))
      dir.create(file.path(ofigs,nodes[i]))
    }
  }
  
  #### DON"T Need to repeat these steps in later code ### 
  
  # #plot total demand and total CUL 
  # zz <- rbind(allWU[,c("Date","Value","Year","Slot")],
  #             allCUL[,c("Date","Value","Year","Slot")])
  
  # #create a depleted slot   
  # requested <- zz %>%
  #   dplyr::filter(Slot == "Depletion Requested")
  # depleted <- zz %>%
  #   dplyr::filter(Slot == "Depletion Shortage")
  # depleted$Value = requested$Value - depleted$Value   
  # depleted$Slot = rep("Depletion",times = length(depleted$Slot))
  # xx <- rbind.data.frame(zz,depleted)
  # #
  # p <- xx %>% 
  #   dplyr::filter(Slot %in% c("Depletion Requested","Depletion","CUL"))  %>% 
  #   group_by(Slot,Year) %>%
  #   summarise(Value = sum(Value))  %>%
  #   ggplot(aes(x = Year, y = Value, color = Slot)) + theme_light() + 
  #   geom_line() +
  #   scale_color_manual(values = c(mycolors[4],mycolors[3],mycolors[3])) +
  #   scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  #   labs(title = paste("UB Total Annual Demand"), y = "Depletions (AF/yr)")
  # print(p)
  # if(printfigs==T){ ggsave(filename = file.path(ofigs,paste0("UB Ann Total Demand",scens,".png")), width = gage_widths[5],height = gage_heights[5])}
  
  print(paste("Node",nodes[i]))
  
  pdf(file = file.path(ofigs,paste(nodes[i],scens,"Use.pdf")), width=9, height=6) ### disable if running one big plot including earlier dev.off
  
  ##############################################################################
  #### #### A. Gage vs Model Outflow  ####  ####
  ##############################################################################
  
  # annual plot
  df <-  df_annual %>%
    dplyr::filter(Variable == gages[i] | Variable == outflows[i]) %>%
    dplyr::group_by(Year, Variable) 
  
  
  #custom plot parameters for those areas that are compared to Reservoir Inflow rather than gage 
  if (i %in% c(3,7,9,16,18)){ #"4 Blue Mesa"  "9 Fontenelle" "11 Greendale" "18 Archuleta"
    legend_labs <- c("Res Inflow", "Model")
    node_title <- paste("Above",nodes[i])
  } else {
    legend_labs <- c("Gage", "Model")
    node_title <- nodes[i]
  }
  
  p <- df %>%
    ggplot(aes(x = Year, y = Value, color = Variable)) +
    geom_line() +
    theme_light() + 
    scale_colour_discrete(#name  ="Legend",  #change legend 
      type= c(mycolors[4],mycolors[3]), # customcolors 
      breaks=c(gages[i], outflows[i]),
      labels=legend_labs)  + 
    scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
    labs(title = paste(node_title,"Annual Flow"), y = y_lab_yr)
  print(p)
  if(printfigs==T){ ggsave(filename = file.path(ofigs,nodes[i],paste0(nodes[i]," Ann Gage",scens,".png")), width = gage_widths[1],height = gage_heights[1])}
  
  #calculate residual
  gage <- df_annual %>%
    dplyr::filter(Variable == gages[i])
  
  simulated <- df_annual %>%
    dplyr::filter(Variable == outflows[i])
  
  diff <- gage
  diff$Value = simulated$Value - gage$Value
  diff$Variable = rep("Residual",times = length(diff$Variable))
  
  cumsum_err <- cumsum(diff$Value) #get cumulative error
  
  df_csv <- cbind.data.frame(pivot_wider(rbind.data.frame(df,diff),
              names_from = Variable,values_from = Value),
              cumsum_err)
  
  #dump out the data for Jim 
  if(printfigs==T){ write.csv(x = df_csv,
                             file = file.path(ofigs,nodes[i],paste0(nodes[i]," Ann Gage",scens,".csv")))}
  
  #annual residual
  diff <- diff %>%
    dplyr::group_by(Year, Variable) 
  p <- diff %>%
    ggplot(aes(x = Year, y = Value, color = Variable)) +
    geom_line() +
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste(node_title,"Annual Residual"), y = y_lab_yr)
  print(p)
  if(printfigs==T){ ggsave(filename = file.path(ofigs,nodes[i],paste0(nodes[i]," Ann Resid",scens,".png")), width = gage_widths[2],height = gage_heights[2])}
  
  #annual cumsum residual
  p <- df_csv %>%
    ggplot(aes(x = Year, y = cumsum_err)) +
    geom_line() +
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste(node_title,"Annual Cummulative Residual"), y = y_lab_yr)
  print(p)
  if(printfigs==T){ ggsave(filename = file.path(ofigs,nodes[i],paste0(nodes[i]," Ann Cumsum Resid",scens,".png")), width = gage_widths[2],height = gage_heights[2])}
  
  #annual metrics 
  mae <- round(sum(abs(diff$Value))/length(diff$Value))
  bias <- round(sum(diff$Value)/length(diff$Value))
  error_perc <- round(mae/mean(gage$Value)*100)
  ann_avgflow <- round(mean(gage$Value))
  # print(paste(title,"mae",mae,"bias",bias,"error % of gage",error_perc*100))
  
  #create a sperate matrix of annual stats to store the % of gage erorr 
  if(!exists("annstats") | i==1){
    annstats <- array(c(outflows[i],mae,bias,error_perc,ann_avgflow)) #c(outflows[i],mae,bias,error_perc*100)
  } else {
    annstats <- rbind(annstats,c(outflows[i],mae,bias,error_perc,ann_avgflow))
  }
  
  #### monthly #####  
  
  #monthly flows
  df <- df_monthly %>%
    dplyr::filter(Variable == gages[i] | Variable == outflows[i]) 
  
  p <- df %>%
    dplyr::group_by(Date, Variable) %>%
    ggplot(aes(x = Date, y = Value, color = Variable)) +
    geom_line() +
    theme_light() +
    scale_colour_discrete(#name  ="Legend",  #change legend
      type= c(mycolors[4],mycolors[3]), # customcolors
      breaks=c(gages[i], outflows[i]),
      labels=legend_labs)  +
    scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
    labs(title = paste(node_title,"Monthly Flow"), y = y_lab_mon)
  print(p)
  if(printfigs==T){ ggsave(filename = file.path(ofigs,nodes[i],paste0(nodes[i]," Mon Gage",scens,".png")), width = gage_widths[3],height = gage_heights[3])}
  
  #calculate residual
  gage <- df_monthly %>%
    dplyr::filter(Variable == gages[i])
  simulated <- df_monthly %>%
    dplyr::filter(Variable == outflows[i])
  
  diff <- gage
  diff$Value = simulated$Value - gage$Value
  diff$Variable = rep("Residual",times = length(diff$Variable))
  
  cumsum_err <- cumsum(diff$Value) #get cumulative error
  
  df_csv <- cbind.data.frame(pivot_wider(rbind.data.frame(df[,names(diff)],diff),
                                         names_from = Variable,values_from = Value),
                             cumsum_err)
  
  #dump out the data for Jim 
  if(printfigs==T){ write.csv(x = df_csv,
                              file = file.path(ofigs,nodes[i],paste0(nodes[i]," Mon Gage",scens,".csv")))}
  
  #monthly residual
  p <- diff %>%
    dplyr::group_by(Date, Variable) %>%
    ggplot(aes(x = Date, y = Value, color = Variable)) +
    geom_line() +
    theme_light() +
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste(node_title,"Monthly Residual"), y = y_lab_mon)
  print(p)
  if(printfigs==T){ ggsave(filename = file.path(ofigs,nodes[i],paste0(nodes[i]," Mon Resid",scens,".png")), width = gage_widths[4],height = gage_heights[4])}
  
  #monthly cumsum residual
  p <- df_csv %>%
    ggplot(aes(x = Date, y = cumsum_err)) +
    geom_line() +
    theme_light() +
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste(node_title,"Monthly Cummulative Residual"), y = y_lab_mon)
  print(p)
  if(printfigs==T){ ggsave(filename = file.path(ofigs,nodes[i],paste0(nodes[i]," Mon CumSum Resid",scens,".png")), width = gage_widths[4],height = gage_heights[4])}
  
  
  metrics <- diff %>%
    dplyr::group_by(MonthNum) %>%
    summarise('MAE' = round(mean(abs(Value))),'Bias' = round(mean(Value))) #%>%
  # metrics
  
  ann_metrics <- metrics[1,]
  ann_metrics[1,] = as.list(c(0,mae,bias))
  # ann_metrics
  
  metrics = rbind.data.frame(metrics,ann_metrics,c(NA,error_perc,NA))
  # write.csv(metrics,file = file.path(results_dir,paste0(gages[i],".csv")))
  
  ##############################################################################
  #### #### B. Filter Sector ####  ####
  ##############################################################################
  
  #filter out all but one node
  WU <- allWU %>%
    dplyr::filter(Node == nodes[i]) ###THIS WORKS must be something wrong with my dataframe
  CUL <- allCUL %>%
    dplyr::filter(Node == nodes[i]) %>%
    dplyr::filter(Sector != "ResReg") #remove from total demands
  
  #create a depleted slot, there is not one on the water user in RW
  requested <- WU %>%
    dplyr::filter(Slot == "Depletion Requested") # zz %>%
  depleted <- WU %>% #this slot will be later overwritten with actual depletion values 
    dplyr::filter(Slot == "Depletion Shortage") # Schedule - Depletion = all shortages  
  schedule <- WU %>%
    dplyr::filter(Slot == "Depletion Schedule")
  
  depleted$Value = requested$Value - depleted$Value
  depleted$Slot = rep("Depletion",times = length(depleted$Slot))
  
  WU <- rbind(schedule,requested,depleted)
  zz <- rbind(WU[,c("Date","Value","Year","Slot")],
              CUL[,c("Date","Value","Year","Slot")])
  
  # # Adding factors so ggplot does not alphabetize legend
  zz$Slot = factor(zz$Slot, levels=c("Depletion Schedule","Depletion Requested","Depletion","CUL"))
  
  #plot total demand and total CUL
  p <- zz %>%
    dplyr::filter(Slot %in% c("Depletion Schedule","Depletion Requested","Depletion","CUL")) %>%
    group_by(Slot,Year) %>%
    summarise(Value = sum(Value))  %>%
    ggplot(aes(x = Year, y = Value, color = Slot)) + theme_light() +
    geom_line(aes(linetype=Slot)) +
    scale_linetype_manual(values = mylinetypes) +
    scale_color_manual(values = mycolors) +
    scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
    labs(title = paste(node_title,"Total Annual Demand"), y = "Depletions (AF/yr)")
  print(p)
  if(printfigs==T){ ggsave(filename = file.path(ofigs,nodes[i],paste0(nodes[i]," Ann Total Demand",scens,".png")), width = gage_widths[5],height = gage_heights[5])}
  
  # #plot depletion requested with flow - I was trying to look at flow-shortage relationship
  # xxx <- df_monthly %>%
  #   dplyr::filter(Variable == gages[i] | Variable == outflows[i]) %>%
  #   mutate(Slot = Variable)
  # flowuse <- rbind.data.frame(zz,xxx[,c("Date","Value","Year","Slot" )])
  # 
  # p <- flowuse %>%
  #   dplyr::filter(Slot %in% c("Depletion Requested",outflows[i],"Depletion Shortage","CUL")) %>%
  #   # dplyr::filter(Slot %in% c("Depletion Requested","CUL",gages[i],outflows[i],"Depletion Shortage")) %>%
  #   group_by(Slot,Year) %>%
  #   summarise(Value = sum(Value))  %>%
  #   ggplot(aes(x = Year, y = Value, color = Slot)) + theme_light() +
  #   geom_line() +
  #   scale_color_manual(values = c(mycolors[4],"#eb34de",mycolors[3],mycolors[3])) +
  #   # scale_color_manual(values = c(mycolors[4],mycolors[3],mycolors[3])) + #if don't have CUL
  #   scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  #   labs(title = paste(node_title,"Annual Flow-Shortage Relationship"), y = "(AF/yr)")
  # # print(p)
  
  # # what to do about sectors unique to CRSS? "Environmental" and "Lease". Fish & Wildlife only in LB
  # add CUL stockpond, livestock and minearls into sectors evap, ag, energy
  CUL[which(CUL$Sector == "Evaporation"),]$Value = CUL[which(CUL$Sector == "Evaporation"),]$Value + CUL[which(CUL$Sector == "Stockpond"),]$Value
  CUL[which(CUL$Sector == "Agriculture"),]$Value = CUL[which(CUL$Sector == "Agriculture"),]$Value + CUL[which(CUL$Sector == "Livestock"),]$Value
  
  # sort(unique(allWU[which(allWU$Sector == "Minerals"),]$Node)) #some sectors have mineraals and some don't, they don't match up
  if (sum(WU[which(WU$Sector == "Minerals"),]$Value) == 0){
    CUL[which(CUL$Sector == "Energy"),]$Value = CUL[which(CUL$Sector == "Energy"),]$Value + CUL[which(CUL$Sector == "Minerals"),]$Value
    print("No Minerals in CRSS, combining CUL Mineral with CUL Energy")
  } #some sectors have mineraals and some don't, they don't match up
  
  #limit which sectors are plotted
  sectors <- unique(WU$Sector) #only plot sectors that exisit in CP
  sectors = sectors[which(sectors %in% onlythesesectors)] #minearls is now included but only if exisits in WU
  sectors = sectors[which(sectors %in% unique(CUL$Sector))]
  # sectors
  
  #filter out all unused sectors 
  WU <- WU %>%
    dplyr::filter(Sector %in% sectors)
  CUL <- CUL %>%
    dplyr::filter(Sector %in% sectors)
  
  ##############################################################################
  #### #### C. Sector Plots   ####  ####
  ##############################################################################
  
  if(length(sectors) > 0){ #have some sector of interest
    for (j in 1:length(sectors)) {
      
      print(paste("Sector",sectors[j]))
      
      x <- WU %>%
        dplyr::filter(Sector == sectors[j]) %>%
        group_by(Sector,Slot,Year,MonthNum,Date) %>%
        summarise(Value = sum(Value))# %>% # summarize multiple users into one
      uses <- CUL %>%
        dplyr::filter(Sector == sectors[j])
      uses <- uses[,names(x)]  #same column layout
      
      #previously created a depleted slot, so no need to redo, just filter x
      depleted <- x %>% #this slot will be later overwritten with actual depletion values 
        dplyr::filter(Slot == "Depletion") # Schedule - Depletion = all shortages  
      schedule <- x %>%
        dplyr::filter(Slot == "Depletion Schedule")
      
      #for stats later
      precntshorted <- sum(depleted$Value)/sum(schedule$Value)*100 #base on schedule now
      annrequest <- sum(schedule$Value)/nyears
      
      xx <- rbind.data.frame(x,uses)
      
      # # Adding factors so ggplot does not alphabetize legend
      xx$Slot = factor(xx$Slot, levels=c("Depletion Schedule","Depletion Requested","Depletion","CUL"))
      
      #annual
      p <- xx %>%
        group_by(Slot,Year) %>%
        summarise(Value = sum(Value)) %>%
        ggplot(aes(x = Year, y = Value, color = Slot)) + theme_light() +
        geom_line(aes(linetype=Slot)) +
        scale_linetype_manual(values = mylinetypes) +
        scale_color_manual(values = mycolors) +
        scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
        labs(title = paste(node_title,sectors[j],"Annual"), y = "Depletions (AF/yr)")
      print(p)
      
      #print small annual plot
      if(printfigs==T){
        px <- xx %>%
          group_by(Slot,Year) %>%
          summarise(Value = sum(Value)) %>%
          ggplot(aes(x = Year, y = Value, color = Slot)) + theme_light() +
          geom_line(aes(linetype=Slot)) +
          scale_linetype_manual(values = mylinetypes) +
          scale_color_manual(values = mycolors) +
          scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
          labs(title = sectors[j]) +
          theme(legend.position = "none", axis.title.x = element_blank(),axis.title.y= element_blank())
        # print(px)
        # ggsave(plot = px, filename = file.path(ofigs,nodes[i],paste0(nodes[i],sectors[j]," Ann",scens,".png")), width = w_ansector,height = h_ansector)
        ggsave(plot = px, filename = file.path(ofigs,nodes[i],paste(j,sectors[j],"Ann",scens,".png")), width = sect_widths[1],height = sect_heights[1])
      }
      
      #calculate residual
      diff <- xx[which(xx$Slot == "Depletion"),]
      diff$Value = diff$Value - xx[which(xx$Slot == "CUL"),]$Value 
      length(diff$Value)/12
      length(xx[which(xx$Slot == "CUL"),]$Value)/12
      #Error: Assigned data `value` must be compatible with existing data
      diff$Slot = rep("Residual",times = length(diff$Value))
      
      cumsum_err <- cumsum(diff$Value) #get cumulative error
      
      #annual residual
      p <- diff %>%
        group_by(Slot,Year) %>%
        summarise(Value = sum(Value)) %>%
        ggplot(aes(x = Year, y = Value, color = Slot)) +  theme_light() +
        geom_line() +
        scale_y_continuous(labels = scales::comma) +
        labs(title = paste(node_title,sectors[j],"Annual Residual"), y = "Depletions (AF/yr)")
      print(p)
      
      #monthly
      p <- xx %>%
        group_by(Slot,Date) %>%
        ggplot(aes(x = Date, y = Value, color = Slot)) +
        theme_light() +
        geom_line(aes(linetype=Slot)) +
        scale_linetype_manual(values = mylinetypes) +
        scale_color_manual(values = mycolors) +
        labs(title = paste(node_title,sectors[j],"Monthly"), y = "Depletions (AF/mo)")
      if (min(xx$Value) < 0) {
        #don't limit y to 0
        p <- p +
          scale_y_continuous(labels = scales::comma)
      } else {
        p <- p +
          scale_y_continuous(limits = c(0,NA), labels = scales::comma)
      }
      print(p)
      
      if(printfigs==T){ #print figure of monthly agg
        px <- xx %>%
          group_by(Slot,Date) %>%
          ggplot(aes(x = Date, y = Value, color = Slot)) +
          theme_light() +
          geom_line(aes(linetype=Slot)) +
          scale_linetype_manual(values = mylinetypes) +
          scale_color_manual(values = mycolors) +
          scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
          labs(y = "Depletions (AF/mo)") +
          theme(legend.position = "none", axis.title.x = element_blank())#,axis.title.y= element_blank())
        # print(px)
        ggsave(plot = px, filename = file.path(ofigs,nodes[i],paste0(j,sectors[j]," Mon",scens,".png")), width = sect_widths[2],height = sect_heights[2])
      }
      
      #monthly residual
      p <- diff %>%
        dplyr::group_by(Date, Slot) %>%
        ggplot(aes(x = Date, y = Value, color = Slot)) +
        geom_line() +
        theme_light() +
        scale_y_continuous(labels = scales::comma) +
        labs(title = paste(node_title,sectors[j],"Monthly Residual"), y = "Depletions (AF/mon)")
      print(p)
      
      df_sector <- cbind.data.frame(pivot_wider(rbind.data.frame(xx[,names(diff)],diff),
                                               names_from = Slot,values_from = Value),
                                   cumsum_err)
      
      #monthly cumsum residual
      p <- df_sector %>%
        ggplot(aes(x = Date, y = cumsum_err)) +
        geom_line() +
        theme_light() +
        scale_y_continuous(labels = scales::comma) +
        labs(title = paste(node_title,sectors[j],"Cummulative Monthly Residual"), y = "Depletions (AF/mon)")
      print(p)
      
      #dump out the data for Jim 
      if(printfigs==T){ 
        if  (j == 1) {
          df_all_sector <- df_sector #build df on first sector
          
        } else if (j == length(sectors)) {
          
          df_all_sector = rbind.data.frame(df_all_sector,df_sector)
          
          #print data on last sector 
          write.csv(x = df_all_sector,
                    file = file.path(ofigs,nodes[i],paste0(nodes[i],"_Sector_Mon",scens,".csv")))}
  
        } else {
          
          df_all_sector = rbind.data.frame(df_all_sector,df_sector)
          
        }
      
      # % monthly / annual distribution plot
      xxx <- xx %>%
        filter(Slot == "Depletion Schedule" | Slot == "CUL") %>%
        group_by(Slot,Year) %>%
        mutate(Distirubtion = Value/sum(Value)) %>%
        group_by(Slot,MonthNum) %>%
        summarise(Distirubtion = mean(Distirubtion))
      
      #save the CUL distribution to apply as new distribution factor
      sctrdist<-xxx[which(xxx$Slot == "CUL"),]
      sctrdist <- cbind(sctrdist,xxx[which(xxx$Slot == "Depletion Schedule"),]$Distirubtion)[,3:4]
      colnames(sctrdist) = c(paste("CUL",sectors[j]),paste("CRSS",sectors[j]))
      # sctrdist
      
      #finish distribution plot
      p <- xxx %>%
        ggplot(aes(x = MonthNum, y = Distirubtion, color = Slot)) +
        theme_light() +
        scale_y_continuous(labels = scales::percent) +
        scale_x_continuous(breaks = 1:12,labels = month.abb) +
        scale_color_manual(values = c(mycolors[3],mycolors[4])) +
        geom_line() +
        labs(title = paste(node_title,sectors[j],"Distribution"), y = "Monthly Distribution",x="Month")
      print(p)
      
      if(printfigs==T){ #print figure of monthly agg
        px <- xxx %>%
          ggplot(aes(x = MonthNum, y = Distirubtion, color = Slot)) +
          theme_light() +
          scale_y_continuous(labels = scales::percent) +
          scale_x_continuous(breaks = 1:12,labels = month.abb) +
          scale_color_manual(values = c(mycolors[3],mycolors[4])) +
          geom_line() +
          theme(legend.position = "none", axis.title.x = element_blank(),axis.title.y= element_blank())
        # print(px)
        ggsave(plot = px, filename = file.path(ofigs,nodes[i],paste(j,sectors[j],"Mon Dist.png")), width = sect_widths[3],height = sect_heights[3])
      }
      
      #enable the below line if want to calculate MAE and BIAS as Depletion Requested - CUL (credit for shortage)
      # depleted <- requested # inccorrect method how I did other sectors beyond ag in my intial wbs
      
      #monthly MAE and bias
      depleted$CUL <- uses$Value
      
      zz <- depleted %>%
        mutate(Diff = Value - CUL)  %>%
        mutate(absDiff = abs(Value - CUL))
      
      monbias <- zz %>%
        group_by(MonthNum) %>%
        summarise(Bias = mean(Diff)) %>%
        round()
      
      monMAE <- zz %>%
        group_by(MonthNum) %>%
        summarise(MAE = mean(absDiff)) %>%
        round()
      
      #sum to annual
      anndepleted <- depleted %>%
        group_by(Year) %>%
        summarise(Value = sum(Value)) #%>%
      annuses <- uses %>%
        group_by(Year) %>%
        summarise(Value = sum(Value)) #%>%
      
      anndepleted$CUL <- annuses$Value
      
      #compute
      anndepleted <- anndepleted %>%
        mutate(Diff = Value - CUL)  %>%
        mutate(absDiff = abs(Value - CUL))
      
      #mean
      annbias <- anndepleted %>%
        summarise(Bias = mean(Diff)) %>%
        round()
      annMAE <- anndepleted %>%
        summarise(MAE = mean(absDiff)) %>%
        round()
      
      error_prec_annual <- round(annMAE/annrequest*100)
      
      mystats <- rbind(cbind(monMAE[,2],monbias[,2]),
                       c(annMAE,annbias))
      
      #add off stats
      mystats <- rbind(mystats,mystats[1:2,])
      mystats[14,1] = error_prec_annual
      mystats[14,2] = NA
      mystats[15,1] = round(precntshorted)
      mystats[15,2] = NA
      
      row.names(mystats) = c(month.abb,"Annual","% AnnMAE/AnnRequest","Total % Shorted")
      # mystats
      
      colnames(mystats) = paste(sectors[j],colnames(mystats))
      
      if (j == 1){
        allstats <- mystats
        allsctrdist <- sctrdist
      } else {
        allstats <- cbind(allstats,mystats )
        allsctrdist <- cbind(allsctrdist,sctrdist )
        
      }
      
      #combine metrics from out-gage with allstats from demands analysis
      write.csv(cbind(rbind(metrics[,2:3],NA),allstats),
                file = file.path(ofigs,nodes[i],paste(nodes[i],scens,"Stats.csv")))
      
      #write sector monthly distributions
      write.csv(allsctrdist,
                file = file.path(ofigs,nodes[i],paste(nodes[i],scens,"SectorMonthlyDistribution.csv")))
      
      # if (i==1) {
      #   
      # }
      # 
      # writexl::write_xlsx(allsctrdist,
      #           file = file.path(ofigs,paste(nodes[i],scens,"SectorMonthlyDistribution.csv")))
      # xlsx::write
      
    } #end for sectors loop
    
  } #end if sectors > 0  
  
  dev.off()
  
  
  #make row names then don't print reach 
  if (dim(annstats)[1]==length(nodes)) { #this should mean you ran all of the traces
    colnames(annstats) <- c("Reach","MAE","Bias","Error % of gage","AA Gage Flow")
    annstats <- annstats[,c("Reach","MAE","Bias","AA Gage Flow","Error % of gage")] 
    rownames(annstats) <- nodes
    # rownames(annstats)[18] <- "Lees Ferry"
    write.csv(annstats[,2:5],file = file.path(ofigs,paste0("AnnualVerificationStats",scens,".csv")))
  } else {
    write.csv(annstats,file = file.path(ofigs,paste0("Partial_AnnualVerificationStats",scens,".csv")))
  }
  
} #end node loop

dev.off() #mega plot 


