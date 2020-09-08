#### Combined Gage and Use Verificaiton Script ########
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

### Fix this in Verification Demand code ########
scen_dir <- file.path(CRSSDIR,"Scenario")
# #containing the sub folders for each ensemble
scens <- "DemandVerification" # file name for where results are stored and for folder in /results/
file_dir <- file.path(scen_dir, scens[1])

# check folders
if(!file.exists(file_dir))
  stop('Scenarios folder(s) do not exist or scen_dir is set up incorrectly.
       Please ensure Scenarios is set correctly.')

ofigs <- file.path(results_dir,scens[1])
if (!file.exists(ofigs)) {
  message(paste('Creating folder:', ofigs))
  dir.create(ofigs)
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#### #### A. Read flow data  ####  ####

#agg file specifying which slots
rw_agg_file <- "VerificationRun_rwagg.csv"
#read agg file specifying which slots
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) #ubres.rdf res.rdf

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
df_monthly <- rdf_aggregate(rwa1, rdf_dir = results_dir) 

#get everything on a date 
df_monthly$Date = as.Date(paste0(df_monthly$Year,df_monthly$Month,"01"), format = "%Y%B%d")
#get a numeric month number
df_monthly$MonthNum = as.numeric(format.Date(df_monthly$Date, format = "%m"))

rw_agg_file <- "VerificationRun_rwagg_annual.csv"
#read agg file specifying which slots
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) #ubres.rdf res.rdf

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
df_annual <- rdf_aggregate(rwa1, rdf_dir = results_dir) 


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


#### #### B. Read water use data  ####  ####

#### read attributes data to make key ####
Attributes <- read_xml(x="C:/Users/cfelletter/Documents/CRSS working/DemandUseVerificationRun/Attributes.xml")
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

# sectors <- unique(allWU$Sector)
onlythesesectors <- c("Agriculture","Evaporation","Energy","Exports","M & I")        #"Minerals", "Environmental","Lease" ,"Fish & Wildlife"
    

#### #### C. Read CU&L data  ####  ####
allCUL <- readxl::read_excel(path = "C:/Users/cfelletter/Documents/natflowsaltmodel/results/Check/HistoricCUL_MasterCheck.xlsx",sheet = 'CULBySector' )

allCUL <- pivot_longer(allCUL, cols = names(allCUL)[3:length(names(allCUL))],names_to = 'Date',values_to = "Value")

#format date and make year month colums 
allCUL$Date = as.Date(as.numeric(allCUL$Date), origin = "1899-12-30")
allCUL$Year = as.numeric(format.Date(allCUL$Date, format = "%Y"))
#get a numeric month number - NOT SURE I NEED THIS 
allCUL$MonthNum = as.numeric(format.Date(allCUL$Date, format = "%m"))
allCUL$Slot = rep("CUL",times=length(allCUL$Date))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

y_lab_yr = "Flow (ac-ft/yr)"
y_lab_mon = "Flow (ac-ft/mo)"

i=1
j=1

length(nodes) == length(outflows) == length(gages)
length(outflows) == length(gages)

if(!(length(outflows) == length(gages)))
  stop('Please ensure Nodes, Gages, Outflows are set correctly.')


# pdf(file.path(ofigs,"WUandCULbyCPbySector.pdf"), width=9, height=6) #disable below if enable

for (i in 1:length(nodes)) {
# for (i in 7:8) {
  
  print(paste("Node",nodes[i]))
  
  pdf(file = file.path(ofigs,paste0(nodes[i]," Use.pdf")), width=9, height=6)
  
  #### #### A. Gage vs Model Outflow  ####  ####
  
  #annual plot
  p <- df_annual %>%
    dplyr::filter(Variable == gages[i] | Variable == outflows[i]) %>%
    dplyr::group_by(Year, Variable) %>%
    ggplot(aes(x = Year, y = Value, color = Variable)) +
    geom_line() +
    theme_light() + 
    scale_colour_discrete(#name  ="Legend",  #change legend 
                            # type= c("#999999", "#E69F00","#E69F00","#E69F00"), # customcolors 
                            breaks=c(gages[i], outflows[i]),
                            labels=c("Gage", "Model"))  + 
    scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
    labs(title = paste(nodes[i],"Annual Flow"), y = y_lab_yr)
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
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste(nodes[i],"Annual Residual"), y = y_lab_yr)
  print(p)
  
  #annual metrics 
  mae <- round(sum(abs(diff$Value))/length(diff$Value))
  bias <- round(sum(diff$Value)/length(diff$Value))
  error_perc <- round(mae/mean(gage$Value)*100)

  # print(paste(title,"mae",mae,"bais",bias,"error % of gage",error_perc*100))
  
  #create a sperate matrix of annual stats to store the % of gage erorr 
  if(i==1){
    annstats <- array(c(outflows[i],mae,bias,error_perc)) #c(outflows[i],mae,bias,error_perc*100)
  } else {
    annstats <- rbind(annstats,c(outflows[i],mae,bias,error_perc))
  }
  
  #### monthly #####  
  
  #monthly flows
  p <- df_monthly %>%
    dplyr::filter(Variable == gages[i] | Variable == outflows[i]) %>%
    dplyr::group_by(Date, Variable) %>%
    ggplot(aes(x = Date, y = Value, color = Variable)) +
    geom_line() +
    theme_light() + 
    scale_colour_discrete(#name  ="Legend",  #change legend 
      # type= c("#999999", "#E69F00","#E69F00","#E69F00"), # customcolors 
      breaks=c(gages[i], outflows[i]),
      labels=c("Gage", "Model"))  + 
    scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
    labs(title = paste(nodes[i],"Monthly Flow"), y = y_lab_mon)
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
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste(nodes[i],"Monthly Residual"), y = y_lab_mon)
  print(p)
  
  metrics <- diff %>%
    dplyr::group_by(MonthNum) %>%
    summarise('MAE' = round(mean(abs(Value))),'Bias' = round(mean(Value))) #%>%
  # metrics  
  
  ann_metrics <- metrics[1,] 
  ann_metrics[1,] = as.list(c(0,mae,bias))
  # ann_metrics
  
  metrics = rbind.data.frame(metrics,ann_metrics,c(NA,error_perc,NA))
  
  # write.csv(metrics,file = file.path(results_dir,paste0(gages[i],".csv")))
  
  #### #### B. Sector Plots   ####  ####
  
  #filter out all but one node
  WU <- allWU %>%
    dplyr::filter(Node == nodes[i])
  CUL <- allCUL %>%
    dplyr::filter(Node == nodes[i])
  
  #limit which sectors are plotted 
  sectors <- unique(WU$Sector) #only plot sectors that exisit in CP
  sectors = sectors[which(sectors %in% onlythesesectors)]
  sectors = sectors[which(sectors %in% unique(CUL$Sector))]
  # sectors
  
  #filter out all unused sectors and slots 
  WU <- WU %>%
    dplyr::filter(Sector %in% sectors) %>%
    dplyr::filter(Slot %in% c("Depletion Requested","Depletion Shortage")) 
  CUL <- CUL %>%
    dplyr::filter(Sector %in% sectors) 

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
  
    #create a depleted slot   
    requested <- x %>%
        dplyr::filter(Slot == "Depletion Requested")
      depleted <- x %>%
        dplyr::filter(Slot == "Depletion Shortage")
    
    #for stats later  
    precntshorted <- sum(depleted$Value)/sum(requested$Value)*100  
    annrequest <- sum(requested$Value)/19 
        
    depleted$Value = requested$Value - depleted$Value   
    depleted$Slot = rep("Depletion",times = length(depleted$Slot))
    
    xx <- rbind.data.frame(requested,depleted,uses)
    
    # # Adding factors so ggplot does not alphebetize legend
    xx$Slot = factor(xx$Slot, levels=c("Depletion Requested","Depletion","CUL"))
    
    #annual  
    p <- xx %>% 
      group_by(Slot,Year) %>%
      summarise(Value = sum(Value)) %>%
      # group_by(ObjectSlot)  %>%
      ggplot(aes(x = Year, y = Value, color = Slot)) +  theme_light() + 
      geom_line() +
      scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
      labs(title = paste(nodes[i],sectors[j],"Annual"), y = "Depletions (AF/yr)")
    print(p)
    
    #monthly 
    p <- xx %>%
      group_by(Slot,Date) %>%
      ggplot(aes(x = Date, y = Value, color = Slot)) +
      theme_light() + 
      scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
      geom_line() +
      labs(title = paste(nodes[i],sectors[j],"Monthly"), y = "Depletions (AF/mo)")
    print(p)
    
    
    # % monthly / annual distribution plot 
    p <- xx %>% 
      group_by(Slot,Year) %>%
      mutate(Distirubtion = Value/sum(Value)) %>% 
      group_by(Slot,MonthNum) %>%
      summarise(Distirubtion = mean(Distirubtion)) %>% 
      ggplot(aes(x = MonthNum, y = Distirubtion, color = Slot)) +
      theme_light() + 
      scale_y_continuous(labels = scales::percent) + 
      scale_x_continuous(breaks = 1:12,labels = month.abb) + 
      geom_line() +
      labs(title = paste(nodes[i],sectors[j],"Distribution"), y = "Monthly Distribution",x="Month")
    print(p)
    
    
      
    
    #overwrite this line if want to calculate MAE and BIAS as Depletion - CUL (credit for shortage)
    # depleted <- requested     
    #in my workbooks I calculted the MAE and BIAS as 
    #All other sectors were Requested - CUL for 

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
    } else {
      allstats <- cbind(allstats,mystats )
      
    } 
    
    
  } #end for sectors loop
    
    
    # write.csv(allstats,file = file.path(ofigs,paste0(nodes[i]," Stats.csv")))
    # cbind(rbind(metrics[,2:3],NA),allstats)
    write.csv(cbind(rbind(metrics[,2:3],NA),allstats),
              file = file.path(ofigs,paste0(nodes[i]," Stats.csv")))

  } #end if sectors > 0  
  
  dev.off()


} #end node loop

# dev.off() #mega plot 

colnames(annstats) <- c("Reach","MAE","Bais","Error % of gage")
rownames(annstats) <- nodes
annstats
write.csv(annstats[,2:4],file = file.path(ofigs,paste0("AnnualVerificationStats.csv")))