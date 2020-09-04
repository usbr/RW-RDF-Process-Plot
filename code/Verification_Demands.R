#### how to get depletions information ########
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
tmp2 <- str_split(allWU$ObjectSlot, ".D", n = 2, simplify = TRUE)
#### this might be messing up some objects
# unique(tmp2[,2])
# head(tmp2)

allWU['object'] = tmp2[,1]

allWU['Slot'] = paste0(rep("D",length(tmp2[,2])),tmp2[,2]) #fix the d 

rm(tmp,tmp2)

# test <- allWU[1:50,]
# test %>% 
#   full_join(lookup_wu, by="object")

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
    

#### read demands #####
# library(openxlsx)
# allCUL <- read.xlsx(xlsxFile = "C:/Users/cfelletter/Documents/natflowsaltmodel/results/Check/CULbySectorbyCP.xlsx",sheet = 'CULBySector')
# allCUL <- readxl::read_excel(path = "C:/Users/cfelletter/Documents/natflowsaltmodel/results/Check/CULbySectorbyCP.xlsx",sheet = 'CULBySector' ) 
#update sheet in Master instead so it doesn't break for new years 
allCUL <- readxl::read_excel(path = "C:/Users/cfelletter/Documents/natflowsaltmodel/results/Check/HistoricCUL_MasterCheck.xlsx",sheet = 'CULBySector' )



allCUL <- pivot_longer(allCUL, cols = names(allCUL)[3:length(names(allCUL))],names_to = 'Date',values_to = "Value")

#format date and make year month colums 
allCUL$Date = as.Date(as.numeric(allCUL$Date), origin = "1899-12-30")
allCUL$Year = as.numeric(format.Date(allCUL$Date, format = "%Y"))
#get a numeric month number - NOT SURE I NEED THIS 
allCUL$MonthNum = as.numeric(format.Date(allCUL$Date, format = "%m"))
allCUL$Slot = rep("CUL",times=length(allCUL$Date))
head(allCUL)

# # white space check 
# xnodes <- unique(allCUL$Node)
# for (i in 1:length(nodes)) {
#   print(paste(nodes[i],xnodes[i]== nodes[i]))
#   # for (i in 2:4) {
# }
# xsectors <- unique(allCUL$Sector)
# xsectors[3]
# xsectors[3] == onlythesesectors[5]
# 
# str_remove(xsectors[3]," ")
# str_remove(xsectors[3],"\\s")




######## PLOT ######## 


i=1
j=1


pdf(file.path(ofigs,"WUandCULbyCPbySector.pdf"), width=9, height=6)

for (i in 1:length(nodes)) {
# for (i in 7:8) {
  
  print(paste("Node",nodes[i]))
  
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
      ggplot(aes(x = Year, y = Value, color = Slot)) +
      geom_line() +
      # geom_point() +
      labs(title = paste(nodes[i],sectors[j]), y = "Depletions (AF/yr)")
    print(p)
    
    #monthly 
    p <- xx %>%
      group_by(Slot,Date) %>%
      ggplot(aes(x = Date, y = Value, color = Slot)) +
      geom_line() +
      labs(title = paste(nodes[i],sectors[j]), y = "Depletions (AF/mo)")
    print(p)
    
    
    # % monthly / annual distribution plot 
    p <- xx %>% 
      group_by(Slot,Year) %>%
      mutate(Distirubtion = Value/sum(Value)) %>% 
      group_by(Slot,MonthNum) %>%
      summarise(Distirubtion = mean(Distirubtion)) %>% 
      ggplot(aes(x = MonthNum, y = Distirubtion, color = Slot)) +
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
    
  } #end if no data 
    write.csv(allstats,file = file.path(ofigs,paste0(nodes[i]," Stats.csv")))

  } #end sector loop 


} #end node loop

dev.off()


