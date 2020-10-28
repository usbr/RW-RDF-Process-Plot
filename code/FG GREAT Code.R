# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# rm(list=ls()) #clear the enviornment 
# 
# ### Directory Set Up
# # where scenarios are folder are kept
# #containing the sub folders for each ensemble

CRSSDIR <- Sys.getenv("CRSS_DIR")

# # where scenarios are folder are kept
scen_dir <- file.path(CRSSDIR,"Scenario")
# #containing the sub folders for each ensemble

results_dir <- file.path(CRSSDIR,"results") 

source('C:/Users/cfelletter/Documents/RW-RDF-Process-Plot/code/Stat_emp_ExcCrv.r')
source('C:/Users/cfelletter/Documents/RW-RDF-Process-Plot/code/stat-boxplot-custom.r')

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#generic scenario locations 

#Font Forecast Model 
scens <- list(
  "Offc CRSS" = "Baseline,DNF,2007Dems,IG_DCP,MTOM_Most",
  "Baseline" = "Base_7001,DNF,2007Dems,GREAT_7001,MTOM_Most",
  "CPM" = "CPM_7001,DNF,2007Dems,GREAT_7001,MTOM_Most",
  "SMB" = "SMB_7001,DNF,2007Dems,GREAT_7001,MTOM_Most",
  "LTSP" = "LTSP_7001,DNF,2007Dems,GREAT_7001,MTOM_Most",
  "All GREAT" = "All_7001,DNF,2007Dems,GREAT_7001,MTOM_Most"
)

mainScenGroup <- names(scens)[2] #name of the subfolder this analysis will be stored

startyr = 2021 #filter out all years > this year
endyr = 2040 #2060 has a bad year of data
yrs2show <- startyr:endyr # can't use this until your run extends to end of 2023

#### Plot Controls #####
Figs <- "7001.pdf"#paste0('FGDev_MonthlyFigs_',startyr,endyr,'.pdf')
printfigs_monthly<-F#T#make png figures 
printfigs_daily<-T#T#make png figures since pdfs take FOREVER to open
printfigs_exceed<-F#T#make png figures 

# mycolors <- c("#009E73","#6bbd28","#0072B2") #for Sector plots dark green, light green, blue 
# mylinetypes <- c("dashed","solid","solid")
#standard powerpoint figure sizes 
# first is for monthly plots, second is for daily plots 
widths <- c(9.5,9.5)
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

# check folders
if(!file.exists(file.path(scen_dir, scens[1]))
   | !file.exists(file.path(scen_dir, scens[2])))
  stop('Scenarios folder(s) do not exist or scen_dir is set up incorrectly.
       Please ensure Scenarios is set correctly.')

ofigs <- file.path(results_dir,mainScenGroup) 
ofigs <- file.path(results_dir,Figs) 

if (!file.exists(ofigs)) {
  message(paste('Creating folder:', ofigs))
  dir.create(ofigs)
}

message('Figures will be saved to: ', ofigs)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results - monthly 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#file names 

# rdf <- "Streamgages.rdf"
# read.rdf(iFile = file.path(scen_dir,scens[1],rdf))

#agg file specifying which slots
rw_agg_file <- "rw_agg_FGonly.csv"
#read agg file specifying which slots
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) #ubres.rdf res.rdf

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
) 

unique(scen_res$Variable) #check variable names 

#get everything on a date 
scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d") #this takes so long! 
#get a numeric month number
scen_res$MonthNum = as.numeric(format.Date(scen_res$MonthNum, format = "%m"))

scen_res_monthly <- scen_res #don't let daily overwrite 
scen_res <- scen_res_monthly #don't let daily overwrite


#filter out incomplete years
# scen_res <- scen_res %>%
#     dplyr::filter(Year >= first(yrs2show)) %>%
#     dplyr::filter(Year <= last(yrs2show))

AFMonCFS <- c(61.48760331,55.53719008,61.48760331,59.50413223,61.48760331,59.50413223,61.48760331,
  61.48760331,59.50413223,61.48760331,59.50413223,61.48760331)
CFSAFMon <- 1/AFMonCFS  
convert <- scen_res %>%
  dplyr::filter(Variable == "FlamingGorge.Outflow") %>%
  dplyr::filter(Scenario == "Offc CRSS") #%>%
# convert <- scen_res[which(scen_res$Variable == "FlamingGorge.Outflow" && scen_res$Scenario == "Offc CRSS"),] # doesn't work for some reason 
#gotta be a better way than the below but it works... 
changeslots <- which(scen_res$Variable == "FlamingGorge.Outflow")[which(which(scen_res$Variable == "FlamingGorge.Outflow") %in% which(scen_res$Scenario == "Offc CRSS"))]
# convert == scen_res[changeslots,] #just a check 
convert$Value = convert$Value*rep(CFSAFMon,times = length(convert$Value)/12)
scen_res[changeslots,] = convert
# head(convert)

# #change a individual scenario name afte the fact 
# changeslots <- which(scen_res$Scenario == "Max Constrained")
# scen_res[changeslots,"Scenario"] = rep("800<=Max(Min(+25))",times = length(changeslots))
# unique(scen_res$Scenario)

# # Adding factors so ggplot does not alphebetize legend
scen_res$Scenario = factor(scen_res$Scenario, levels=names(scens))
# head(scen_res)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
scen_res_monthly <- scen_res
scen_res <- scen_res_monthly


## create a pdf  
pdf(file.path(ofigs,paste("Monthly",Figs)), width=9, height=6)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

variable = "FlamingGorge.Outflow"
title = paste(variable,first(yrs2show),"-",last(yrs2show))

#annual sum of outflows (should be in ac-ft to do this) vs year
y_lab = "Annual Flow (1,000 ac-ft/yr)"

#convert cfs to af/mo 
convert <- scen_res_monthly %>%
  dplyr::filter(Variable == variable) %>%
  mutate(Value = Value/1000)  #convert to KAF 
convert$Value = convert$Value*rep(AFMonCFS,times = length(convert$Value)/12)

p <- convert %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = sum(Value)) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  # geom_boxplot() +
  # scale_x_continuous(breaks = 1:12,labels = month.abb) + 
  # scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  scale_y_continuous(labels = scales::comma) +
  labs(title = paste("Average Annual",title), y = y_lab, x = "Year")
print(p)
if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}



# y_lab = "Sum of monthly cfs"
# p <- scen_res_monthly %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise(Value = sum(Value)) %>%
#   ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
#   geom_line() +
#   # geom_boxplot() +
#   # scale_x_discrete("Month",labels = month.abb) + #display abb. month names
#   labs(title = paste("Average Annual",title), y = y_lab)
# print(p)



#monthly boxplot of outflows vs month
# y_lab = "Monthly Flow (ac-ft/mon)"
y_lab = "Monthly Flow (cfs)"
target <- data.frame(yintercept=800)
p <- scen_res_monthly %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=yintercept), data=target) +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = title, y = y_lab)
print(p)
if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste(title,variable,".png")), width = widths[1],height = heights[1])}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

variable = "FlamingGorge.Pool Elevation"
y_lab = "PE (ft)"
title = paste(variable,first(yrs2show),"-",last(yrs2show))

# plot annual PE vs year
# REALLY should be using Alan's function to pick up the december PE not this average of all months but this will such 
p <- scen_res_monthly %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  # geom_boxplot() +
  # scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  # geom_hline(aes(yintercept=yintercept), data=target) +
  labs(title = paste("Average Annual",title), y = y_lab)
print(p)
if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}


# #annual boxplot of traces 
# p <- scen_res_monthly %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
#   dplyr::group_by(Scenario, Year) %>%
#   ggplot(aes(x = factor(Year), y = Value, color = Scenario)) +
#   # geom_boxplot() + #generic geom uses 1.5 * IQR for the whiskers
#   # custom has whiskers go to the 10th/90th
#   stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
#   labs(title = paste(variable,startyr,"-",endyr),
#        y = y_lab, x = "Year") 
# print(p)



# monthly box plot of PE 
y_lab = "End of Month PE (ft)"
p <- scen_res_monthly %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  dplyr::group_by(Scenario, MonthNum) %>%
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
  geom_boxplot() +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = title, y = y_lab)
print(p)
if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste(title,variable,".png")), width = widths[1],height = heights[1])}

dev.off()


#    -------------------        Percent Exceedance of Traces       ----------------------


if (T) { #set true for easy running all plots 
  ## create a pdf  
  pdf(file.path(ofigs,paste("Exceedance",Figs)), width=9, height=6)
  
  variable = "FlamingGorge.Pool Elevation"
  var_title_alt_name <- "Flaming Gorge Elevation"
  y_lab = "Water Surface Elevation (ft)"
  
  mycolors <- c("#009E73","#6bbd28","#0072B2","#6bbd28","#0072B2") #for Sector plots dark green, light green, blue
  mycolors <- c("#00ffff","#000076","#FFFF00","#ff0bff","#49ff49") #alphabetical scens 
  
  # heathers color #00ffff - cyan - , #49ff49 green  , #ff0bff purple , #000076 dark blue baseline
  
  
  #all PE Exceed
  title <- paste(var_title_alt_name)
  ylims <- c(5840,6060) # heathers GREAT report limits 
  p <- scen_res_monthly %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario, TraceNumber) %>%
    mutate(Value = max(Value)) %>%
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    stat_eexccrv() +
    scale_color_manual(values = mycolors) +
    ylim(ylims) +
    # geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
    labs(title = title,
         y = y_lab, caption = caption) +
    scale_x_continuous("Percent Exceedance",labels = scales::percent) + 
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste('Exceed',title,".png")), width = widths[1],height = heights[1])}
  
  #EOCY PE only
  title <- paste('EOCY',var_title_alt_name)
  ylims <- c(5840,6060) # heathers GREAT report limits 
  exc_month = 12
  p <- scen_res_monthly %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    stat_eexccrv() +
    scale_color_manual(values = mycolors) +
    ylim(ylims) +# geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
    labs(title = title,
         y = y_lab, caption = caption) +
    scale_x_continuous("Percent Exceedance",labels = scales::percent) + 
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste('Exceed',title,".png")), width = widths[1],height = heights[1])}
  
  #May 1 PE only
  title <- paste('May 1',var_title_alt_name)
  exc_month = 4
  p <- scen_res_monthly %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    stat_eexccrv() +
    scale_color_manual(values = mycolors) +
    ylim(ylims) +    # geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
    labs(title = title,
         y = y_lab, caption = caption) +
    scale_x_continuous("Percent Exceedance",labels = scales::percent) + 
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste('Exceed',title,".png")), width = widths[1],height = heights[1])}
  
  
  caption <- ''
  y_lab = "Monthly Flow (cfs)"
  
  variables = c("FlamingGorge.Outflow","GreenNearJensen.Gage Inflow")
  var_title_alt_names <- c("Flaming Gorge Monthly Release","Jensen Monthly Flows")
  # Muth_Avg <- data.frame(yintercept=2000) #could use this but wouldn't be able to loop through 
  
  #monthly flow exceed loops
  for (j in 1:2) {
    variable = variables[j]
    var_title_alt_name <- var_title_alt_names[j]
    
    #maximum annual release april - july by Trace 
    exc_month <- c(4,5,6,7) # April - July
    title <- paste('Maximum Annual',var_title_alt_name,month.name[first(exc_month)],'-',month.name[last(exc_month)])
    ymin <- c(0,0) ; ymax <- c(10000,50000)#old GREAT 
    p <- scen_res_monthly %>%
      dplyr::filter(Variable == variable) %>%
      dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
      dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
      #all but one month otherwise would lump all the months together
      dplyr::group_by(Scenario, TraceNumber) %>%
      mutate(Value = max(Value)) %>%
      dplyr::group_by(Scenario) %>%
      ggplot(aes(Value, color = Scenario)) +
      stat_eexccrv() +
      scale_color_manual(values = mycolors) +
      # ylim(c(ymin[j],ymax[j])) +
      coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j])) + #don't drop data 
      labs(title = title,
           y = y_lab, caption = caption) +
      scale_x_continuous("Percent Exceedance",labels = scales::percent) + 
      theme(plot.caption = element_text(hjust = 0)) #left justify 
    print(p)
    if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste('Exceed',title,".png")), width = widths[1],height = heights[1])}
    
    #release april - july  
    exc_month <- c(4,5,6,7) # April - July
    title <- paste(var_title_alt_name,month.name[first(exc_month)],'-',month.name[last(exc_month)])
    ymin <- c(0,0) ; ymax <- c(10000,50000)#old GREAT 
    p <- scen_res_monthly %>%
      dplyr::filter(Variable == variable) %>%
      dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
      dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
      #all but one month otherwise would lump all the months together
      dplyr::group_by(Scenario) %>%
      ggplot(aes(Value, color = Scenario)) +
      stat_eexccrv() +
      # geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
      labs(title = title,
           y = y_lab, caption = caption) +
      scale_x_continuous("Percent Exceedance",labels = scales::percent) + 
      theme(plot.caption = element_text(hjust = 0)) #left justify 
    print(p)
    if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste('Exceed',title,".png")), width = widths[1],height = heights[1])}
    
    exc_month <- c(6) # June
    title = paste(var_title_alt_name,month.name[exc_month])
    ymin <- c(0,0) ; ymax <- c(10000,45000)#old GREAT 
    p <- scen_res_monthly %>%
      dplyr::filter(Variable == variable) %>%
      dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
      dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
      #all but one month otherwise would lump all the months together
      dplyr::group_by(Scenario) %>%
      ggplot(aes(Value, color = Scenario)) +
      stat_eexccrv() +
      # geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
      labs(title = title,
           y = y_lab, caption = caption) +
      scale_x_continuous("Percent Exceedance",labels = scales::percent) + 
      theme(plot.caption = element_text(hjust = 0)) #left justify 
    print(p)
    if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste('Exceed',title,".png")), width = widths[1],height = heights[1])}
    
    exc_month <- c(7) # July
    title = paste(var_title_alt_name,month.name[exc_month])
    ymin <- c(0,0) ; ymax <- c(3000,30000)#old GREAT 
    p <- scen_res_monthly %>%
      dplyr::filter(Variable == variable) %>%
      dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
      dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
      #all but one month otherwise would lump all the months together
      dplyr::group_by(Scenario) %>%
      ggplot(aes(Value, color = Scenario)) +
      stat_eexccrv() +
      # geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
      labs(title = title,
           y = y_lab, caption = caption) +
      scale_x_continuous("Percent Exceedance",labels = scales::percent) + 
      theme(plot.caption = element_text(hjust = 0)) #left justify 
    print(p)
    if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste('Exceed',title,".png")), width = widths[1],height = heights[1])}
    
    exc_month <- c(8,9) # Aug - Sept
    title <- paste(var_title_alt_name,month.name[first(exc_month)],'-',month.name[last(exc_month)])
    ymin <- c(0,0) ; ymax <- c(3000,6000)#old GREAT 
    p <- scen_res_monthly %>%
      dplyr::filter(Variable == variable) %>%
      dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
      dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
      #all but one month otherwise would lump all the months together
      dplyr::group_by(Scenario) %>%
      ggplot(aes(Value, color = Scenario)) +
      stat_eexccrv() +
      # geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
      # multi month title
      labs(title = title,
           y = y_lab, caption = caption) +
      scale_x_continuous("Percent Exceedance",labels = scales::percent) + 
      theme(plot.caption = element_text(hjust = 0)) #left justify 
    print(p)
    if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste('Exceed',title,".png")), width = widths[1],height = heights[1])}
    
    exc_month <- c(10,11) # Oct - Nov
    title <- paste(var_title_alt_name,month.name[first(exc_month)],'-',month.name[last(exc_month)])
    ymin <- c(0,0) ; ymax <- c(3000,4500)#old GREAT 
    p <- scen_res_monthly %>%
      dplyr::filter(Variable == variable) %>%
      dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
      dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
      #all but one month otherwise would lump all the months together
      dplyr::group_by(Scenario) %>%
      ggplot(aes(Value, color = Scenario)) +
      stat_eexccrv() +  
      # geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
      # multi month title
      labs(title = title,
           y = y_lab, caption = caption) +
      scale_x_continuous("Percent Exceedance",labels = scales::percent) + 
      theme(plot.caption = element_text(hjust = 0)) #left justify 
    print(p)
    if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste(title,".png")), width = widths[1],height = heights[1])}
    
    exc_month <- c(12,1,2) # Dec - Feb
    title <- paste(var_title_alt_name,month.name[first(exc_month)],'-',month.name[last(exc_month)])
    ymin <- c(0,0) ; ymax <- c(3000,4500)#old GREAT 
    p <- scen_res_monthly %>%
      dplyr::filter(Variable == variable) %>%
      dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
      dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
      #all but one month otherwise would lump all the months together
      dplyr::group_by(Scenario) %>%
      ggplot(aes(Value, color = Scenario)) +
      stat_eexccrv() +
      # geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
      # multi month title
      labs(title = title,
           y = y_lab, caption = caption) +
      scale_x_continuous("Percent Exceedance",labels = scales::percent) + 
      theme(plot.caption = element_text(hjust = 0)) #left justify 
    print(p)
    if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste('Exceed',title,".png")), width = widths[1],height = heights[1])}
    
  } #end exceed vars loop
  
  dev.off()
}

#### WORK ON THIS ##### 

# #figure out which months are releasing minimum flow
# variable = "FlamingGorge.Outflow"
# y_lab = "Monthly Flow (cfs)"
# target <- data.frame(yintercept=800)
# df <- scen_res_monthly %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
#   mutate(MinFlow = 
#            if(Value<=800){1},else{0}) # %>%
# df
# summary(df)
# df %>% dplyr::filter(Month %in% month.name[c(7,8,9)]) %>% #only look a July-Sept 
#   dplyr::group_by(Scenario) %>%
#   summarise(Value = sum(Value))
# df

  dplyr::group_by(Scenario, MonthNum) %>%
  
  ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=yintercept), data=target) +
  scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = title, y = y_lab)
print(p)




# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results - Daily ----- NOT TESTED
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
variables <<- c("DailyFlows.FlamingGorgeDaily","DailyFlows.JensenDaily") 
rdffile <-  "DailyFlows.rdf"
# rdffiles <- rep("DailyFlows.rdf",times = length(variables))  #rdf file with slot you want
startyrs <<- rep(2040,times = length(variables)) #c(2019,2019,2019,2019,2019) #filter out all years > this year # NOT enabled in plot
endyrs <<- rep(2040,times = length(variables)) #Note: start year same as end year for Daily
# customcaptions <- rep(NA,times = length(variables)) #c(NA,NA,"Inflow Exceedance",NA) #NA or this will over write the default caption on boxplots
# custom_y_labs <- rep(NA,times = length(variables)) #c(NA,NA,"Inflow Exceedance",NA) #NA gives defaults, enter if want soemthing different
# Note: use of custom caption and labels

#Special rw scen agg since RWDataPlyr doesn't support daily
for (i in 1:length(scens)) {
  
  # for (j in 1:length(variables)) {
  # variable <- variables[j]
  # rdffile <- rdffiles[j]
  
  scen_res_x <- file.path(scen_dir,scens[i],rdffile) %>% #this means "pipe" the data to the next function
    rdf_to_rwtbl2() 
  # unique(scen_res_x$ObjectSlot)
  scen_res_x <- scen_res_x %>%
    filter(ObjectSlot == "DailyFlows.FlamingGorgeDaily" | ObjectSlot == "DailyFlows.JensenDaily")
  
  #add on Scenario since rdf_to_rwtbl2 doesn't add it
  scen_res_x <- cbind.data.frame(
    scen_res_x,
    Scenario = rep(names(scens)[i], Times = length(scen_res_x$Timestep))
  )
  # head(savescen_res_x)
  # head(scen_res_x
  #convert Timestep chr to POSIXct ## WHY does this take so long 
  scen_res_x$Timestep <- as.POSIXct(strptime(scen_res_x$Timestep,"%Y-%m-%d %H:%M"))
  scen_res_x$Timestep <- as.Date(scen_res_x$Timestep) - 1 # fix first entry -  first entry is 2019-1-31 24:00 which gets converted to 2019-02-01
  # does rdf output not give Jan 1-30 for first month of daily slot? Seems like it 
  
  if(i == 1){
    scen_res = scen_res_x
  } else {
    scen_res = rbind.data.frame(scen_res,scen_res_x)
  }
} #close i Scenario loop

unique(scen_res$ObjectSlot) #check variable names
unique(scen_res$Scenario) #check Scenario names

#get everything on a date
scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
#get a numeric month number
scen_res$MonthNum = as.numeric(format.Date(scen_res$Timestep, format = "%m"))
scen_res$DayNum = as.numeric(format.Date(scen_res$Timestep, format = "%d"))
scen_res$JDay = as.numeric(format.Date(scen_res$Timestep, format = "%j"))

# # Adding factors so ggplot does not alphebetize legend
scen_res$Scenario = factor(scen_res$Scenario, levels=names(scens))

scen_res_daily <- scen_res #save off so don't over write with monthly data 

y_lab <- "Daily Flow (cfs)" #default
caption <- NA
JensenTarget <- data.frame(yintercept=2000)
zoom <- T #if true zoom to 0 - 10kfs 

j=1

## create a pdf  
# pdf(file.path(ofigs,paste0("Daily",Figs)), width=9, height=6)
pdf(file.path(ofigs,paste0("Zoom_Daily",Figs)), width=9, height=6)

 
for (j in 1:length(variables)) {
  variable <- variables[j]
  title <- variable
  # startyr <- startyrs[j] 
  # endyr <- endyrs[j]
  # customcaption <- customcaptions[j]
  # custom_y_lab <- custom_y_labs[j]
    
  #    -------------------        All Trace Mean Daily        ----------------------
  
#plot the dailys average by scenario and by Julian Day (all traces)
   p <- scen_res %>%
    dplyr::filter(ObjectSlot == variable) %>% 
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario, JDay) %>% 
    dplyr::summarise(Value = mean(Value)) %>%
    ggplot(aes(JDay, Value, color = Scenario)) + 
    # geom_point() +
    geom_line() +
    # scale_y_continuous(labels = scales::comma) + #add commas to axis 
    scale_y_continuous(limits = c(0,5000), labels = scales::comma) + #### ZOOM ####
    labs(title = paste("Mean",variable), y = y_lab) 
     # labs(caption = caption) +
    # theme(plot.caption = element_text(hjust = 0)) #left justify 
  if (variable == "DailyFlows.JensenDaily"){
    p <- p + 
      geom_hline(aes(yintercept=yintercept), data=JensenTarget) 
      # scale_y_continuous(limits = c(0,10000), labels = scales::comma) 
  } 
  
  print(p)
  if(printfigs_daily==T){ ggsave(filename = file.path(ofigs,paste("Mean",variable,".png")), width = widths[2],height = heights[2])}
  

#    -------------------        All Trace Boxplot        ----------------------
  p <- scen_res %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(ObjectSlot == variable) %>% 
    dplyr::group_by(Scenario, MonthNum) %>% 
    ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
    # geom_boxplot() +
    stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) + 
    scale_x_discrete("Month",labels = month.abb) + #display abb. month names
    # scale_y_continuous(labels = scales::comma) + #add commas to axis 
    scale_y_continuous(limits = c(0,5000), labels = scales::comma) + #### ZOOM ####
    labs(title = paste(title), y = y_lab) +
    # labs(caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  #Jensen add hline 
  if (variable == "DailyFlows.JensenDaily"){
    p <- p + 
      geom_hline(aes(yintercept=yintercept), data=JensenTarget) 
    # scale_y_continuous(limits = c(0,10000), labels = scales::comma) 
  } 
  print(p)
  if(printfigs_daily==T){ ggsave(filename = file.path(ofigs,paste(variable,".png")), width = widths[2],height = heights[2])}
  

} # end j variables loop 

dev.off()


#plot all the dailies for a given year 
# p <- scen_res %>%
#     # dplyr::filter(Year <= plotyr) %>% #one run has 2023 so filter that out so axis work
#     dplyr::filter(ObjectSlot == "DailyFlows.FlamingGorgeDaily") %>% #one run has 2023 so filter that out so axis work
#     dplyr::group_by(Scenario, Timestep) %>% #don't need to do this since only one var
#     dplyr::summarise(Value = mean(Value)) %>%
#     ggplot(aes(Timestep, Value, color = Scenario)) + 
#     geom_point() +
#     geom_line() +
#     scale_y_continuous(labels = scales::comma) + #add commas to axis 
#     # labs(title = paste("Mean",variable,plotyr), y = y_lab,caption = caption) +
#     scale_x_date("Month", breaks = date_breaks("months"),
#                  labels = date_format("%b")) +
#     theme(plot.caption = element_text(hjust = 0)) #left justify 
#   print(p)
#   



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 6. Process Results - Expression Slots ----- NOT TESTED
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

scens <- list(
  # "Offc CRSS" = "Baseline,DNF,2007Dems,IG_DCP,MTOM_Most",
  "Baseline" = "Base_7001,DNF,2007Dems,GREAT_7001,MTOM_Most",
  "CPM" = "CPM_7001,DNF,2007Dems,GREAT_7001,MTOM_Most",
  "SMB" = "SMB_7001,DNF,2007Dems,GREAT_7001,MTOM_Most",
  "LTSP" = "LTSP_7001,DNF,2007Dems,GREAT_7001,MTOM_Most",
  "All GREAT" = "All_7001,DNF,2007Dems,GREAT_7001,MTOM_Most"
)


#agg file specifying which slots
rw_agg_file <- "rw_agg_GREATExp.csv"
#read agg file specifying which slots
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) #ubres.rdf res.rdf
#vignette("rwdataplyr-workflow", package = "RWDataPlyr") # SHOULD i create custom winter agg? jsut do with dplyr

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
) 

scen_stats <- scen_res #save off so don't over write with monthly data 

unique(scen_res$Variable) #check variable names 


variables_mon <- c("Meet_JensenLL_EIS","Meet_JensenLL_CPMBF","Winter25" )
variables_ann <- c("Have_Spike","Winter_Volume" )
variables_mon <- c("Meet_JensenLL_EIS","Meet_JensenLL_CPMBF" )


#how often is each scenario meeting the EIS and CPMBF targets for reach 2 LL?
df <- scen_res %>%
  dplyr::filter(Variable %in% variables_mon) %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  # dplyr::filter(Month %in% month.name[c(1,2,12)]) %>% #only look a winter months 
  dplyr::filter(Month %in% month.name[c(7,8,9)]) %>% #only look a July-Sept 
  # dplyr::group_by(Scenario,Variable,Year) %>%
  # summarise(Value = sum(Value)) %>% #stop here if only want year
  dplyr::group_by(Scenario,Variable) %>%
  summarise(Value = sum(Value))
df
#WHY MEET CPMBF much more often in July-Sept than EIS in All GREAT? In ModWet and Wet the requirement is less for CPMBF vs EIS so can that explain why there is 
#work on this part


#check winter request - 1 if Dec-Jan FG.Outflow is either AvgRelease * 1.25 OR Jensen LL *1.25 - Yampa
df <- scen_res %>%
  dplyr::filter(Variable == "Winter25") %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  # dplyr::filter(Month %in% month.name[c(1,2,12)]) %>%
  dplyr::group_by(Scenario,Variable) %>%
  summarise(Value = sum(Value))
df 
print('1 = got extra winter water, probably Jensen LL *1.25 - Yampa (or min Avg*1.25). 0 = something else controls either 800 or Jensen UP (max) ')

#winter volume 
df <- scen_res %>%
  dplyr::filter(Variable  == "Winter_Volume") %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  # dplyr::group_by(Scenario,Variable,Year) %>%
  # summarise(Value = sum(Value)) %>% #stop here if only want year
  dplyr::group_by(Scenario,Variable) %>%
  summarise(Value = sum(Value))
df # winter volume is most in LTSP since less of the summer baseflow requests since the peak is later! 

p <- scen_res %>%
  dplyr::filter(Variable  == "Winter_Volume") %>%
  dplyr::filter(Year <= 2040) %>% 
  dplyr::filter(Year > 2020) %>% #2020 only has December
  dplyr::group_by(Scenario,Year) %>% 
  summarise(Value = mean(Value)) %>% 
  ggplot(aes(x = Year, y = Value, color = Scenario)) +
    # geom_boxplot() +
    geom_line() +
    labs(title = "Mean Winter Volume Delivered", y = "AF/yr")
p  

#WHY MEET CPMBF much more often in July-Sept than EIS in All GREAT? In ModWet and Wet the requirement is less for CPMBF vs EIS so can that explain why there is 
#work on this part


#check SPIKE - 1 if does
df <- scen_res %>%
  dplyr::filter(Variable == "Have_Spike") %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  # dplyr::filter(Month %in% month.name[c(1,2,12)]) %>%
  dplyr::group_by(Scenario,Variable) %>%
  summarise(Value = sum(Value))
df #SMB is working, less spikes in All GREAT as expected 


head(df)
# # Adding factors so ggplot does not alphebetize legend
scen_res$Scenario = factor(scen_res$Scenario, levels=names(scens))
head(scen_res)
# 
# # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ## 4. Plot Custom UB Figures 
# # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ## create a pdf  
# pdf(file.path(ofigs,paste("Monthly",Figs)), width=9, height=6)
# 
# # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# variable = "FlamingGorge.Outflow"
# title = paste(variable,first(yrs2show),"-",last(yrs2show))
# 
# 
# # monthly box plot of PE 
# y_lab = "End of Month PE (ft)"
# p <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
#   dplyr::group_by(Scenario, MonthNum) %>%
#   ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
#   geom_boxplot() +
#   scale_x_discrete("Month",labels = month.abb) + #display abb. month names
#   labs(title = title, y = y_lab)
# print(p)
# if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste(title,variable,".png")), width = widths[1],height = heights[1])}


dev.off()







