# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls()) #clear the enviornment

CRSSDIR <- Sys.getenv("CRSS_DIR")

results_dir <- file.path(CRSSDIR,"results") 

#Additional plotting functions and libraries
library('tidyverse') #ggplot2,dplyr,tidyr
library('devtools')
library(RWDataPlyr)
#see RWDATPlyr Workflow for more information
library(CRSSIO)
# plotEOCYElev() and csVarNames()
source('code/Stat_emp_ExcCrv.r')
source('code/stat-boxplot-custom.r')

# # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ## Load Feather with Processed Results 

scen_res_monthly <- feather::read_feather(path = file.path(CRSSDIR,'crsp_ops_data.feather'))
summary(scen_res_monthly)
scens <- unique(scen_res_monthly$ScenarioGroup)
scens
unique(scen_res_monthly$Variable)
length(unique(scen_res_monthly$Scenario))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Figs <- "June2021"

# mycolors <- c("#f8766d","#fcbe03","#000076","#ff0bff","#49ff49","#00ffff") #CRSS offical + match heather Base, LTSP, LTSP SMB, All

startyr = 2022 #filter out all years > this year
endyr = 2030 #2060 has a bad year of data
yrs2show <- startyr:endyr # can't use this until your run extends to end of 2023

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

# check folders
if(!file.exists(file.path(scen_dir, scens[1]))
   | !file.exists(file.path(scen_dir, scens[2])))
  stop('Scenarios folder(s) do not exist or scen_dir is set up incorrectly.
       Please ensure Scenarios is set correctly.')

# ofigs <- file.path(results_dir,mainScenGroup) 
ofigs <- file.path(results_dir,Figs) 

if (!file.exists(ofigs)) {
  message(paste('Creating folder:', ofigs))
  dir.create(ofigs)
}

message('Figures will be saved to: ', ofigs)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5. Plot monthly figures  
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if (T) {
  pdf(file.path(ofigs,paste("Monthly",Figs)), width=9, height=6)
  
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # ++++++++++++++++ vs year line plots   +++++++++++++++++++++++++++++++++++++++++++++++++++
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  variable = "FlamingGorge.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  #annual sum of outflows (should be in ac-ft to do this) vs year
  y_lab = "Annual Flow (1,000 ac-ft/yr)"
  
  #convert cfs to af/mo 
  convert <- scen_res_monthly %>%
    dplyr::filter(Variable == variable) 
  convert$Value = convert$Value*rep(AFMonCFS,times = length(convert$Value)/12)
  p <- convert %>%
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

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # ++++++++++++++++ DO vs year plots  +++++++++++++++++++++++++++++++++++++++++++++++++++
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## DO Occurances ##
p <- 
 scen_res_DO %>%
    dplyr::filter(Variable == "ExtendedOperations.PowellForecastDeficitFlag") %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario,TraceNumber,Year) %>%
    summarise(Value = sum(Value)/12) %>% #first sum by year/12 to get % of months in DO, keeping scens, traces, and years together
    dplyr::group_by(Scenario,Year)  %>%
    summarise(Value = mean(Value)) %>% #then avg all traces to get avg % of year in DO, keeping scens and years together 
  ggplot(aes(x = Year, y = Value, color = Scenario)) +
  scale_x_continuous(minor_breaks = 1990:3000, breaks = seq(1990,3000,5),
                     labels = seq(1990,3000,5), expand = c(0,0)) +
  # ylim = c(0,1) +
  # coord_cartesian(xlim =c(2021,2040), ylim = c(0,1), expand = expand) + #don't drop data
  geom_line() +
   # theme_light() + 
   scale_y_continuous(minor_breaks = seq(0,1,.05), breaks = seq(0,1,.1), labels = scales::percent) +
   scale_color_manual(values = mycolors) +
   labs(title = paste("Precent Of Traces with Drought Responce Operation Occuring"), y = "Precent Of Traces", x = "Year") #, caption = "Both scenarios include the same Drought Responce Operation.")
print(p)
if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Average Annual Precent Of Months In DO.png")), width = widths[1],height = heights[1])}

### Normal Release 
variable = "ExtendedOperations.FlamingGorgeNormalRelease"
title = paste(variable,first(yrs2show),"-",last(yrs2show))
#annual sum of outflows (should be in ac-ft to do this) vs year
y_lab = "Annual Flow (1,000 ac-ft/yr)"
#convert cfs to af/mo 
convert <- scen_res_DO %>%
  dplyr::filter(Variable == variable) 
convert$Value = convert$Value*rep(AFMonCFS,times = length(convert$Value)/12)
p <- convert %>%
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
  labs(title = paste("Average Annual Release Before Calculating DO"), y = y_lab, x = "Year")
print(p)
if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}

### Extra DO Release 
variable = "ExtendedOperations.FlamingGorgeReleaseDifference"
title = paste(variable,first(yrs2show),"-",last(yrs2show))
#annual sum of outflows (should be in ac-ft to do this) vs year
y_lab = "Annual Flow (1,000 ac-ft/yr)"
#convert cfs to af/mo 
convert <- scen_res_DO %>%
  dplyr::filter(Variable == variable) 
convert$Value = convert$Value*rep(AFMonCFS,times = length(convert$Value)/12)
p <- convert %>%
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
  labs(title = paste("Average Annual Additional Release For DO"), y = y_lab, x = "Year")
print(p)
if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}
 
### end DO ###

variable = "FlamingGorge.Storage"
title = paste(variable,first(yrs2show),"-",last(yrs2show))
y_lab = "EOCY Storage (1,000 ac-ft)"
exc_month = 12
p <- scen_res_monthly %>%
  dplyr::filter(Variable == variable) %>%
  mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
  dplyr::filter(MonthNum%in%exc_month) %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  theme_light() + 
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = mycolors) +
  labs(title = paste("Average EOCY",title), y = y_lab, x = "Year")
print(p)
if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Mean EOCY",variable,".png")), width = widths[1],height = heights[1])}

  variable = "FlamingGorge.Pool Elevation"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "EOCY Water Surface Elevation (ft)"
  exc_month = 12
  p <- scen_res_monthly %>%
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
  
  #powell.pe vs time  
  variable = "Powell.Pool Elevation"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "EOCY Water Surface Elevation (ft)"
  exc_month = 12
  p <- scen_res_monthly %>%
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
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}
  
  #powell.inflow vs time  
  variable = "Powell.Inflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "Annual Flow (1,000 ac-ft/yr)"

  p <- scen_res_monthly %>%
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
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # ++++++++++++++++ vs month boxplots   +++++++++++++++++++++++++++++++++++++++++++++++++++
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  variable = "FlamingGorge.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  #monthly boxplot of outflows vs month
  y_lab = "Monthly Flow (cfs)"
  target <- data.frame(yintercept=800)
  p <- scen_res_monthly %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario, MonthNum) %>%
    ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
    geom_boxplot() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    geom_hline(aes(yintercept=yintercept), data=target) +
    scale_x_discrete("Month",labels = month.abb) + #display abb. month names
    labs(title = title, y = y_lab)
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste(title,variable,".png")), width = widths[1],height = heights[1])}
  
  # monthly box plot of PE 
  variable = "FlamingGorge.Pool Elevation"
  y_lab = "EOM Water Surface Elevation (ft)"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  p <- scen_res_monthly %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario, MonthNum) %>%
    ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
    geom_boxplot() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    scale_x_discrete("Month",labels = month.abb) + #display abb. month names
    labs(title = title, y = y_lab)
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste(title,variable,".png")), width = widths[1],height = heights[1])}
  
  ylims <- c(6000,6040)  
  # zoomed in monthly box plot of PE 
  p <- scen_res_monthly %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario, MonthNum) %>%
    ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
    geom_boxplot() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    scale_x_discrete("Month",labels = month.abb) + #display abb. month names
    coord_cartesian(ylim = c(ylims[1],ylims[2]), expand = F) + #don't drop data 
    labs(title = title, y = y_lab)
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste('Zoom',title,variable,".png")), width = widths[1],height = heights[1])}
  
  #monthly boxplot of inflow vs month
  variable = "YampaAtDeerlodge.Gage Inflow"
  y_lab = "Monthly Flow (cfs)"  
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  
  p <- scen_res_monthly %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario, MonthNum) %>%
    ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
    geom_boxplot() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    scale_x_discrete("Month",labels = month.abb) + #display abb. month names
    labs(title = title, y = y_lab)
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste(title,variable,".png")), width = widths[1],height = heights[1])}
  
  #monthly boxplot of inflow vs month
  variable = "GreenNearJensen.Gage Inflow"
  y_lab = "Monthly Flow (cfs)"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  
  p <- scen_res_monthly %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario, MonthNum) %>%
    ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
    geom_boxplot() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    scale_x_discrete("Month",labels = month.abb) + #display abb. month names
    labs(title = title, y = y_lab)
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste(title,variable,".png")), width = widths[1],height = heights[1])}
  
  
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
  
  dev.off()
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5.b. Plot monthly Powell figures  
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if (T) {
  pdf(file.path(ofigs,paste("Monthly_Powell",Figs)), width=9, height=6)
  
  variable = "Powell.Inflow"
  title = "Lake Powell Monthly Inflow" #paste(variable,first(yrs2show),"-",last(yrs2show))
  #monthly boxplot of outflows vs month
  y_lab = "Monthly Flow (KAF/mo)"
  # target <- data.frame(yintercept=800)
  p <- scen_res_monthly %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF 
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario, MonthNum) %>%
    ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
    geom_boxplot() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    # geom_hline(aes(yintercept=yintercept), data=target) +
    scale_x_discrete("Month",labels = month.abb) + #display abb. month names
    labs(title = title, y = y_lab)
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste(title,variable,".png")), width = widths[1],height = heights[1])}
  
  # monthly box plot of PE 
  variable = "Powell.Pool Elevation"
  y_lab = "EOM Water Surface Elevation (ft)"
  title = "Lake Powell Pool Elevation" #paste(variable,first(yrs2show),"-",last(yrs2show))
  p <- scen_res_monthly %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario, MonthNum) %>%
    ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
    geom_boxplot() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    scale_x_discrete("Month",labels = month.abb) + #display abb. month names
    labs(title = title, y = y_lab)
  print(p)
  if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste(title,variable,".png")), width = widths[1],height = heights[1])}
  
  ####  Exceedance of Powell Inflow ####  
  y_lab = "Monthly Flow (KAF/mo)"
  variable = "Powell.Inflow"
  maintitle = "Lake Powell Monthly Inflow"
  
  #release april - july  
  exc_month <- c(4,5,6,7) # April - July
  title <- paste(maintitle,month.name[first(exc_month)],'-',month.name[last(exc_month)])
  # ymin <- c(0,0) ; ymax <- c(8600,50000); mybreaks <- c(2000,5000)#old GREAT 
  p <- scen_res_FG %>%
    dplyr::filter(Variable == variable) %>% 
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() +
    scale_color_manual(values = mycolors) +
    # coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
    # scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab) + #, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify  
  if(no_legend){p <- p + theme(legend.position="none")}
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 5',title,".png")), width = widths[1],height = heights[1])}
  
  #release Jul - Sept  
  exc_month <- c(7,8,9) # April - July
  title <- paste(maintitle,month.name[first(exc_month)],'-',month.name[last(exc_month)])
  # ymin <- c(0,0) ; ymax <- c(8600,50000); mybreaks <- c(2000,5000)#old GREAT 
  p <- scen_res_FG %>%
    dplyr::filter(Variable == variable) %>% 
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() +
    scale_color_manual(values = mycolors) +
    # coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
    # scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab) + #, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify  
  if(no_legend){p <- p + theme(legend.position="none")}
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 5',title,".png")), width = widths[1],height = heights[1])}
  
  #release Aug - Sept  
  exc_month <- c(8,9) # April - July
  title <- paste(maintitle,month.name[first(exc_month)],'-',month.name[last(exc_month)])
  # ymin <- c(0,0) ; ymax <- c(8600,50000); mybreaks <- c(2000,5000)#old GREAT 
  p <- scen_res_FG %>%
    dplyr::filter(Variable == variable) %>% 
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() +
    scale_color_manual(values = mycolors) +
    # coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
    # scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab) + #, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify  
  if(no_legend){p <- p + theme(legend.position="none")}
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 5',title,".png")), width = widths[1],height = heights[1])}
  
  #release Oct - Nov   
  exc_month <- c(10,11) # April - July
  title <- paste(maintitle,month.name[first(exc_month)],'-',month.name[last(exc_month)])
  # ymin <- c(0,0) ; ymax <- c(8600,50000); mybreaks <- c(2000,5000)#old GREAT 
  p <- scen_res_FG %>%
    dplyr::filter(Variable == variable) %>% 
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() +
    scale_color_manual(values = mycolors) +
    # coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
    # scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab) + #, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify  
  if(no_legend){p <- p + theme(legend.position="none")}
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 5',title,".png")), width = widths[1],height = heights[1])}
  
  #release Dec - Feb   
  exc_month <- c(12,1,2) # April - July
  title <- paste(maintitle,month.name[first(exc_month)],'-',month.name[last(exc_month)])
  # ymin <- c(0,0) ; ymax <- c(8600,50000); mybreaks <- c(2000,5000)#old GREAT 
  p <- scen_res_FG %>%
    dplyr::filter(Variable == variable) %>% 
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() +
    scale_color_manual(values = mycolors) +
    # coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
    # scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab) + #, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify  
  if(no_legend){p <- p + theme(legend.position="none")}
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 5',title,".png")), width = widths[1],height = heights[1])}
  
  #release Mar - Apr   
  exc_month <- c(3,4) # April - July
  title <- paste(maintitle,month.name[first(exc_month)],'-',month.name[last(exc_month)])
  # ymin <- c(0,0) ; ymax <- c(8600,50000); mybreaks <- c(2000,5000)#old GREAT 
  p <- scen_res_FG %>%
    dplyr::filter(Variable == variable) %>% 
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() +
    scale_color_manual(values = mycolors) +
    # coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
    # scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab) + #, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify  
  if(no_legend){p <- p + theme(legend.position="none")}
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 5',title,".png")), width = widths[1],height = heights[1])}
  
  dev.off()
}
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 6. Process Daily 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if (T) {
rdffile <-  "DailyFlows.rdf"

#Special rw scen agg since RWDataPlyr doesn't support daily
for (i in 1:length(scens)) {
  
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
  scen_res_x$Timestep <- as.POSIXct(strptime(scen_res_x$Timestep,"%Y-%m-%d %H:%M"))
  scen_res_x$Timestep <- as.Date(scen_res_x$Timestep) - 1 # fix first entry -  first entry is 2019-1-31 24:00 which gets converted to 2019-02-01
  # does rdf output not give Jan 1-30 for first month of daily slot? Seems like it 
  
  if(i == 1){
    scen_res = scen_res_x
  } else {
    scen_res = rbind.data.frame(scen_res,scen_res_x)
  }
} #close i Scenario loop

#get everything on a date
scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
#get a numeric month number
scen_res$MonthNum = as.numeric(format.Date(scen_res$Timestep, format = "%m"))
scen_res$DayNum = as.numeric(format.Date(scen_res$Timestep, format = "%d"))
scen_res$JDay = as.numeric(format.Date(scen_res$Timestep, format = "%j"))

# # Adding factors so ggplot does not alphebetize legend
scen_res$Scenario = factor(scen_res$Scenario, levels=names(scens))

scen_res_daily <- scen_res #save off so don't over write with monthly data 
saveRDS(scen_res_daily,file = file.path(ofigs,paste0("scen_res_daily.RDS")))
# scen_res_daily <- readRDS(file = file.path(ofigs,paste0("scen_res_daily.RDS")))

scen_res_daily <- scen_res_daily %>% #filter out scens you don't want to keep for plots
  dplyr::filter(Scenario %in% keepscens)
unique(scen_res_daily$Scenario)
}
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 7.  Daily plot 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if (T) {
y_lab <- "Daily Flow (cfs)" #default
caption <- NA
JensenTarget <- data.frame(yintercept=2000)
zoom <- T #if true zoom to 0 - 10kfs 
variables_daily <<- c("DailyFlows.FlamingGorgeDaily","DailyFlows.JensenDaily") 
var_title_alt_names_daily <- c("Flaming Gorge Daily Release","Jensen Daily Flows")

ylimlow <- c(1000,0)
ylimup <- c(3000,5000)

j=1
## create a pdf  
# pdf(file.path(ofigs,paste0("Daily",Figs)), width=9, height=6)
pdf(file.path(ofigs,paste0("Zoom_Daily",Figs)), width=9, height=6)

for (j in 1:length(variables_daily)) {
  variable <- variables_daily[j]
  title <- var_title_alt_names_daily[j]
  # customcaption <- customcaptions[j]
  # custom_y_lab <- custom_y_labs[j]
    
  #    -------------------        All Trace Mean Daily        ----------------------
  
#plot the dailys average by scenario and by Julian Day (all traces)
   p <- scen_res_daily %>%
    dplyr::filter(ObjectSlot == variable) %>% 
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario, JDay) %>% 
    dplyr::summarise(Value = mean(Value)) %>%
    ggplot(aes(JDay, Value, color = Scenario)) + 
    geom_line() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    # scale_y_continuous(labels = scales::comma) + #add commas to axis 
    # scale_y_continuous(limits = c(ylimlow[j],ylimup[j]), labels = scales::comma) + #### ZOOM ####
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
  p <- scen_res_daily %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(ObjectSlot == variable) %>% 
    dplyr::group_by(Scenario, MonthNum) %>% 
    ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
    geom_boxplot() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    # stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) + #too hard to render use generic 
    scale_x_discrete("Month",labels = month.abb) + #display abb. month names
    scale_y_continuous(labels = scales::comma) + #add commas to axis
    # coord_cartesian(ylim = c(ylimlow[j],ylimup[j]), expand = F) + #don't drop data !!! important when setting limits 
    labs(title = paste(title), y = y_lab) +
    # labs(caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  #Jensen add hline 
  if (variable == "DailyFlows.FlamingGorgeDaily"){
    p <- p + 
      geom_hline(aes(yintercept=yintercept), data=target) 
  } 
  if (variable == "DailyFlows.JensenDaily"){
    p <- p + 
      geom_hline(aes(yintercept=yintercept), data=JensenTarget) 
  } 
  print(p)
  if(printfigs_daily==T){ ggsave(filename = file.path(ofigs,paste(variable,".png")), width = widths[2],height = heights[2])}

  
  title <- paste('Zoom',var_title_alt_names_daily[j])
  #    -------------------        All Trace Boxplot        ----------------------
  p <- scen_res_daily %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(ObjectSlot == variable) %>% 
    dplyr::group_by(Scenario, MonthNum) %>% 
    ggplot(aes(x = factor(MonthNum), y = Value, color = Scenario)) +
    geom_boxplot() +
    theme_light() + 
    scale_color_manual(values = mycolors) +
    # stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) + #too hard to render use generic 
    scale_x_discrete("Month",labels = month.abb) + #display abb. month names
    scale_y_continuous(labels = scales::comma) + #add commas to axis
    coord_cartesian(ylim = c(ylimlow[j],ylimup[j]), expand = F) + #don't drop data !!! important when setting limits 
    labs(title = paste(title), y = y_lab) +
    # labs(caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  #Jensen add hline 
  if (variable == "DailyFlows.FlamingGorgeDaily"){
    p <- p + 
      geom_hline(aes(yintercept=yintercept), data=target) 
  } 
  if (variable == "DailyFlows.JensenDaily"){
    p <- p + 
      geom_hline(aes(yintercept=yintercept), data=JensenTarget) 
  } 
  print(p)
  if(printfigs_daily==T){ ggsave(filename = file.path(ofigs,paste('Zoom',variable,".png")), width = widths[2],height = heights[2])}
  
  
} # end j variables loop 

dev.off()
}
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 7. Plot Results - Percent Exceedance of Traces  ----- 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#all Exceedance plots 
if (T) { #set true for easy running all plots 
  #    ------------------- Requires monthly and daily have ran ----------------------
  # if (!(exists(scen_res_monthly) & exists(scen_res_daily))){warning('monthly & daily need to be processed first')}
  
  ## create a pdf  
  pdf(file.path(ofigs,paste("Exceedance",Figs)), width=9, height=6)
  
  no_legend <- F
  expand <- F
  
  variable = "FlamingGorge.Pool Elevation"
  var_title_alt_name <- "Flaming Gorge Elevation"
  y_lab = "Water Surface Elevation (ft)"
  
  #all PE Exceed
  title <- paste(var_title_alt_name)
  ylims <- c(5840,6060) # heathers GREAT report limits 
  p <- scen_res_monthly %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::group_by(Scenario, TraceNumber) %>%
    mutate(Value = max(Value)) %>%
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() +
    scale_color_manual(values = mycolors) +
    # geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
    labs(title = title,
         y = y_lab, caption = caption) +
    coord_cartesian(xlim =c(0,1), ylim = c(ylims[1],ylims[2]), expand = expand) + #don't drop data 
    scale_y_continuous(breaks=seq(ylims[1],ylims[2],40)) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  if(no_legend){p <- p + theme(legend.position="none")}
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 1',title,".png")), width = widths[1],height = heights[1])}
  
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
    theme_light() + 
    stat_eexccrv() +
    scale_color_manual(values = mycolors) +
    coord_cartesian(xlim =c(0,1), ylim = c(ylims[1],ylims[2]), expand = expand) + #don't drop data 
    scale_y_continuous(breaks=seq(ylims[1],ylims[2],40)) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    # ylim(ylims) +# geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  if(no_legend){p <- p + theme(legend.position="none")}
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 2',title,".png")), width = widths[1],height = heights[1])}
  
  
  #April-July PE only
  title <- paste('April-July',var_title_alt_name)
  ylims <- c(5840,6060) # heathers GREAT report limits 
  exc_month <- c(4,5,6,7) # April - July
  p <- scen_res_monthly %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    #all but one month otherwise would lump all the months together
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() +
    scale_color_manual(values = mycolors) +
    # coord_cartesian(xlim =c(0,1), ylim = c(ylims[1],ylims[2]), expand = expand) + #don't drop data
    scale_y_continuous(breaks=seq(ylims[1],ylims[2],40)) +
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    # ylim(ylims) +# geom_hline(aes(yintercept=yintercept), data=Muth_Avg) +
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  if(no_legend){p <- p + theme(legend.position="none")}
  print(p)
  # if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 2',title,".png")), width = widths[1],height = heights[1])}
  
  
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
    theme_light() + 
    stat_eexccrv() +
    scale_color_manual(values = mycolors) +
    labs(title = title,
         y = y_lab, caption = caption) +
    coord_cartesian(xlim =c(0,1), ylim = c(ylims[1],ylims[2]), expand = expand) + #don't drop data 
    scale_y_continuous(breaks=seq(ylims[1],ylims[2],40)) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  if(no_legend){p <- p + theme(legend.position="none")}
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 3',title,".png")), width = widths[1],height = heights[1])}

  caption <- ''
  variables = c("FlamingGorge.Outflow","GreenNearJensen.Gage Inflow")
  var_title_alt_names <- c("Flaming Gorge Monthly Release","Jensen Monthly Flows")
  variables_daily = c("DailyFlows.FlamingGorgeDaily", "DailyFlows.JensenDaily")
  var_title_alt_names_daily <- c("Flaming Gorge Daily Release","Jensen Daily Flows")
  # Muth_Avg <- data.frame(yintercept=2000) #could use this but wouldn't be able to loop through
  j=1
  ### flow exceed loop ###
  for (j in 1:2) {
    ####  daily  flow April-July ####  
    y_lab = "Daily Flow (cfs)"
    variable = variables_daily[j]
    var_title_alt_name <- var_title_alt_names_daily[j]
    
    #maximum annual release april - july by Trace 
    exc_month <- c(4,5,6,7) # April - July
    title <- paste('Maximum Annual',var_title_alt_name,month.name[first(exc_month)],'-',month.name[last(exc_month)])
    ymin <- c(0,0) ; ymax <- c(8600,50000); mybreaks <- c(2000,5000)#old GREAT 
    p <- scen_res_daily %>%
      dplyr::filter(ObjectSlot == variable) %>% 
      dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
      dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
      dplyr::group_by(Scenario,Year,TraceNumber) %>% #now I need to get this to summarize one max for the 4 months
      summarize(Value = max(Value)) %>%  # this works I just am picking monthly so its a avg of the daily. Need max daily 
      dplyr::group_by(Scenario) %>% 
      ggplot(aes(Value, color = Scenario)) +
      theme_light() + 
      stat_eexccrv() +
      scale_color_manual(values = mycolors) +
      coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data
      scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
      labs(title = title,
           y = y_lab, caption = caption) +
      theme(plot.caption = element_text(hjust = 0)) #left justify 
    print(p)
    if(no_legend){p <- p + theme(legend.position="none")}
    print(p)
    if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 4',title,".png")), width = widths[1],height = heights[1])}
    
    #release march - april  
    exc_month <- c(3,4) # 
    title <- paste(var_title_alt_name,month.name[first(exc_month)],'-',month.name[last(exc_month)])
    ymin <- c(0,0) ; ymax <- c(8600,50000); mybreaks <- c(2000,5000)#old GREAT 
    p <- scen_res_daily %>%
      dplyr::filter(ObjectSlot == variable) %>% 
      dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
      dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
      #all but one month otherwise would lump all the months together
      dplyr::group_by(Scenario) %>%
      ggplot(aes(Value, color = Scenario)) +
      theme_light() + 
      stat_eexccrv() +
      scale_color_manual(values = mycolors) +
      coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
      scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
      scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
      labs(title = title,
           y = y_lab, caption = caption) +
      theme(plot.caption = element_text(hjust = 0)) #left justify  
    if(no_legend){p <- p + theme(legend.position="none")}
    print(p)
    if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 45',title,".png")), width = widths[1],height = heights[1])}
    
    #release april - july  
    exc_month <- c(4,5,6,7) # April - July
    title <- paste(var_title_alt_name,month.name[first(exc_month)],'-',month.name[last(exc_month)])
    ymin <- c(0,0) ; ymax <- c(8600,50000); mybreaks <- c(2000,5000)#old GREAT 
    p <- scen_res_daily %>%
      dplyr::filter(ObjectSlot == variable) %>% 
      dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
      dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
      #all but one month otherwise would lump all the months together
      dplyr::group_by(Scenario) %>%
      ggplot(aes(Value, color = Scenario)) +
      theme_light() + 
      stat_eexccrv() +
      scale_color_manual(values = mycolors) +
      coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
      scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
      scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
      labs(title = title,
           y = y_lab, caption = caption) +
      theme(plot.caption = element_text(hjust = 0)) #left justify  
    if(no_legend){p <- p + theme(legend.position="none")}
    print(p)
    if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 5',title,".png")), width = widths[1],height = heights[1])}
    
    exc_month <- c(6) # June
    title = paste(var_title_alt_name,month.name[exc_month])
    ymin <- c(0,0) ; ymax <- c(8600,45000); mybreaks <- c(2000,10000)#old GREAT 
    p <- scen_res_daily %>%
      dplyr::filter(ObjectSlot == variable) %>% 
      dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
      dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
      #all but one month otherwise would lump all the months together
      dplyr::group_by(Scenario) %>%
      ggplot(aes(Value, color = Scenario)) +
      theme_light() + 
      stat_eexccrv() +
      scale_color_manual(values = mycolors) +
      coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
      scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
      scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
      labs(title = title,
           y = y_lab, caption = caption) +
      theme(plot.caption = element_text(hjust = 0)) #left justify  
    if(no_legend){p <- p + theme(legend.position="none")}
    print(p)
    if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 6',title,".png")), width = widths[1],height = heights[1])}
    
    exc_month <- c(7) # July
    title = paste(var_title_alt_name,month.name[exc_month])
    ymin <- c(0,0) ; ymax <- c(3000,30000);mybreaks <- c(1000,10000)#old GREAT#old GREAT 
    p <- scen_res_daily %>%
      dplyr::filter(ObjectSlot == variable) %>% 
      dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
      dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
      #all but one month otherwise would lump all the months together
      dplyr::group_by(Scenario) %>%
      ggplot(aes(Value, color = Scenario)) +
      theme_light() + 
      stat_eexccrv() +
      scale_color_manual(values = mycolors) +
      coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
      scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
      scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
      labs(title = title,
           y = y_lab, caption = caption) +
      theme(plot.caption = element_text(hjust = 0)) #left justify  
    if(no_legend){p <- p + theme(legend.position="none")}
    print(p)
    if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 7',title,".png")), width = widths[1],height = heights[1])}
    
    ####  monthly  flow August-December ####  
    variable = variables[j]
    var_title_alt_name <- var_title_alt_names[j]
    y_lab = "Monthly Flow (cfs)"
    
    exc_month <- c(8,9) # Aug - Sept
    title <- paste(var_title_alt_name,month.name[first(exc_month)],'-',month.name[last(exc_month)])
    ymin <- c(0,0) ; ymax <- c(3000,6000);mybreaks <- c(1000,1000)#old GREAT 
    p <- scen_res_monthly %>%
      dplyr::filter(Variable == variable) %>%
      dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
      dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
      #all but one month otherwise would lump all the months together
      dplyr::group_by(Scenario) %>%
      ggplot(aes(Value, color = Scenario)) +
      theme_light() + 
      stat_eexccrv() +
      scale_color_manual(values = mycolors) +
      coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
      scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
      scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
      labs(title = title,
           y = y_lab, caption = caption) +
      theme(plot.caption = element_text(hjust = 0)) #left justify 
    if(no_legend){p <- p + theme(legend.position="none")}
    print(p)
    if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 8',title,".png")), width = widths[1],height = heights[1])}
    
    exc_month <- c(10,11) # Oct - Nov
    title <- paste(var_title_alt_name,month.name[first(exc_month)],'-',month.name[last(exc_month)])
    ymin <- c(0,0) ; ymax <- c(3000,4500);mybreaks <- c(1000,1000)#old GREAT 
    p <- scen_res_monthly %>%
      dplyr::filter(Variable == variable) %>%
      dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
      dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
      #all but one month otherwise would lump all the months together
      dplyr::group_by(Scenario) %>%
      ggplot(aes(Value, color = Scenario)) +
      theme_light() + 
      stat_eexccrv() +  
      scale_color_manual(values = mycolors) +
      coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
      scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
      scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
      labs(title = title,
           y = y_lab, caption = caption) +
      theme(plot.caption = element_text(hjust = 0)) #left justify  
    if(no_legend){p <- p + theme(legend.position="none")}
    print(p)
    if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 9',title,".png")), width = widths[1],height = heights[1])}
    
    exc_month <- c(12,1,2) # Dec - Feb
    title <- paste(var_title_alt_name,month.name[first(exc_month)],'-',month.name[last(exc_month)])
    ymin <- c(0,0) ; ymax <- c(3000,4500);mybreaks <- c(1000,1000)#old GREAT 
    p <- scen_res_monthly %>%     
      dplyr::filter(Variable == variable) %>%
      dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
      dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
      dplyr::group_by(Scenario) %>%
      ggplot(aes(Value, color = Scenario)) +
      theme_light() + 
      stat_eexccrv() + 
      scale_color_manual(values = mycolors) +
      coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
      scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
      scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
      labs(title = title,
           y = y_lab, caption = caption) +
      theme(plot.caption = element_text(hjust = 0)) #left justify 
    if(no_legend){p <- p + theme(legend.position="none")}
    print(p)
    if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 92',title,".png")), width = widths[1],height = heights[1])}
    
  } #end exceed vars loop
  
  dev.off()
}
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 8. Plot Results - Expression Slots ----- NOT TESTED
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if(T){
#agg file specifying which slots
rw_agg_file <- "rw_agg_GREATExp.csv"
#read agg file specifying which slots
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) #ubres.rdf res.rdf
#vignette("rwdataplyr-workflow", package = "RWDataPlyr") # SHOULD i create custom winter agg? jsut do with dplyr

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res_exp <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
) 

# unique(scen_res_exp$Variable) #check variable names 

variables_mon <- c("Meet_JensenLL_EIS","Meet_JensenLL_CPMBF","Winter25" )
variables_ann <- c("Have_Spike","Winter_Volume" )
variables_mon <- c("Meet_JensenLL_EIS","Meet_JensenLL_CPMBF" )

#how often is each scenario meeting the EIS and CPMBF targets for reach 2 LL?
#1 if Jensen flow greater than CPMBF[Hclass] or EIS lower limit, any month 
df1 <- scen_res_exp %>%
  dplyr::filter(Variable %in% variables_mon) %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  # dplyr::filter(Month %in% month.name[c(1,2,12)]) %>% #only look a winter months 
  dplyr::filter(Month %in% month.name[c(7,8,9)]) %>% #only look a July-Sept 
  # dplyr::group_by(Scenario,Variable,Year) %>%
  # summarise(Value = sum(Value)) %>% #stop here if only want year
  dplyr::group_by(Scenario,Variable) %>%
  # summarise(Value = sum(Value))
  summarise(Value = mean(Value))

df1
#WHY MEET CPMBF much more often in July-Sept than EIS in All GREAT? In ModWet and Wet the requirement is less for CPMBF vs EIS so can that explain why there is 
#work on this part


##1 for any Dec-Feb with release 800, 0 else ##slot miss named should be Flow800orless 
df2 <- scen_res_exp %>%
  dplyr::filter(Variable == "Winter25") %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  # dplyr::filter(Month %in% month.name[c(1,2,12)]) %>%
  dplyr::group_by(Scenario,Variable) %>%
  # summarise(Value = sum(Value))
  summarise(Value = mean(Value))
df2$Variable = rep("WinterFlow800",times=length(df2$Variable))
df2
#rename! 


#winter volume sum of FG release December-Feb by year
df3 <- scen_res_exp %>%
  dplyr::filter(Variable  == "Winter_Volume") %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  # dplyr::group_by(Scenario,Variable,Year) %>%
  # summarise(Value = sum(Value)) %>% #stop here if only want year
  dplyr::group_by(Scenario,Variable) %>%
  # summarise(Value = sum(Value))
  summarise(Value = mean(Value))
df3 # winter volume is most in LTSP since less of the summer baseflow requests since the peak is later! 

p <- scen_res_exp %>%
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
if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Mean Winter Volume.png')), width = widths[1],height = heights[1])}
dev.off()
#WHY MEET CPMBF much more often in July-Sept than EIS in All GREAT? In ModWet and Wet the requirement is less for CPMBF vs EIS so can that explain why there is 
#work on this part

#check SPIKE - 1 if does by year
df4 <- scen_res_exp %>%
  dplyr::filter(Variable == "Have_Spike") %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  # dplyr::filter(Month %in% month.name[c(1,2,12)]) %>%
  dplyr::group_by(Scenario,Variable) %>%
  # summarise(Value = sum(Value))
  summarise(Value = mean(Value))
df4 #SMB is working, less spikes in All GREAT as expected 

write.csv(rbind(df1,df2,df3,df4), file.path(ofigs,paste('ExpressionSlots.csv'))) #uncomment Value = sum(Value) for all instances across all traces 
write.csv(rbind(df1,df2,df3,df4), file.path(ofigs,paste('ExpressionSlots_average.csv')))

saveRDS(scen_res_exp,file = file.path(ofigs,paste0("scen_res_exp.RDS")))

}
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 9. Plot Results - H Class
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if(T){
  
  rw_agg_file <- "rw_agg_FG_HClass.csv" # HClass 
  
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
  
  scen_res_hclass <- scen_res
  saveRDS(scen_res_hclass,file = file.path(ofigs,paste0("scen_res_hclass.RDS")))
  
  pdf(file.path(ofigs,paste0("FGAsp_HClass_",startyr,endyr,'.pdf')), width=9, height=6)
  
  variable = "FlamingGorgeData.BaseFlowHClass" #1] "FlamingGorgeData.BaseFlowHClass" "FlamingGorgeData.SpringHClass"  
  y_lab = "H Class"
  var_title_alt_name = "BaseFlowHClass"  
  
  
  exc_month <- 1:12 # Dec - Feb
  title <- paste(var_title_alt_name,month.name[first(exc_month)],'-',month.name[last(exc_month)])
  p <- scen_res %>%     
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    # dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() + 
    scale_color_manual(values = mycolors) +
    # coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
    # scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  if(no_legend){p <- p + theme(legend.position="none")}
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed ',title,".png")), width = widths[1],height = heights[1])}
  
  
  
  exc_month <- c(8,12,1,11,10,9,2) # Dec - Feb
  title <- paste(var_title_alt_name,month.name[first(exc_month)],'-',month.name[last(exc_month)])
  p <- scen_res %>%     
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() + 
    scale_color_manual(values = mycolors) +
    # coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
    # scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  if(no_legend){p <- p + theme(legend.position="none")}
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed ',title,".png")), width = widths[1],height = heights[1])}
  
  
  variable = "FlamingGorgeData.SpringHClass"  
  var_title_alt_name = "SpringHClass"
  exc_month <- c(4,5,6,7) # Apr - Jul
  title <- paste(var_title_alt_name,month.name[first(exc_month)],'-',month.name[last(exc_month)])
  p <- scen_res %>%     
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() + 
    scale_color_manual(values = mycolors) +
    # coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
    # scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  if(no_legend){p <- p + theme(legend.position="none")}
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed ',title,".png")), width = widths[1],height = heights[1])}
  
  
  variable = "FlamingGorgeData.YampaHClass"  
  var_title_alt_name = "YampaHClass"
  exc_month <- c(4,5,6,7) # Apr - Jul
  title <- paste(var_title_alt_name,month.name[first(exc_month)],'-',month.name[last(exc_month)])
  p <- scen_res %>%     
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
    dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
    dplyr::group_by(Scenario) %>%
    ggplot(aes(Value, color = Scenario)) +
    theme_light() + 
    stat_eexccrv() + 
    scale_color_manual(values = mycolors) +
    # coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
    # scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
    scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
    labs(title = title,
         y = y_lab, caption = caption) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  if(no_legend){p <- p + theme(legend.position="none")}
  print(p)
  if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed ',title,".png")), width = widths[1],height = heights[1])}
  
  dev.off()
  
  
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##############  Filter Out All Except Dry and Mod Dry H Class  ########
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# scen_res_monthly <- readRDS(file=file.path(ofigs,paste0("scen_res_monthly.RDS"))) #prevent neeed to reprocess
# scen_res_daily <- readRDS(file = file.path(ofigs,paste0("scen_res_daily.RDS")))
# scen_res_exp <- readRDS(file = file.path(ofigs,paste0("scen_res_exp.RDS")))
# scen_res_DO <- readRDS(file = file.path(ofigs,paste0("scen_res_DO.RDS")))
# scen_res_hclass <- readRDS(file = file.path(ofigs,paste0("scen_res_hclass.RDS")))

unique(scen_res_hclass$Scenario)
unique(scen_res_monthly$Scenario)

unique(scen_res_hclass$Variable)
hclass <- unique(scen_res_hclass$Variable) #base,spring,yampa
dim(scen_res_hclass)
dim(scen_res_hclass)
dim(df)
# 

class <- pivot_wider(scen_res_hclass,names_from = Variable,values_from = Value)

out <- scen_res_monthly %>%
  dplyr::filter(Variable == "FlamingGorge.Outflow")

df <- cbind.data.frame(out,BaseFlowHClass = class$FlamingGorgeData.BaseFlowHClass, SpringFlowHClass = class$FlamingGorgeData.SpringHClass)
tail(df)

df_filter <- df %>%
  dplyr::filter(df$SpringFlowHClass == 0 | df$SpringFlowHClass == 1 ) #%>%
df_filter <- df_filter %>%
  dplyr::filter(df_filter$BaseFlowHClass == 0 | df_filter$BaseFlowHClass == 1 ) 
head(df_filter)

y_lab = "Monthly Flow (cfs)"
title = "Dry OR ModDry Spring AND Baseflow H Class FG.Outflow Exceedance "
caption = "Only Spring & Baseflow H Class = Dry OR ModDry"
p <- df_filter %>% 
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  dplyr::group_by(Scenario) %>% 
  ggplot(aes(Value, color = Scenario)) +
  theme_light() + 
  stat_eexccrv() +
  scale_color_manual(values = mycolors) +
  # coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data
  scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
  labs(title = title,
       y = y_lab, caption = caption) +
  theme(plot.caption = element_text(hjust = 0)) #left justify 
print(p)
ggsave(filename = file.path(ofigs,paste(title,".png")), width = widths[1],height = heights[1])

summary(p)

# df2 <- out %>% ### this isn't working for finding the bypass only - just process the spill slot
#   mutate(Value = max(Value-4600,0)) %>% 
#   dplyr::filter(Value > 0) 
# p <- df2 %>%
#   dplyr::group_by(Scenario) %>% 
#   ggplot(aes(Value, color = Scenario)) +
#   theme_light() + 
#   stat_eexccrv() +
#   scale_color_manual(values = mycolors) +
#   # coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data
#   scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
#   labs(title = title,
#        y = y_lab, caption = caption) +
#   theme(plot.caption = element_text(hjust = 0)) #left justify 
# print(p)

# test1 <- df2 %>% #fails, gives same values 
#   dplyr::filter(Scenario == names(scens[1])) 
# test2 <- df2 %>%
#   dplyr::filter(Scenario == names(scens[4])) 
# mean(test1$Value)
# mean(test2$Value)

# test1 <- df_filter %>% #works 
#   dplyr::filter(Scenario == names(scens[1])) 
# test2 <- df_filter %>%
#   dplyr::filter(Scenario == names(scens[4])) 
# mean(test1$Value)
# mean(test2$Value)


# test1 <- out %>% #works 
#   dplyr::filter(Scenario == names(scens[1])) 
# test2 <- out %>%
#   dplyr::filter(Scenario == names(scens[4])) 
# mean(test1$Value)
# mean(test2$Value)
# 
# 
# test1 <- scen_res_monthly %>%
#   dplyr::filter(Scenario == names(scens[1])) 
# test2 <- scen_res_monthly %>%
#   dplyr::filter(Scenario == names(scens[4]))
# head(test1)
# head(test2)
# mean(test1$Value)
# mean(test2$Value)

