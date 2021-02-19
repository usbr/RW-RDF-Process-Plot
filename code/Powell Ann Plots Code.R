# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls()) #clear the enviornment

CRSSDIR <- Sys.getenv("CRSS_DIR")
# CRSSDIR <- "C:\\Users\\cfelletter\\Documents\\CRSS.Offc" #results in old model dir

# # where scenarios are folder are kept
scen_dir <- file.path(CRSSDIR,"Scenario")
# #containing the sub folders for each ensemble

results_dir <- file.path(CRSSDIR,"results") 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#all scens full hydro 
scens <- list( ### don't comment these out use keepscens variabile #### 
               "Basecase" = "Base_7004,DNF,2007Dems,GREAT_7002_MinFlow,MTOM_Most", 
               "LTSP" = "LTSP_7004,DNF,2007Dems,GREAT_7002_MinFlow,MTOM_Most", 
               "CPMBF" = "CPM_7004,DNF,2007Dems,GREAT_7002_MinFlow,MTOM_Most",
               "SMB" = "SMB_7004,DNF,2007Dems,GREAT_7002_MinFlow,MTOM_Most",
               "LTSP&SMB" = "LTSP_SMB_7004,DNF,2007Dems,GREAT_7002_MinFlow,MTOM_Most",
               "LTSP,SMB,CPMBF" = "All_7004,DNF,2007Dems,GREAT_7002_MinFlow,MTOM_Most"#,
               
)

## pick which scens to plot from larger group to process and save as RDS files for later analysis 
keepscens <- names(scens)
keepscens <- c("Basecase","LTSP","LTSP&SMB","LTSP,SMB,CPMBF") #names(scens)[c(1,2,7,8)] #compare base and all w/wo new 7002 rls
keepscens <- c("Basecase","LTSP,SMB,CPMBF") #names(scens)[c(1,2,7,8)] #compare base and all w/wo new 7002 rls

Figs <- "7004mdl_7002rls.pdf"

#stress test compare
scens <- list( ### don't comment these out use keepscens variabile #### 
               "Basecase_Full" = "Base_7004,DNF,2007Dems,GREAT_7002_MinFlow,MTOM_Most", 
               "Basecase_Stress" = "Base_7004,ISM1988_2018,2007Dems,GREAT_7002_MinFlow,MTOM_Most", 
               "LTSP_Stress" = "LTSP_7004,ISM1988_2018,2007Dems,GREAT_7002_MinFlow,MTOM_Most",
               # "CPMBF_Stress" = "CPM_7004,ISM1988_2018,2007Dems,GREAT_7002_MinFlow,MTOM_Most",
               # "SMB_Stress" = "SMB_7004,ISM1988_2018,2007Dems,GREAT_7002_MinFlow,MTOM_Most",
               "LTSP&SMB_Stress" = "LTSP_SMB_7004,ISM1988_2018,2007Dems,GREAT_7002_MinFlow,MTOM_Most",
               "LTSP,SMB,CPMBF_Full" = "All_7004,DNF,2007Dems,GREAT_7002_MinFlow,MTOM_Most",
               "LTSP,SMB,CPMBF_Stress" = "All_7004,ISM1988_2018,2007Dems,GREAT_7002_MinFlow,MTOM_Most"#,
               
)




## pick which scens to plot from larger group to process and save as RDS files for later analysis 
keepscens <- names(scens)
# keepscens <- names(scens)[c(1,2,3,6,7,8)] #all but

Figs <- "Stress_7004mdl_7002rls.pdf"

mycolors <- c("#f8766d","#fcbe03","#000076","#ff0bff","#49ff49","#00ffff") #CRSS offical + match heather Base, LTSP, LTSP SMB, All
Noscens <- length(keepscens)
library(RColorBrewer)
# mycolors <- brewer.pal(n = Noscens, name = "Paired")
mycolors <- brewer.pal(n = Noscens, name = "Set1")
# library(scales)
# show_col(hue_pal()(8))
# mycolors <- hue_pal()(Noscens) # standard r colors 

startyr = 2021 #filter out all years > this year
endyr = 2040 #2060 has a bad year of data
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

#Additional plotting functions and libraries
library('tidyverse') #ggplot2,dplyr,tidyr
library('devtools')
library(RWDataPlyr)
#see RWDATPlyr Workflow for more information
library(CRSSIO)
# plotEOCYElev() and csVarNames()
source('code/Stat_emp_ExcCrv.r')
source('code/stat-boxplot-custom.r')

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

# # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ## Skip Process Results - Load RDS 

if(any(list.files(ofigs) == "scen_res_monthly.RDS")){ 
  #exists(file.path(ofigs,paste0("scen_res_monthly.RDS")))
  
  scen_res_monthly <- readRDS(file=file.path(ofigs,paste0("scen_res_monthly.RDS"))) #prevent neeed to reprocess
  scen_res_monthly <- scen_res_monthly %>% #filter out scens you don't want to keep for plots
    dplyr::filter(Scenario %in% keepscens)
  unique(scen_res_monthly$Scenario)
  scen_res_monthly$Scenario = factor(scen_res_monthly$Scenario, levels=names(scens))

  scen_res_daily <- readRDS(file = file.path(ofigs,paste0("scen_res_daily.RDS")))
  scen_res_daily <- scen_res_daily %>% #filter out scens you don't want to keep for plots
    dplyr::filter(Scenario %in% keepscens)
  unique(scen_res_daily$Scenario)
  scen_res_daily$Scenario = factor(scen_res_daily$Scenario, levels=names(scens))
  
  # scen_res_exp <- readRDS(file = file.path(ofigs,paste0("scen_res_exp.RDS")))
  
  scen_res_DO <- readRDS(file = file.path(ofigs,paste0("scen_res_DO.RDS")))
  scen_res_DO <- scen_res_DO %>% #filter out scens you don't want to keep for plots
    dplyr::filter(Scenario %in% keepscens)
  
  scen_res_hclass <- readRDS(file = file.path(ofigs,paste0("scen_res_hclass.RDS")))
  scen_res_hclass <- scen_res_hclass %>% #filter out scens you don't want to keep for plots
    dplyr::filter(Scenario %in% keepscens)
  
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results - annual 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if (T) {
#file names 
# rdf <- "UBDO.rdf"
# rdf_slot_names(read.rdf(iFile = file.path(scen_dir,scens[1],rdf)))

#agg file specifying which slots
rw_agg_file <- "PowellAnnSlots.csv"
#read agg file specifying which slots
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) #ubres.rdf res.rdf

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
) 

# unique(scen_res$Variable) #check variable names 

# #get everything on a date 
# scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d") #this takes so long! 
# #get a numeric month number
# scen_res$MonthNum = as.numeric(format.Date(scen_res$MonthNum, format = "%m"))
# 
# 
# #filter out incomplete years
# # scen_res <- scen_res %>%
# #     dplyr::filter(Year >= first(yrs2show)) %>%
# #     dplyr::filter(Year <= last(yrs2show))
# 
# AFMonCFS <- c(61.48760331,55.53719008,61.48760331,59.50413223,61.48760331,59.50413223,61.48760331,
#   61.48760331,59.50413223,61.48760331,59.50413223,61.48760331)
# CFSAFMon <- 1/AFMonCFS  

# # Adding factors so ggplot does not alphebetize legend
scen_res$Scenario = factor(scen_res$Scenario, levels=names(scens))
# head(scen_res)


scen_res_annual <- scen_res
scen_res <- scen_res_annual
saveRDS(scen_res_annual,file = file.path(ofigs,paste0("scen_res_annual.RDS")))
# scen_res_monthly <- readRDS(file=file.path(ofigs,paste0("scen_res_monthly.RDS"))) #prevent neeed to reprocess
         
scen_res <- scen_res %>% #filter out scens you don't want to keep for plots
  dplyr::filter(Scenario %in% keepscens)
unique(scen_res$Scenario)

source("code/Custom_Powell_Clouds.R")
}
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Process Results - UB Drought Operations 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if(T){
rdf <- "UBDO.rdf"
noslots<-4
rwa1 <- rwd_agg(data.frame(
  file = c(rep(rdf, noslots)),
  slot = c("ExtendedOperations.PowellForecastDeficitFlag","ExtendedOperations.PowellForecastDeficit",
           "ExtendedOperations.FlamingGorgeNormalRelease","ExtendedOperations.FlamingGorgeReleaseDifference"
  ), 
  period = rep("asis", noslots), #c("cy", "eocy", "djf", "July", "wy", "asis"),
  summary = rep(NA, noslots),#c("min", NA, "sum", NA, "sum", NA),
  eval = rep(NA, noslots),#c("<=", rep(NA, 5)),
  t_s = rep(NA, noslots),#c(1060, NA, 0.001, NA, 0.000001, NA),
  variable = c("ExtendedOperations.PowellForecastDeficitFlag","ExtendedOperations.PowellForecastDeficit",
               "ExtendedOperations.FlamingGorgeNormalRelease","ExtendedOperations.FlamingGorgeReleaseDifference"),
  stringsAsFactors = FALSE
))

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res_DO <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
) 

#save RDS and filter to keepscens 
saveRDS(scen_res_DO,file=file.path(ofigs,paste0("scen_res_DO.RDS"))) #prevent neeed to reprocess
scen_res_DO <- scen_res_DO %>% #filter out scens you don't want to keep for plots
  dplyr::filter(Scenario %in% keepscens)
unique(scen_res_DO$Scenario)
scen_res_DO$Scenario = factor(scen_res_DO$Scenario, levels=names(scens))

}
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
  scale_x_continuous(breaks = 2021:2040) +
  geom_line() +
   theme_light() + 
   scale_y_continuous(labels = scales::percent) +
   scale_color_manual(values = mycolors) +
   labs(title = paste("DO Average Annual Occurance"), y = "Mean Precent Of Months In DO", x = "Year")
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
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 7. Plot Results - Percent Exceedance of Traces  ----- 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#all Exceedance plots 
if (T) { #set true for easy running all plots 
  #    ------------------- Requires monthly and daily have ran ----------------------
  # if (!(exists(scen_res_monthly) & exists(scen_res_daily))){warning('monthly & daily need to be processed first')}
  
  ## create a pdf  
  pdf(file.path(ofigs,paste("Powell_Exceedance",Figs)), width=9, height=6)
  
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

