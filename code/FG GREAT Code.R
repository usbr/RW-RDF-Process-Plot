# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls()) #clear the enviornment
# 
# ### Directory Set Up
# # where scenarios are folder are kept
# #containing the sub folders for each ensemble

CRSSDIR <- Sys.getenv("CRSS_DIR")

# # where scenarios are folder are kept
scen_dir <- file.path(CRSSDIR,"Scenario")
# #containing the sub folders for each ensemble

results_dir <- file.path(CRSSDIR,"results") 



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# Figs <- "7002_Heather.pdf"#paste0('FGDev_MonthlyFigs_',startyr,endyr,'.pdf')
mycolors <- c("#f8766d","#fcbe03","#000076","#ff0bff","#49ff49","#00ffff") #CRSS offical + match heather Base, LTSP, LTSP SMB, All
# # red,gold, Heathers: dark blue (base), pink, lime green , cyan 
# keepscens <- c("IG_NoLTSP,NoDO","IG_NoLTSP,DO","Basecase,NoDO","Basecase,DO","LTSP,SMB,CPMBF,NoDO","LTSP,SMB,CPMBF,DO")


#all scens 
scens <- list( ### don't comment these out use keepscens variabile #### 
               "NoRequests,NoDO" = "NoDO_Base_7003,DNF,2007Dems,NoAdaptive_GREAT_7001,MTOM_Most", # aka "IG_NoLTSP,NoDO" #this one is good to go, dont need it to be 7002
               "NoRequests,DO" = "Base_7003,DNF,2007Dems,NoAdaptive_GREAT_7001,MTOM_Most", # aka "IG_NoLTSP,DO" #Basecase2019: - old downramp
               # "IG_Offc,NoDO" = "NoDO_Base_7001,DNF,2007Dems,NoDO_IG,MTOM_Most", - old
               # "IG_Offc,DO" = "Base_7001,DNF,2007Dems,IG_Offc,MTOM_Most", #"Baseline,DNF,2007Dems,IG_DCP,MTOM_Most",
               "Basecase,NoDO" = "NoDO_Base_7002,DNF,2007Dems,NoDO_GREAT_7001,MTOM_Most",
               # "Basecase_No800,DO" = "No800_Base_7003,DNF,2007Dems,GREAT_7001,MTOM_Most", 
               "Basecase,DO" = "Base_7003,DNF,2007Dems,GREAT_7001,MTOM_Most", #reran formerly - Base_7002,DNF,2007Dems,GREAT_7001,MTOM_Most",
               "LTSP,DO" = "LTSP_7002,DNF,2007Dems,GREAT_7001,MTOM_Most", ### these need rerun
               "LTSP&SMB,DO" = "LTSP_SMB_7002,DNF,2007Dems,GREAT_7001,MTOM_Most",
               "LTSP,SMB,CPMBF,NoDO" = "NoDO_All_7002,DNF,2007Dems,NoDO_GREAT_7001,MTOM_Most",
               "LTSP,SMB,CPMBF,DO" = "All_7002,DNF,2007Dems,GREAT_7001,MTOM_Most"#,
               # "CPMBF,DO" = "CPM_7002,DNF,2007Dems,GREAT_7001,MTOM_Most",
               # "SMB,DO" = "SMB_7002,DNF,2007Dems,GREAT_7001,MTOM_Most"
)
Figs <- "7003.pdf"#paste0('FGDev_MonthlyFigs_',startyr,endyr,'.pdf')
mycolors <- c("#f8766d","#fcbe03","#000076","#ff0bff","#49ff49","#00ffff") #CRSS offical + match heather Base, LTSP, LTSP SMB, All

keepscens <- names(scens)
keepscens <- c("NoRequests,NoDO","NoRequests,DO","Basecase,DO","LTSP,DO","LTSP&SMB,DO","LTSP,SMB,CPMBF,DO")

Figs <- "7003_NoDOvDO.pdf"#paste0('FGDev_MonthlyFigs_',startyr,endyr,'.pdf')
mycolors <- c("#f8766d","#fcbe03","#000076","#ff0bff","#49ff49","#00ffff") #CRSS offical + match heather Base, LTSP, LTSP SMB, All
keepscens <- c("NoRequests,NoDO","NoRequests,DO")

#New no 800 scens 
scens <- list( ### don't comment these out use keepscens variabile #### 
               "Basecase,DO" = "Base_7003,DNF,2007Dems,GREAT_7001,MTOM_Most", #reran formerly - Base_7002,DNF,2007Dems,GREAT_7001,MTOM_Most",
               "Basecase_no800M-A,DO" = "No800_Base_7003,DNF,2007Dems,GREAT_7001,MTOM_Most", 
               "no800M-A,LTSP,SMB,CPMBF,DO" = "No800_All_7003,DNF,2007Dems,GREAT_7001,MTOM_Most",
               "LTSP,SMB,CPMBF,DO" = "All_7002,DNF,2007Dems,GREAT_7001,MTOM_Most"#,
               
)
Figs <- "7003_No800.pdf"#paste0('FGDev_MonthlyFigs_',startyr,endyr,'.pdf')
mycolors <- c("#f8766d","#fcbe03","#000076","#ff0bff","#49ff49","#00ffff") #CRSS offical + match heather Base, LTSP, LTSP SMB, All
keepscens <- names(scens)



# Noscens <- length(keepscens)
# library(RColorBrewer)
# mycolors <- brewer.pal(n = Noscens, name = "Paired")
# mycolors <- brewer.pal(n = Noscens, name = "Set1")
# library(scales)
# show_col(hue_pal()(8))
# mycolors <- hue_pal()(Noscens) # standard r colors 

mainScenGroup <- names(scens)[2] #name of the subfolder this analysis will be stored

startyr = 2021 #filter out all years > this year
endyr = 2040 #2060 has a bad year of data
yrs2show <- startyr:endyr # can't use this until your run extends to end of 2023

#### Plot Controls #####
printfigs_monthly<-T#T#make png figures 
printfigs_daily<-T#T#make png figures since pdfs take FOREVER to open
printfigs_exceed<-T#T#make png figures 
 
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
source('C:/Users/cfelletter/Documents/RW-RDF-Process-Plot/code/Stat_emp_ExcCrv.r')
source('C:/Users/cfelletter/Documents/RW-RDF-Process-Plot/code/stat-boxplot-custom.r')

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

scen_res_exp <- readRDS(file = file.path(ofigs,paste0("scen_res_exp.RDS")))

scen_res_DO <- readRDS(file = file.path(ofigs,paste0("scen_res_DO.RDS")))
scen_res_DO <- scen_res_DO %>% #filter out scens you don't want to keep for plots
  dplyr::filter(Scenario %in% keepscens)

# keepscens <- c("NoRequests,NoDO","NoRequests,DO","Basecase,DO","LTSP,DO","LTSP&SMB,DO","LTSP,SMB,CPMBF,DO")
# scen_res_monthly <- scen_res_monthly %>% #filter out scens you don't want to keep for plots
#   dplyr::filter(Scenario %in% keepscens)
# scen_res_daily <- scen_res_daily %>% #filter out scens you don't want to keep for plots
#   dplyr::filter(Scenario %in% keepscens)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results - monthly 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if (T) {
#file names 
# rdf <- "UBDO.rdf"
# rdf_slot_names(read.rdf(iFile = file.path(scen_dir,scens[1],rdf)))

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

# unique(scen_res$Variable) #check variable names 

#get everything on a date 
scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d") #this takes so long! 
#get a numeric month number
scen_res$MonthNum = as.numeric(format.Date(scen_res$MonthNum, format = "%m"))


#filter out incomplete years
# scen_res <- scen_res %>%
#     dplyr::filter(Year >= first(yrs2show)) %>%
#     dplyr::filter(Year <= last(yrs2show))

AFMonCFS <- c(61.48760331,55.53719008,61.48760331,59.50413223,61.48760331,59.50413223,61.48760331,
  61.48760331,59.50413223,61.48760331,59.50413223,61.48760331)
CFSAFMon <- 1/AFMonCFS  

# # Adding factors so ggplot does not alphebetize legend
scen_res$Scenario = factor(scen_res$Scenario, levels=names(scens))
# head(scen_res)


scen_res_monthly <- scen_res
scen_res <- scen_res_monthly
saveRDS(scen_res_monthly,file = file.path(ofigs,paste0("scen_res_monthly.RDS")))
# scen_res_monthly <- readRDS(file=file.path(ofigs,paste0("scen_res_monthly.RDS"))) #prevent neeed to reprocess
         
scen_res_monthly <- scen_res_monthly %>% #filter out scens you don't want to keep for plots
  dplyr::filter(Scenario %in% keepscens)
unique(scen_res_monthly$Scenario)
}
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 9. Plot Results - Drought Operations Slots ----- NOT TESTED
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##### UB DO stats #####  #####  #####  #####  #####  #####  #####  #####  #####
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
## 4. Plot monthly figures  
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#all Monthly plots 
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
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
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

 
  ### end DO ####
  

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
## 5. Process Daily 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Daily plots process 
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
## 6.  Daily plot 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Daily plots  
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

#    ------------------- Requires monthly and daily have ran ----------------------
# if (!(exists(scen_res_monthly) & exists(scen_res_daily))){warning('monthly & daily need to be processed first')}


#all Exceedance plots 
if (T) { #set true for easy running all plots 
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
## 9. Plot Results - Drought Operations Slots ----- NOT TESTED
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##### UB DO stats #####  #####  #####  #####  #####  #####  #####  #####  #####

# rdf <- "UBDO.rdf"
# noslots<-4
# rwa1 <- rwd_agg(data.frame(
#   file = c(rep(rdf, noslots)),
#   slot = c("ExtendedOperations.PowellForecastDeficitFlag","ExtendedOperations.PowellForecastDeficit",
#            "ExtendedOperations.FlamingGorgeNormalRelease","ExtendedOperations.FlamingGorgeReleaseDifference"
#   ), 
#   period = rep("asis", noslots), #c("cy", "eocy", "djf", "July", "wy", "asis"),
#   summary = rep(NA, noslots),#c("min", NA, "sum", NA, "sum", NA),
#   eval = rep(NA, noslots),#c("<=", rep(NA, 5)),
#   t_s = rep(NA, noslots),#c(1060, NA, 0.001, NA, 0.000001, NA),
#   variable = c("ExtendedOperations.PowellForecastDeficitFlag","ExtendedOperations.PowellForecastDeficit",
#                "ExtendedOperations.FlamingGorgeNormalRelease","ExtendedOperations.FlamingGorgeReleaseDifference"),
#   stringsAsFactors = FALSE
# ))
# 
# #rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
# scen_res_DO <- rw_scen_aggregate(
#   scens,
#   agg = rwa1,
#   scen_dir = scen_dir
# ) 
# 
# #save RDS and filter to keepscens 
# saveRDS(scen_res_DO,file=file.path(ofigs,paste0("scen_res_DO.RDS"))) #prevent neeed to reprocess
# scen_res_DO <- scen_res_DO %>% #filter out scens you don't want to keep for plots
#   dplyr::filter(Scenario %in% keepscens)
# unique(scen_res_DO$Scenario)
# scen_res_DO$Scenario = factor(scen_res_DO$Scenario, levels=names(scens))


df <- scen_res_DO %>%
  dplyr::filter(Variable == "ExtendedOperations.PowellForecastDeficitFlag") %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  # dplyr::filter(Month %in% month.name[c(1,2,12)]) %>%
  dplyr::group_by(Scenario,Year) %>%
  summarise(Value = sum(Value))

## DO Occurances ##
# p <- df %>%
  scen_res_DO %>%
  dplyr::filter(Variable == "ExtendedOperations.PowellForecastDeficitFlag") %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  dplyr::group_by(Scenario,Year) %>%
  summarise(Value = sum(Value)) %>%
  ggplot(aes(x = Year, y = Value, color = Scenario)) +
  geom_line() +
  theme_light() + 
  scale_color_manual(values = mycolors) +
  labs(title = "DO Occurances", y = "Count")
print(p)
if(printfigs_DO==T){ ggsave(filename = file.path(ofigs,paste(title,variable,".png")), width = widths[1],height = heights[1])}

## Average Defecit Volume ## 
# p <- df %>%
scen_res_DO %>%
  dplyr::filter(Variable == "ExtendedOperations.PowellForecastDeficit") %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  dplyr::group_by(Scenario,Year) %>%
  summarise(Value = mean(Value)) %>%
  mutate(Value = Value/1000000) %>%  #convert to MAF 
  ggplot(aes(x = Year, y = Value, color = Scenario)) +
  geom_line() +
  theme_light() + 
  scale_color_manual(values = mycolors) +
  labs(title = "Mean Powell Forecast Deficit", y = "Deficit (MAF)")
print(p)
if(printfigs_DO==T){ ggsave(filename = file.path(ofigs,paste(title,variable,".png")), width = widths[1],height = heights[1])}


## Average Additional Release ## 
# p <- df %>%
variable = "ExtendedOperations.FlamingGorgeReleaseDifference"
scen_res_DO %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  dplyr::group_by(Scenario,Year) %>%
  summarise(Value = mean(Value)) %>%
  mutate(Value = Value/1000) %>%  #convert to KAF 
  ggplot(aes(x = Year, y = Value, color = Scenario)) +
  geom_line() +
  theme_light() + 
  scale_color_manual(values = mycolors) +
  labs(title = "Mean FG Additional DO Release", y = "Release (KAF)")
print(p)
if(printfigs_DO==T){ ggsave(filename = file.path(ofigs,paste(title,variable,".png")), width = widths[1],height = heights[1])}

## Average Normal Release ## 
variable = "ExtendedOperations.FlamingGorgeNormalRelease"
scen_res_DO %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  mutate(Value = Value/1000) #convert to KAF
convert$Value = convert$Value*rep(AFMonCFS,times = length(convert$Value)/12)
convert %>%
  dplyr::group_by(Scenario,Year) %>%
  summarise(Value = sum(Value)) %>%
  ggplot(aes(x = Year, y = Value, color = Scenario)) +
  geom_line() +
  theme_light() + 
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = mycolors) +
  labs(title = variable, y = y_lab)
print(p)
if(printfigs_DO==T){ ggsave(filename = file.path(ofigs,paste(title,variable,".png")), width = widths[1],height = heights[1])}

variable = "ExtendedOperations.FlamingGorgeReleaseDifference"
df_combined <- x <- scen_res_DO %>%
  dplyr::filter(Variable == variable) 
variable = "ExtendedOperations.FlamingGorgeNormalRelease"
y <- scen_res_DO %>%
  dplyr::filter(Variable == variable) 
df_combined$Value = x$Value + y$Value  
# View(df_combined)
#convert cfs to af/mo 

variable = "FlamingGorge.Outflow"
# df_out <- scen_res_monthly %>%
#   dplyr::filter(Variable == variable) 
# summary(df_combined$Value - df_out$Value) #checks out 
#annual sum of outflows (should be in ac-ft to do this) vs year
y_lab = "Annual Flow (1,000 ac-ft/yr)"

convert <- df_combined %>%
  # dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  mutate(Value = Value/1000) #convert to KAF
convert$Value = convert$Value*rep(AFMonCFS,times = length(convert$Value)/12)
convert %>%
  dplyr::group_by(Scenario,TraceNumber,Year) %>%
  summarise(Value = sum(Value)) %>%
  dplyr::group_by(Scenario,Year) %>%
  summarise(Value = mean(Value)) #%>%
  ggplot(aes(x = Year, y = Value, color = Scenario)) +
  geom_line() +
  theme_light() + 
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = mycolors) +
  labs(title = variable, y = y_lab)
#MATCHES  Monthly Trace 
print(p)

convert <- df_combined %>%
  # dplyr::filter(Variable == variable) %>%
  dplyr::filter(Year <= last(yrs2show)) #2060 has NA values so filter that out
convert$Value = convert$Value*rep(AFMonCFS,times = length(convert$Value)/12)
df <- convert %>%
  mutate(Value = Value/1000) #convert to KAF after we convert to AF  

View(convert)
#average number of occurances of DO 
# df <- 
convert %>%
  dplyr::group_by(Scenario,TraceNumber,Year) %>%
  summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
  dplyr::group_by(Scenario,Year)  %>%
  summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
  ggplot(aes(x = Year, y = Value, color = Scenario)) +
  geom_line() +
  theme_light() + 
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = mycolors) +
  labs(title = variable, y = y_lab)
View(df)

#what I'm using in monthly code
# p <- convert %>%
#   dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise(Value = sum(Value)) %>%


df <- convert %>%
  dplyr::group_by(Scenario,Year) %>%
  summarise(Value = sum(Value)) #%>%
  dplyr::group_by(Scenario,Year) %>%
  summarise(Value = mean(Value)) #%>%
View(df)

  # summarise(Value = sum(Value))
  summarise(Value = sum(Value))
df 

# df <- scen_res_DO %>%
#   dplyr::filter(Variable == "ExtendedOperations.PowellForecastDeficit") %>%
#   dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
#   # dplyr::filter(Month %in% month.name[c(1,2,12)]) %>%
#   dplyr::group_by(Scenario,Variable) %>%
#   # summarise(Value = sum(Value))
#   summarise(Value = sum(Value))
# df 






df <- cbind(df1,df2)
# head(df)
write.csv(df,file = file.path(ofigs,paste("Trace",trace_no,"DO_Stats.csv")))

dev.off()
