# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls()) #clear the enviornment

CRSSDIR <- Sys.getenv("CRSS_DIR")
# CRSSDIR <- "C:\\Users\\fellette\\Documents\\GIT\\crss.trirvw2020" #BA
CRSSDIR <- "C:/Users/cfelletter/Documents/CRSS.v6"

# # where scenarios are folder are kept
scen_dir <- file.path(CRSSDIR,"Scenario")
# #containing the sub folders for each ensemble

results_dir <- file.path(CRSSDIR,"results") 

library(RColorBrewer)
#Additional plotting functions and libraries
library('tidyverse') #ggplot2,dplyr,tidyr
library('devtools')
library(RWDataPlyr)
#see RWDATPlyr Workflow for more information
library(CRSSIO)
# plotEOCYElev() and csVarNames()
source('code/Stat_emp_ExcCrv.r')
source('code/stat-boxplot-custom.r')

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#all scens 
scens <- list(
  "v601_ST" = "V6.0.1,ISM1988_2020,2016Dems,IGv6.0.1,CRMMS_Most",
  "NavajoDev_ST" = "NavajoDev.V6.0.2,ISM1988_2020,2016Dems,NavajoRls.V6.0.2,CRMMS_Most",
  "9003_NoRefire" = "NavajoDev.V6.0.2,ISM1988_2020,2016Dems,V6.0.3 NoRefire,CRMMS_Most",
  "9003_Refire" = "NavajoDev.V6.0.2,ISM1988_2020,2016Dems,V6.0.3 wRefire,CRMMS_Most"
)

Figs <- "NavajoDev_ST_9003"

###

scens <- list(
  "Mar.V6.0.1" = "V6.0.1,ISM1988_2020,2016Dems,IGv6.0.1,CRMMS_Most",
  "NavajoDev.V6.0.2.9003" = "NavajoDev.V6.0.2,ISM1988_2020,2016Dems,V6.0.2.9003_FinalJune1,CRMMS_Most"
)

Figs <-
  'ST-NavajoDev.V6.0.2.9003 vs 6.0.1'

## pick which scens to plot from larger group to process and save as RDS files for later analysis 
keepscens <- names(scens)

Noscens <- length(keepscens)
# mycolors <- brewer.pal(n = Noscens, name = "Paired")
mycolors <- brewer.pal(n = Noscens, name = "Set1")

startyr = 2024 #filter out all years > this year
endyr = 2040 #2040 was what I was showing but 2040 fails for one DRO trace 
yrs2show <- startyr:endyr # can't use this until your run extends to end of 2023

caption <- ""

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

# if(any(list.files(ofigs) == "scen_res_monthly.RDS")){
#   #exists(file.path(ofigs,paste0("scen_res_monthly.RDS"))) # this only works for an object, so if was loaded could do exists("scen_res_monthly")
# 
#   scen_res_monthly <- readRDS(file=file.path(ofigs,paste0("scen_res_monthly.RDS"))) #prevent neeed to reprocess
#   scen_res_monthly <- scen_res_monthly %>% #filter out scens you don't want to keep for plots
#     dplyr::filter(Scenario %in% keepscens)
#   unique(scen_res_monthly$Scenario)
#   scen_res_monthly$Scenario = factor(scen_res_monthly$Scenario, levels=names(scens))
# 
#   scen_res_daily <- readRDS(file = file.path(ofigs,paste0("scen_res_daily.RDS")))
#   scen_res_daily <- scen_res_daily %>% #filter out scens you don't want to keep for plots
#     dplyr::filter(Scenario %in% keepscens)
#   unique(scen_res_daily$Scenario)
#   scen_res_daily$Scenario = factor(scen_res_daily$Scenario, levels=names(scens))
# 
# }

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results - monthly 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#file names 
# rdf <- "UBDO.rdf"
# rdf_slot_names(read.rdf(iFile = file.path(scen_dir,scens[1],rdf)))
# 
#   rdf <- "UBDO.rdf"
#   noslots<-4
#   rwa1 <- rwd_agg(data.frame(
#     file = c(rep(rdf, noslots)),
#     slot = c("ExtendedOperations.PowellForecastDeficitFlag","ExtendedOperations.PowellForecastDeficit",
#              "ExtendedOperations.FlamingGorgeNormalRelease","ExtendedOperations.FlamingGorgeReleaseDifference"
#     ), 
#     period = rep("asis", noslots), #c("cy", "eocy", "djf", "July", "wy", "asis"),
#     summary = rep(NA, noslots),#c("min", NA, "sum", NA, "sum", NA),
#     eval = rep(NA, noslots),#c("<=", rep(NA, 5)),
#     t_s = rep(NA, noslots),#c(1060, NA, 0.001, NA, 0.000001, NA),
#     variable = c("ExtendedOperations.PowellForecastDeficitFlag","ExtendedOperations.PowellForecastDeficit",
#                  "ExtendedOperations.FlamingGorgeNormalRelease","ExtendedOperations.FlamingGorgeReleaseDifference"),
#     stringsAsFactors = FALSE
#   ))  
  
  
#agg file specifying which slots
rw_agg_file <- "rw_agg_NavBluff.csv"
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
# saveRDS(scen_res_monthly,file = file.path(ofigs,paste0("scen_res_monthly.RDS")))
# scen_res_monthly <- readRDS(file=file.path(ofigs,paste0("scen_res_monthly.RDS"))) #prevent neeed to reprocess
         
scen_res_monthly <- scen_res_monthly %>% #filter out scens you don't want to keep for plots
  dplyr::filter(Scenario %in% keepscens)
unique(scen_res_monthly$Scenario)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5. Plot monthly figures  
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  pdf(file.path(ofigs,paste("Monthly",Figs)), width=9, height=6)
  
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # ++++++++++++++++ vs year line plots   +++++++++++++++++++++++++++++++++++++++++++++++++++
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # variable = "FlamingGorge.Outflow"
  # title = paste(variable,first(yrs2show),"-",last(yrs2show))
  # #annual sum of outflows (should be in ac-ft to do this) vs year
  # y_lab = "Annual Flow (1,000 ac-ft/yr)"
  # 
  # #convert cfs to af/mo 
  # convert <- scen_res_monthly %>%
  #   dplyr::filter(Variable == variable) 
  # convert$Value = convert$Value*rep(AFMonCFS,times = length(convert$Value)/12)
  # p <- convert %>%
  #   mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
  #   dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  #   dplyr::group_by(Scenario,TraceNumber,Year) %>%
  #   summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
  #   dplyr::group_by(Scenario,Year)  %>%
  #   summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
  #   ggplot(aes(x = Year, y = Value, color = Scenario)) +
  #   scale_x_continuous(breaks = 2021:2040) +
  #   geom_line() +
  #   theme_light() + 
  #   scale_y_continuous(labels = scales::comma) +
  #   scale_color_manual(values = mycolors) +
  #   labs(title = paste("Average Annual",title), y = y_lab, x = "Year")
  # print(p)http://127.0.0.1:46881/graphics/ac079631-bc56-4238-8926-260d40fe3047.png
  # if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}

 

  variable = "Bluff"
  title = "Bluff Inflow" #paste(variable,first(yrs2show),"-",last(yrs2show))
  #monthly boxplot of outflows vs month
  y_lab = "Flow (CFS)"
  target <- data.frame(yintercept=500)
  p <- scen_res_monthly %>%
    dplyr::filter(Variable == variable) %>%
    # mutate(Value = Value/1000) %>% #convert to KAF 
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
  
 dev.off()
  
  # #release Aug - Sept  
  # exc_month <- c(8,9) # April - July
  # title <- paste(maintitle,month.name[first(exc_month)],'-',month.name[last(exc_month)])
  # # ymin <- c(0,0) ; ymax <- c(8600,50000); mybreaks <- c(2000,5000)#old GREAT 
  # p <- scen_res_monthly %>%
  #   dplyr::filter(Variable == variable) %>% 
  #   dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  #   dplyr::filter(MonthNum%in%exc_month) %>% #This is currently set to filter
  #   #all but one month otherwise would lump all the months together
  #   dplyr::group_by(Scenario) %>%
  #   ggplot(aes(Value, color = Scenario)) +
  #   theme_light() + 
  #   stat_eexccrv() +
  #   scale_color_manual(values = mycolors) +
  #   # coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = expand) + #don't drop data 
  #   # scale_y_continuous(breaks=seq(ymin[j],ymax[j],mybreaks[j])) + 
  #   scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
  #   labs(title = title,
  #        y = y_lab) + #, caption = caption) +
  #   theme(plot.caption = element_text(hjust = 0)) #left justify  
  # if(no_legend){p <- p + theme(legend.position="none")}
  # print(p)
  # if(printfigs_exceed==T){ ggsave(filename = file.path(ofigs,paste('Exceed 5',title,".png")), width = widths[1],height = heights[1])}
  # 
  # 