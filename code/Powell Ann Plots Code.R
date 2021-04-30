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
# #all scens full hydro 
# scens <- list( ### don't comment these out use keepscens variabile #### 
#                "Basecase" = "Base_7004,DNF,2007Dems,GREAT_7002_MinFlow,MTOM_Most", 
#                "LTSP" = "LTSP_7004,DNF,2007Dems,GREAT_7002_MinFlow,MTOM_Most", 
#                "CPMBF" = "CPM_7004,DNF,2007Dems,GREAT_7002_MinFlow,MTOM_Most",
#                "SMB" = "SMB_7004,DNF,2007Dems,GREAT_7002_MinFlow,MTOM_Most",
#                "LTSP&SMB" = "LTSP_SMB_7004,DNF,2007Dems,GREAT_7002_MinFlow,MTOM_Most",
#                "LTSP,SMB,CPMBF" = "All_7004,DNF,2007Dems,GREAT_7002_MinFlow,MTOM_Most"#,
#                
# )
# 
# ## pick which scens to plot from larger group to process and save as RDS files for later analysis 
# keepscens <- names(scens)
# keepscens <- c("Basecase","LTSP","LTSP&SMB","LTSP,SMB,CPMBF") #names(scens)[c(1,2,7,8)] #compare base and all w/wo new 7002 rls
# keepscens <- c("Basecase","LTSP,SMB,CPMBF") #names(scens)[c(1,2,7,8)] #compare base and all w/wo new 7002 rls
# 
# Figs <- "Full_7004mdl_7002rls.pdf"
# 
# #stress test compare
scens <- list( ### don't comment these out use keepscens variabile ####
               # "Basecase_Full" = "Base_7004,DNF,2007Dems,GREAT_7002_MinFlow,MTOM_Most",
               "Basecase" = "Base_7004,ISM1988_2018,2007Dems,GREAT_7002_MinFlow,MTOM_Most",
               # "LTSP_Stress" = "LTSP_7004,ISM1988_2018,2007Dems,GREAT_7002_MinFlow,MTOM_Most",
               # "CPMBF_Stress" = "CPM_7004,ISM1988_2018,2007Dems,GREAT_7002_MinFlow,MTOM_Most", #don't seem to have these results
               # "SMB_Stress" = "SMB_7004,ISM1988_2018,2007Dems,GREAT_7002_MinFlow,MTOM_Most",
               # "LTSP&SMB_Stress" = "LTSP_SMB_7004,ISM1988_2018,2007Dems,GREAT_7002_MinFlow,MTOM_Most",
               # "LTSP,SMB,CPMBF_Full" = "All_7004,DNF,2007Dems,GREAT_7002_MinFlow,MTOM_Most",
               "LTSP,SMB,CPMBF" = "All_7004,ISM1988_2018,2007Dems,GREAT_7002_MinFlow,MTOM_Most"#,

)
# 
# ## pick which scens to plot from larger group to process and save as RDS files for later analysis 
keepscens <- names(scens)
# # keepscens <- names(scens)[c(1,2,3,6,7,8)] #all but
# 
Figs <- "Stress_7004mdl_7002rls.pdf"

# mycolors <- c("#f8766d","#fcbe03","#000076","#ff0bff","#49ff49","#00ffff") #CRSS offical + match heather Base, LTSP, LTSP SMB, All
# 
# 
# Noscens <- length(keepscens)
# library(RColorBrewer)
# # mycolors <- brewer.pal(n = Noscens, name = "Paired")
# mycolors <- brewer.pal(n = Noscens, name = "Set1")
# # library(scales)
# # show_col(hue_pal()(8))
# # mycolors <- hue_pal()(Noscens) # standard r colors 

# scens <- list(
#   "Jan21 Offc" = "Jan2021_2022,ISM1988_2018,2016Dems,IG_DCP,MTOM_Most", #what I compared in my first anlysis
#   "9000" = "Jan2021_9000,ISM1988_2018,2016Dems,IG_DCP,MTOM_Most" #Verify 
# )
# 
# keepscens <- names(scens)
# 
# Figs <- "Jan21_9000_CFChanges"
# 

#only two scens use CRSS Process Colors 
mycolors <- c("#F8766D","#00BFC4") # Process CRSS Res plotColors red then blue 
# keepscens <- names(scens)[1:2]
# keepscens <- c("Basecase_Stress","LTSP,SMB,CPMBF_Stress")

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
library('readxl') #read_xlsx()
library('cowplot') #get_legend()
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

if(any(list.files(ofigs) == "scen_res_annual.RDS")){ 
  exists(file.path(ofigs,paste0("scen_res_monthly.RDS")))

  scen_res_FG <- readRDS(file=file.path(ofigs,paste0("scen_res_FG.RDS"))) #prevent neeed to reprocess
  # scen_res_FG <- readRDS(file=file.path(ofigs,paste0("scen_res_monthly.RDS"))) #old file name 
  scen_res_FG <- scen_res_FG %>% #filter out scens you don't want to keep for plots
    dplyr::filter(Scenario %in% keepscens)
  unique(scen_res_FG$Scenario)
  unique(scen_res_FG$Variable)
  scen_res_FG$Scenario = factor(scen_res_FG$Scenario, levels=names(scens))

  scen_res_annual <- readRDS(file=file.path(ofigs,paste0("scen_res_annual.RDS"))) #prevent neeed to reprocess
  scen_res_annual <- scen_res_annual %>% #filter out scens you don't want to keep for plots
    dplyr::filter(Scenario %in% keepscens)
  unique(scen_res_annual$Scenario)
  unique(scen_res_annual$Variable)
  scen_res_annual$Scenario = factor(scen_res_annual$Scenario, levels=names(scens))

  # scen_res_DO <- readRDS(file = file.path(ofigs,paste0("scen_res_DO.RDS")))
  # scen_res_DO <- scen_res_DO %>% #filter out scens you don't want to keep for plots
  #   dplyr::filter(Scenario %in% keepscens)
  
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
scen_res_annual <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
  # scen_dir = "C:/Users/cfelletter/Documents/CRSS working/GREAT/GREAT Scens"
  
) 

# unique(scen_res_annual$Variable) #check variable names 

# # Adding factors so ggplot does not alphebetize legend
scen_res_annual$Scenario = factor(scen_res_annual$Scenario, levels=names(scens))
# head(scen_res_annual)

scen_res_annual <- scen_res
scen_res <- scen_res_annual
saveRDS(scen_res_annual,file = file.path(ofigs,paste0("scen_res_annual.RDS")))
# scen_res_annual <- readRDS(file=file.path(ofigs,paste0("scen_res_annual.RDS"))) #prevent neeed to reprocess
         
scen_res_annual <- scen_res_annual %>% #filter out scens you don't want to keep for plots
  dplyr::filter(Scenario %in% keepscens)
unique(scen_res_annual$Scenario)
unique(scen_res_annual$Variable)


source("code/Custom_Powell_Clouds.R")

source("code/lee_ferry_volume_plot.R")
}



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results - Annual FG and Jensen 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if (T) {
  #file names 
  # rdf <- "UBDO.rdf"
  # rdf_slot_names(read.rdf(iFile = file.path(scen_dir,scens[1],rdf)))
  
  #agg file specifying which slots
  rw_agg_file <- "rw_agg_FGonly.csv" #includes Powell inflow and PE
  #read agg file specifying which slots
  rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) #ubres.rdf res.rdf
  
  #rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
  scen_res_FG <- rw_scen_aggregate(
    scens,
    agg = rwa1,
    scen_dir = scen_dir
  ) 
  
  # unique(scen_res_FG$Variable) #check variable names 
  
  #get everything on a date 
  scen_res_FG$MonthNum = as.Date(paste0(scen_res_FG$Year,scen_res_FG$Month,"01"), format = "%Y%B%d") #this takes so long! 
  #get a numeric month number
  scen_res_FG$MonthNum = as.numeric(format.Date(scen_res_FG$MonthNum, format = "%m"))
  
  
  #filter out incomplete years
  # scen_res_FG <- scen_res_FG %>%
  #     dplyr::filter(Year >= first(yrs2show)) %>%
  #     dplyr::filter(Year <= last(yrs2show))
  
  AFMonCFS <- c(61.48760331,55.53719008,61.48760331,59.50413223,61.48760331,59.50413223,61.48760331,
                61.48760331,59.50413223,61.48760331,59.50413223,61.48760331)
  CFSAFMon <- 1/AFMonCFS  
  
  # # Adding factors so ggplot does not alphebetize legend
  scen_res_FG$Scenario = factor(scen_res_FG$Scenario, levels=names(scens))
  # head(scen_res_FG)
  
  saveRDS(scen_res_FG,file = file.path(ofigs,paste0("scen_res_FG.RDS")))
  # scen_res_FG <- readRDS(file=file.path(ofigs,paste0("scen_res_FG.RDS"))) #prevent neeed to reprocess
  
  scen_res_FG <- scen_res_FG %>% #filter out scens you don't want to keep for plots
    dplyr::filter(Scenario %in% keepscens)
  unique(scen_res_FG$Scenario)


scen_res_FG <-  scen_res_FG %>%
    dplyr::group_by(Variable,Scenario,TraceNumber,Year) %>%
    mutate(WY = ifelse(MonthNum<10, Year, Year+1))  
  
#convert cfs to af/mo 
convert <- scen_res_FG %>%
  dplyr::filter(Variable%in%c("FlamingGorge.Outflow","GreenNearJensen.Gage Inflow")) 
convert$Value = convert$Value*rep(AFMonCFS,times = length(convert$Value)/12)


annual_cy <- convert %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #2060 has NA values so filter that out
  dplyr::group_by(Variable,Scenario,TraceNumber,Year) %>%
  summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
  mutate(Value = Value/1000000)

annual_wy <- convert %>%
  dplyr::filter(WY <= last(yrs2show)) %>% #2060 has NA values so filter that out
  dplyr::filter(WY >= 2022) %>% #2021 WY is incomplete so filter that out
  dplyr::group_by(Variable,Scenario,TraceNumber,WY) %>%
  summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
  mutate(Value = Value/1000000)

source("code/Custom_FG_Clouds.R")

}

# variable <- "FlamingGorge.Outflow"
# variable <- "GreenNearJensen.Gage Inflow"
# title = paste(variable,first(yrs2show),"-",last(yrs2show))
# #annual sum of outflows (should be in ac-ft to do this) vs year
# y_lab = "Annual Flow (1,000 ac-ft/yr)"
# 
# convert %>%
#   dplyr::filter(Variable == variable) %>%
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
# # print(p)
# 
# scen_res <- convert


