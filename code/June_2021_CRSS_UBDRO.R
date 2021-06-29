# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls()) #clear the enviornment

.rs.restartR() # need to restart R session to fully clean the memory/RAM. 

CRSSDIR <- Sys.getenv("CRSS_DIR")

results_dir <- file.path(CRSSDIR,"results") 
results_dir <- "M:/felletter"


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

Figs <- "June2021"

# mycolors <- c("#f8766d","#fcbe03","#000076","#ff0bff","#49ff49","#00ffff") #CRSS offical + match heather Base, LTSP, LTSP SMB, All

startyr = 2022 #filter out all years > this year
endyr = 2026 #2060 has a bad year of data
yrs2show <- startyr:endyr # can't use this until your run extends to end of 2023

#### Plot Controls #####
printfigs_singletrace<-T#T#make png figures 

# mylinetypes <- c("dashed","solid","solid")
#standard powerpoint figure sizes 
# first is for monthly plots, second is for daily plots 
widths <- 11 #smaller looks really bad, better to just resize larger image
heights <- 6

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                               END USER INPUT
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ofigs <- file.path(results_dir,mainScenGroup) 
ofigs <- file.path(results_dir,Figs) 

if (!file.exists(ofigs)) {
  message(paste('Creating folder:', ofigs))
  dir.create(ofigs)
}

message('Figures will be saved to: ', ofigs)

# # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ## Load Feather with Processed Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++EITHER+++++++++++++++++++++++++++++++++++++++
### already processed feather file and saved results 
scen_res_DNF<-arrow::read_feather(file.path(CRSSDIR,"scen_res_DNF.feather")) 
scen_res_ST<-arrow::read_feather(file.path(CRSSDIR,"scen_res_ST.feather")) 

# ++++++++++++++++++++++++++OR+++++++++++++++++++++++++++++++++++++++++++++++++
### process CRSS crsp_ops_data.feather
if (F) {
  
#chunk up the data for easier working 
scen_res_DNF <- zz %>%
  filter(Scenario %in% c(previous_scens_DNF,scens_latest_DNF,scens_noUBDRO_DNF)) 

scen_res_DNF <- scen_res_DNF %>%
  mutate(ScenarioGroup = case_when(
    Scenario %in% previous_scens_DNF ~ "April 2021 - DNF IG",
    Scenario %in% scens_latest_DNF ~ "June 2021 - DNF IG",
    Scenario %in% scens_noUBDRO_DNF ~ "June 2021 - DNF IG noUBDRO",
    TRUE ~ "BAD"
  ))

unique(scen_res_DNF$ScenarioGroup) #[1] "April 2021 - DNF IG"        "June 2021 - DNF IG"         "June 2021 - DNF IG noUBDRO"
unique(scen_res_DNF$Variable)
length(unique(scen_res_DNF$Scenario)) #105 is correct
unique(scen_res_DNF$Scenario)

scen_res_ST <- zz %>%
  filter(Scenario %in% c(previous_scens_ST,scens_latest_ST,scens_noUBDRO_ST)) 

scen_res_ST <- scen_res_ST %>%
  mutate(ScenarioGroup = case_when(
    Scenario %in% previous_scens_ST ~ "April 2021 - ST IG",
    Scenario %in% scens_latest_ST ~ "June 2021 - ST IG",
    Scenario %in% scens_noUBDRO_ST ~ "June 2021 - ST IG noUBDRO",
    TRUE ~ "BAD"
  ))

unique(scen_res_ST$ScenarioGroup)
unique(scen_res_ST$Variable)
length(unique(scen_res_ST$Scenario))
unique(scen_res_ST$Scenario)

rm(zz)
gc() #call of gc() causes a garbage collection to take place. It can be useful to call gc() after a large object has been removed, as this may prompt R to return memory to the operating system. gc() also return a summary of the occupy memory.

unique(scen_res_ST$ScenarioGroup)
unique(scen_res_ST$Variable)
unique(scen_res_ST$CRMMSTraceNumber)


#chunk up the data for easier working 
scens = unique(scen_res_DNF$ScenarioGroup) 
scens = scens[2:3]
scens
scen_res_DNF <- scen_res_DNF %>%
  filter(Year %in% startyr:endyr) %>%
  filter(ScenarioGroup %in% scens) 
unique(scen_res_DNF$ScenarioGroup) 
unique(scen_res_DNF$Year) 
head(scen_res_DNF)

scens = unique(scen_res_ST$ScenarioGroup) 
scens = scens[2:3]
scens
scen_res_ST <- scen_res_ST %>%
  filter(Year %in% startyr:endyr) %>%
  filter(ScenarioGroup %in% scens) 
unique(scen_res_ST$ScenarioGroup) 
unique(scen_res_ST$Year) 
head(scen_res_ST)

#add a CRMMSTraceNumber
scen_res_ST <- cbind.data.frame(scen_res_ST, 
                 CRMMSTraceNumber = stringr::str_remove(stringr::str_split_fixed(scen_res_ST$Scenario, ",", 5)[,5],"Trace"))
#that's a pretty expensive calc if you have a big df so suggest not doing it more than once if you don't have to. 
# or you could do something like select_scens <- paste0("Apr2021_2022.v5.1,DNF,2016Dems,", c("IG_DCP.v5.1", "IG_DCPNoUBDRO"), "Trace", mtom_i) and then just filter to those two scenarios. 
unique(scen_res_ST$CRMMSTraceNumber)

scen_res_DNF <- cbind.data.frame(scen_res_DNF, 
                                CRMMSTraceNumber = stringr::str_remove(stringr::str_split_fixed(scen_res_DNF$Scenario, ",", 5)[,5],"Trace"))
unique(scen_res_DNF$CRMMSTraceNumber)


#get everything on a date and then a numeric month number
scen_res_DNF$Date = as.Date(paste0(scen_res_DNF$Year,scen_res_DNF$Month,"01"), format = "%Y%B%d") #this takes so long! 
scen_res_DNF$MonthNum = as.numeric(format.Date(scen_res_DNF$Date, format = "%m"))
head(scen_res_DNF)

scen_res_ST$Date = as.Date(paste0(scen_res_ST$Year,scen_res_ST$Month,"01"), format = "%Y%B%d") #this takes so long! 
scen_res_ST$MonthNum = as.numeric(format.Date(scen_res_ST$Date, format = "%m"))
head(scen_res_ST)

arrow::write_feather(scen_res_DNF,file.path(CRSSDIR,"scen_res_DNF.feather")) 
arrow::write_feather(scen_res_ST,file.path(CRSSDIR,"scen_res_ST.feather")) 

gc() #call of gc() causes a garbage collection to take place. It can be useful to call gc() after a large object has been removed, as this may prompt R to return memory to the operating system. gc() also return a summary of the occupy memory.

}
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5. Plot monthly figures  
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### now make plots but only for one hydrology 
Hydro <- "ST"
scen_res = scen_res_ST
scens = unique(scen_res$ScenarioGroup) 

Hydro <- "DNF"
scen_res = scen_res_DNF
scens = unique(scen_res$ScenarioGroup) 

CRMMStraces = unique(scen_res$CRMMSTraceNumber) 
CRMMStraces
CRSStraces = unique(scen_res$TraceNumber) 
CRSStraces

#### Run Powell Clouds 


#### Single Trace Plots 
# create a pdf
pdf(file.path(ofigs,paste0("SingleTrace_June2021_",Hydro,".pdf")), width = widths[1],height = heights[1]) #width= width, height= height)

### 4 Panel Plots ###
# slotNames_plot = c('FlamingGorge.Outflow', 'BlueMesa.Outflow','Navajo.Outflow',)
slotNames_plot = c('FlamingGorge.PE', 'BlueMesa.PE','Navajo.PE','Powell.PE')
CRSStrace <- 15 #ST 15 = 2002, worst case! #ST 14 = 2001, DNF 96 = 2001
CRMMStrace <- 24 # 24 = 2001
title = paste0("CRMMS trace ",CRMMStrace," (",CRMMStrace+1977,'), CRSS trace ',CRSStrace," (",CRSStrace+1987,")") 
title
# ytitle <- "EOCY Water Surface Elevation (ft)"
ytitle <- "Water Surface Elevation (ft)"

df_plot = scen_res %>% filter(Variable %in% slotNames_plot) %>%
  mutate(Variable = factor(Variable, levels = slotNames_plot)) %>%
  dplyr::filter(TraceNumber %in% CRSStrace) %>%
  dplyr::filter(CRMMSTraceNumber %in% CRMMStrace) # %>%
#if I wanted EOCY I'd do below 
  # dplyr::filter(MonthNum %in% exc_month) %>%
  # dplyr::group_by(Variable,ScenarioGroup, Year) %>%
  # dplyr::summarise(Value = mean(Value)) #inflow and outflow this needs to be sum(Value)
g <- df_plot  %>% 
  # ggplot(aes(Year, Value, fill = ScenarioGroup,color = ScenarioGroup)) + 
  ggplot(aes(Date, Value, fill = ScenarioGroup,color = ScenarioGroup)) + 
  # bor_style() +
  # geom_hline(yintercept=0, col="grey20") +
  geom_line() +
  # scale_y_continuous() +
  labs(y = ytitle, x = '',title = title) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 12), 
        legend.spacing.x  = unit(0.5, units = 'cm'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size=rel(1.2)),
        plot.margin=unit(c(1,1.5,3,1),"lines"),
        strip.text = element_text(size = 12)) +
  scale_y_continuous(label = comma) + # guides(fill = guide_legend(nrow=legend_rows,byrow=T)) +
  facet_wrap(~ Variable, scales = 'free_y',ncol = 1)
print(g)
if(printfigs_singletrace==T){ ggsave(filename = file.path(ofigs,paste0(Hydro,".CRSP.PE.",title,".png")), width = 11,height = 7)} #maxes out pptx slide height 

slotNames_plot = c('FlamingGorge.PE', 'BlueMesa.PE','Navajo.PE','Powell.PE')
CRSStrace <- 98#ST 14 = 2001, DNF 96 = 2001
CRMMStrace <- 26 # 25 = 2003
title = paste0("CRMMS trace ",CRMMStrace," (",CRMMStrace+1977,'), CRSS trace ',CRSStrace," (",CRSStrace+1987,")") 
title
ytitle <- "Water Surface Elevation (ft)"

df_plot = scen_res %>% filter(Variable %in% slotNames_plot) %>%
  mutate(Variable = factor(Variable, levels = slotNames_plot)) %>%
  dplyr::filter(TraceNumber %in% CRSStrace) %>%
  dplyr::filter(CRMMSTraceNumber %in% CRMMStrace) # %>%

g <- df_plot  %>% 
  ggplot(aes(Date, Value, fill = ScenarioGroup,color = ScenarioGroup)) + 
  # bor_style() +
  geom_line() +
  # scale_x_continuous(minor_breaks = 1990:3000, breaks = 1990:3000,
  #                    labels = 1990:3000, expand = c(0,0)) +  
  labs(y = ytitle, x = '',title = title) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 12), 
        legend.spacing.x  = unit(0.5, units = 'cm'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size=rel(1.2)),
        plot.margin=unit(c(1,1.5,3,1),"lines"),
        strip.text = element_text(size = 12)) +
  scale_y_continuous(label = comma) + # guides(fill = guide_legend(nrow=legend_rows,byrow=T)) +
  facet_wrap(~ Variable, scales = 'free_y',ncol = 1)
print(g)
if(printfigs_singletrace==T){ ggsave(filename = file.path(ofigs,paste0(Hydro,".CRSP.PE.",title,".png")), width = 11,height = 7)} #maxes out pptx slide height 

dev.off()
dev.off()



### 3 Storage + Powell PE Plots ###
# slotNames_plot = c('FlamingGorge.Outflow', 'BlueMesa.Outflow','Navajo.Outflow',)
slotNames_plot = c('FlamingGorge.Storage', 'BlueMesa.Storage','Navajo.Storage','Powell.PE')
CRSStrace <- 14 #ST 15 = 2002, worst case! #ST 14 = 2001, DNF 96 = 2001
CRMMStrace <- 23 # 24 = 2001
title = paste0("CRMMS trace ",CRMMStrace," (",CRMMStrace+1977,'), CRSS trace ',CRSStrace," (",CRSStrace+1987,")") 
ytitle <- "Storage (kaf)"

df_plot = scen_res %>% filter(Variable %in% slotNames_plot) %>%
  mutate(Variable = factor(Variable, levels = slotNames_plot)) %>%
  dplyr::filter(TraceNumber %in% CRSStrace) %>%
  dplyr::filter(CRMMSTraceNumber %in% CRMMStrace) # %>%
g <- df_plot  %>% 
  ggplot(aes(Date, Value, fill = ScenarioGroup,color = ScenarioGroup)) + 
  geom_line() +
  labs(y = ytitle, x = '',title = title) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 12), 
        legend.spacing.x  = unit(0.5, units = 'cm'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size=rel(1.2)),
        plot.margin=unit(c(1,1.5,3,1),"lines"),
        strip.text = element_text(size = 12)) +
  scale_y_continuous(label = comma) + # guides(fill = guide_legend(nrow=legend_rows,byrow=T)) +
  facet_wrap(~ Variable, scales = 'free_y',ncol = 1)
print(g)
if(printfigs_singletrace==T){ ggsave(filename = file.path(ofigs,paste0(Hydro,".CRSP.PE.",title,".png")), width = 11,height = 7)} #maxes out pptx slide height 

CRSStrace <- 15#ST 14 = 2001, DNF 96 = 2001
CRMMStrace <- 24 # 25 = 2003
title = paste0("CRMMS trace ",CRMMStrace," (",CRMMStrace+1977,'), CRSS trace ',CRSStrace," (",CRSStrace+1987,")") 

df_plot = scen_res %>% filter(Variable %in% slotNames_plot) %>%
  mutate(Variable = factor(Variable, levels = slotNames_plot)) %>%
  dplyr::filter(TraceNumber %in% CRSStrace) %>%
  dplyr::filter(CRMMSTraceNumber %in% CRMMStrace) # %>%

g <- df_plot  %>% 
  ggplot(aes(Date, Value, fill = ScenarioGroup,color = ScenarioGroup)) + 
  geom_line() +
  labs(y = ytitle, x = '',title = title) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 12), 
        legend.spacing.x  = unit(0.5, units = 'cm'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size=rel(1.2)),
        plot.margin=unit(c(1,1.5,3,1),"lines"),
        strip.text = element_text(size = 12)) +
  scale_y_continuous(label = comma) + # guides(fill = guide_legend(nrow=legend_rows,byrow=T)) +
  facet_wrap(~ Variable, scales = 'free_y',ncol = 1)
print(g)
if(printfigs_singletrace==T){ ggsave(filename = file.path(ofigs,paste0(Hydro,".CRSP.PE.",title,".png")), width = 11,height = 7)} #maxes out pptx slide height 

CRSStrace <- 26#ST 14 = 2001, DNF 96 = 2001
CRMMStrace <- 17 # 25 = 2003
title = paste0("CRMMS trace ",CRMMStrace," (",CRMMStrace+1977,'), CRSS trace ',CRSStrace," (",CRSStrace+1987,")") 

df_plot = scen_res %>% filter(Variable %in% slotNames_plot) %>%
  mutate(Variable = factor(Variable, levels = slotNames_plot)) %>%
  dplyr::filter(TraceNumber %in% CRSStrace) %>%
  dplyr::filter(CRMMSTraceNumber %in% CRMMStrace) # %>%

g <- df_plot  %>% 
  ggplot(aes(Date, Value, fill = ScenarioGroup,color = ScenarioGroup)) + 
  # bor_style() +
  geom_line() +
  labs(y = ytitle, x = '',title = title) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 12), 
        legend.spacing.x  = unit(0.5, units = 'cm'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size=rel(1.2)),
        plot.margin=unit(c(1,1.5,3,1),"lines"),
        strip.text = element_text(size = 12)) +
  scale_y_continuous(label = comma) + # guides(fill = guide_legend(nrow=legend_rows,byrow=T)) +
  facet_wrap(~ Variable, scales = 'free_y',ncol = 1)
print(g)
if(printfigs_singletrace==T){ ggsave(filename = file.path(ofigs,paste0(Hydro,".CRSP.PE.",title,".png")), width = 11,height = 7)} #maxes out pptx slide height 

dev.off()
dev.off()




# ### Simple year average
# 
# variable = "FlamingGorge.PE"
# title = paste(variable,first(yrs2show),"-",last(yrs2show))
# y_lab = "EOCY Water Surface Elevation (ft)"
# exc_month = 12
# p <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(MonthNum%in%exc_month) %>%
#   dplyr::group_by(ScenarioGroup, Year) %>%
#   dplyr::summarise(Value = mean(Value)) %>%
#   ggplot(aes(x = factor(Year), y = Value, color = ScenarioGroup, group = ScenarioGroup)) +
#   geom_line() +
#   theme_light() + 
#   # scale_color_manual(values = mycolors) +
#   labs(title = paste("Average EOCY",title), y = y_lab, x = "Year")
# print(p)
# if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Mean EOCY",variable,".png")), width = widths[1],height = heights[1])}
# 
# variable = "Powell.PE"
# title = paste(variable,first(yrs2show),"-",last(yrs2show))
# y_lab = "EOCY Water Surface Elevation (ft)"
# exc_month = 12
# p <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(MonthNum%in%exc_month) %>%
#   dplyr::group_by(ScenarioGroup, Year) %>%
#   dplyr::summarise(Value = mean(Value)) %>%
#   ggplot(aes(x = factor(Year), y = Value, color = ScenarioGroup, group = ScenarioGroup)) +
#   geom_line() +
#   theme_light() + 
#   # scale_color_manual(values = mycolors) +
#   labs(title = paste("Average EOCY",title), y = y_lab, x = "Year")
# print(p)
# if(printfigs_monthly==T){ ggsave(filename = file.path(ofigs,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}
# 

