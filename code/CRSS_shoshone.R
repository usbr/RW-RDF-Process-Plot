library(RWDataPlyr)
library(tidyverse)
library(feather)

#Conor PC
# rwd <- read_rwd_agg("C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/rw_agg/rw_agg_CRSPPowerData_Energy.csv") 
#BA
rwd <- read_rwd_agg("C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/rw_agg/rw_agg_shoshone.csv") 
CRSSDIR <- Sys.getenv("CRSS_DIR")

## latest offical run #
scens_latest <- scens_latest_ST <- "Jun9002,ISM1988_2019,2016Dems,IG_DCP,CRMMS_Most"

## previous offical run #
scens_previous <- scens_previous_ST <- "Jun2021_2022,ISM1988_2019,2016Dems,IG_DCP,CRMMS_Most"

# # aggregate the combined scens list 
# # scens on Manoa 
scen_dir <- "M:/Shared/CRSS/2021/Scenario"
# # scens elsewhere
scen_dir <- "M:/Shared/CRSS/2021/Scenario_dev/AugDev"

file_nm <- "Shoshone.feather" #"CRSPPowerData.feather"

rw_scen_aggregate(c(scens_latest,scens_previous), agg = rwd, scen_dir = scen_dir,
                  file = file_nm)
# # # scens on D: drive of BA
# scen_dir <- "D:/2021/Scenario"
# list.dirs(path=scen_dir)
# rw_scen_aggregate(scens_latest, agg = rwd, 
#                   scen_dir = scen_dir,
#                   file = "CRSPPowerData.feather")
#ends up in the RW-Data.../code/
# zz <- feather::read_feather(file.path(CRSSDIR,"CRSPPowerData.feather")) 
zz <- feather::read_feather(file.path("C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/",file_nm)) 

nm_latest <- "Jun 2021 dev - ST Most"
nm_previous <- "Jun 2021 - ST Most"

zz <- zz %>%
  mutate(Scenario = case_when(
    Scenario %in% scens_previous_ST ~ nm_latest, 
    Scenario %in% scens_latest_ST ~ nm_previous, 
    TRUE ~ "BAD"
  ))

summary(zz)
unique(zz$Scenario)
unique(zz$Variable)

feather::write_feather(zz,file.path(CRSSDIR,"CRSPPowerData.feather")) 

zz %>% group_by(ScenarioGroup,Variable,TraceNumber) %>%
  summarise(Value=sum(Value)) %>%  #summmary by month 
  group_by(ScenarioGroup,Variable) %>%
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 
write.csv(df_stats,file.path(figures_dir,"Stats",paste0("AddDRORelease_stats_2226bxplt_",results_nm,".csv")))


#### =============== Plotting =============== ####

results_nm <- "ShoshoneMove" #results dir folder 

onBA <- TRUE # which computer BA or my PC? find RW-RDF-Process-Plot dir 
if (onBA == TRUE) {
  rwprocess_dir <- "C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot"
} else {
  rwprocess_dir <- "C:/Users/cfelletter/Documents/RW-RDF-Process-Plot"
}

#libraries and setup directories 
source(file.path(rwprocess_dir,"code","libs_n_dirs.R")) 
# scen_dir = #OVERWRITE if not manoa/Shared/CRSS/2021/Scenario 

ww <- 10
hh <- 7
start_yr <- 2022
# end_yr <- 2030 

# scengroups
unique(zz$Scenario)

custom_colors <- c("Jun 2021 - ST Most" = "#fc8d62", #st compare colors
                   "Jun 2021 dev - ST Most" = "#8da0cb")


#### =============== INPUTS =============== ####


unique(zz$Variable)
unique(zz$Variable)

slotnames <- c("Powell.Inflow","Powell.PE","Powell.Outflow","GlenwoodSprings.Outflow","Cameo.Outflow" ) 


scengroups <- unique(zz$Scenario)


# plotting data ---------------------------


# plot individual -------------------------

pdf(file.path(figures_dir,paste0(results_nm,'.pdf')),width = ww, height = hh)
# 
# for(j in 1:length(slotnames)){
#   scens_plot_cloud(zz,vars=slotnames[j],title=slotnames[j])
# }


scens_plot_cloud(zz,vars="GlenwoodSprings.Outflow",title="Glenwood Springs Gage Flow",plot_colors = custom_colors,y_lab="acre-ft/yr")
scens_plot_cloud(zz,vars="Cameo.Outflow",title="Cameo Gage Flow",plot_colors = custom_colors,y_lab="acre-ft/yr")
scens_plot_cloud(zz,vars="Powell.Inflow",title="Powell Inflow",plot_colors = custom_colors,y_lab="acre-ft/yr")


dev.off()

