library(RWDataPlyr)
library(tidyverse)
library(feather)

#Conor PC
# rwd <- read_rwd_agg("C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/rw_agg/rw_agg_CRSPPowerData_Energy.csv") 
#BA
rwd <- read_rwd_agg("C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/rw_agg/rw_agg_shoshone.csv") 

file_nm <- "Shoshone.feather" #"CRSPPowerData.feather"

rw_scen_aggregate(c(scen1,scen2), agg = rwd, scen_dir = scen_dir,
                  file = file_nm)
# # # scens on D: drive of BA
# scen_dir <- "D:/2021/Scenario"
# list.dirs(path=scen_dir)
# rw_scen_aggregate(scens_latest, agg = rwd, 
#                   scen_dir = scen_dir,
#                   file = "CRSPPowerData.feather")
#ends up in the RW-Data.../code/
# scen_res <- feather::read_feather(file.path(CRSSDIR,"CRSPPowerData.feather")) 
scen_res <- feather::read_feather(file.path("C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/",file_nm)) 

#add scenario group for plotting
scen_res <- scen_res %>%
  mutate(ScenarioGroup = case_when(
    Scenario %in% scen1 ~ scen1_shrt_nm,
    Scenario %in% scen2 ~ scen2_shrt_nm, 
    TRUE ~ "BAD"))

unique(scen_res$ScenarioGroup)
# scen_res <- scen_res %>% 
#   dplyr::filter(ScenarioGroup != "BAD")
# unique(scen_res$ScenarioGroup)
unique(scen_res$Variable)

feather::write_feather(scen_res,file.path(feather_data_dir,file_nm)) 

#print out res stats 
scen_res_stats_inout <- scen_res %>%
  dplyr::filter(Variable %in% c("Powell.Inflow","Powell.Outflow","GlenwoodSprings.Outflow","Cameo.Outflow")) %>%
  dplyr::group_by(ScenarioGroup, Year,Variable,TraceNumber) %>% #by leaving Variable in I keep the name in the resulting df
  summarise(Value = sum(Value)) #first sum by year, keeping scens, traces, and years together
scen_res_stats <- scen_res_stats_inout
scen_res_stats %>%
  dplyr::group_by(ScenarioGroup, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value), #summarize over the traces
                   'q10' = quantile(Value,.1),'q90' = quantile(Value,.9),
                   'Min' = min(Value),'Max' = max(Value)) %>% 
  pivot_wider(names_from = ScenarioGroup,values_from=c("Mean","Med","Min","q10","q90","Max")) %>% 
  arrange(Variable,Year) %>%
  write.csv(file = file.path(results_dir,"figure_data",paste("Glenwood","_Stats.csv")))


#### =============== Plotting =============== ####


custom_colors <- c("Dev" = "#fc8d62", #st compare colors
                   "Baseline" = "#8da0cb")
names(custom_colors) <- c(scen1_shrt_nm,scen2_shrt_nm)

# mycolors <- c("#138d75","#f1c40f") #crssplot 138d75=green=dev, f1c40f=gold=baseline
# names(mycolors) <- c(scen1_shrt_nm,scen2_shrt_nm)

#### =============== INPUTS =============== ####


unique(scen_res$Variable)

slotnames <- c("Powell.Inflow","Powell.PE","Powell.Outflow","GlenwoodSprings.Outflow","Cameo.Outflow" ) 


scengroups <- unique(scen_res$Scenario)


# plotting data ---------------------------

unique(scen_res$Scenario)
#must have scenario names changed to match short scen1_nms 
scen_res <- scen_res %>%
  mutate(Scenario = case_when(
    Scenario %in% scen1 ~ scen1_shrt_nm,
    Scenario %in% scen2 ~ scen2_shrt_nm, 
    TRUE ~ "BAD"))
unique(scen_res$Scenario)

# plot individual -------------------------

pdf(file.path(results_dir,paste0(results_nm,'_Glen_Cameo_PowellIn_Clouds.pdf')),width = ww, height = hh)
# 
# for(j in 1:length(slotnames)){
#   scens_plot_cloud(scen_res,vars=slotnames[j],title=slotnames[j])
# }





scens_plot_cloud(scen_res,vars="GlenwoodSprings.Outflow",title="Glenwood Springs Gage Flow",plot_colors = custom_colors,y_lab="acre-ft/yr")
scens_plot_cloud(scen_res,vars="Cameo.Outflow",title="Cameo Gage Flow",plot_colors = custom_colors,y_lab="acre-ft/yr")
scens_plot_cloud(scen_res,vars="Powell.Inflow",title="Powell Inflow",plot_colors = custom_colors,y_lab="acre-ft/yr")


dev.off()

