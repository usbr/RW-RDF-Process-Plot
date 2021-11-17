# from crspopsdata.feather plot Powell & 1 generic UBres In/PE/Out and write stats 
# CF Oct 2021 
# # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
warning('Run Process_CRSS_rdf_generic_feather.R before this')

if(T){ #if you've already processed just Load Feather with Processed Results 
  scen_dir_overwrite=FALSE # don't need this for already processed, just give F so doesn't error on libs_n_dirs 
  #get scen information from .yml file
  yaml_nm=FALSE #
  results_nm <- "PowellElVol_NewInactCap"# "NoChangeNF_PowellElVol" #"NewInactCap_NewNF_PowEV" 
  
  #libraries and setup directories, just use getwd()
  source(file.path(getwd(),"code","libs_n_dirs.R")) 
  
  scen_res <- feather::read_feather(path = file.path(feather_data_dir,'crspopsdata.feather')) 
  summary(scen_res)
  scens <- unique(scen_res$ScenarioGroup)
  scens
  #make ggplot keep this order rather than alpha
  scen_res$ScenarioGroup <- factor(scen_res$ScenarioGroup, levels=scens) 
  unique(scen_res$Variable)
  length(unique(scen_res$Scenario))
}

# results_nm<-unique(scen_res$ScenarioGroup)[1]

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#pick which res to graph along with Powell in/PE/out
res <- "FlamingGorge" #BlueMesa #FlamingGorge #Fontenelle #TaylorPark
res <- F #if F only plot Powell

startyr = 2022 #filter out all years > this year
endyr = 2040#2060

print_png <- T #F = don't make seperate png figures 
widths=9; heights=6
mycolors <- c("#138d75","#f1c40f") #crssplot 138d75=green=dev, f1c40f=gold=baseline

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                               END USER INPUT
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!file.exists(results_nm)) {
  message(paste('Ploting',results_nm,'for Powell &',res))
} else {
  stop('Run Process_CRSS_rdf_generic_feather.R first')
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Plot annual figures  
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
yrs2show <- startyr:endyr 
scen_res <- scen_res %>%
  dplyr::filter(Year %in% yrs2show) 

#get everything on a date 
scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
#get a numeric month number
scen_res$MonthNum = as.numeric(format.Date(scen_res$MonthNum, format = "%m"))

if (res != F) {
pdf(file.path(results_dir,paste0(results_nm,"_AnnualPowell+",res,"_",first(yrs2show),"-",last(yrs2show),'.pdf')), width=9, height=6)


variable = paste0(res,".Inflow")
title = paste(variable,first(yrs2show),"-",last(yrs2show))
y_lab = "Annual Flow (1,000 ac-ft/yr)"

p <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
  dplyr::group_by(ScenarioGroup,TraceNumber,Year) %>%
  summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
  dplyr::group_by(ScenarioGroup,Year)  %>%
  summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
  ggplot(aes(x = Year, y = Value, color = ScenarioGroup)) +
  # scale_x_continuous(breaks = 2021:2040) +
  geom_line() +
  theme_light() + 
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = mycolors) +
  labs(title = paste("Average Annual",title), y = y_lab, x = "Year")
print(p)
if(print_png==T){ ggsave(filename = file.path(figures_dir,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}

variable = paste0(res,".Storage")
title = paste(variable,first(yrs2show),"-",last(yrs2show))
y_lab = "EOCY Storage (1,000 ac-ft)"
exc_month = 12
p <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(MonthNum%in%exc_month) %>%
  dplyr::group_by(ScenarioGroup, Year) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
  ggplot(aes(x = Year, y = Value, color = ScenarioGroup))+#, group = ScenarioGroup)) +
  geom_line() +
  theme_light() + 
  scale_color_manual(values = mycolors) +
  labs(title = paste("Average EOCY",title), y = y_lab, x = "Year")
print(p)
if(print_png==T){ ggsave(filename = file.path(figures_dir,paste("Mean EOCY",variable,".png")), width = widths[1],height = heights[1])}

variable = paste0(res,".Outflow")
title = paste(variable,first(yrs2show),"-",last(yrs2show))
y_lab = "Annual Flow (1,000 ac-ft/yr)"

p <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
  dplyr::group_by(ScenarioGroup,TraceNumber,Year) %>%
  summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
  dplyr::group_by(ScenarioGroup,Year)  %>%
  summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
  ggplot(aes(x = Year, y = Value, color = ScenarioGroup)) +
  # scale_x_continuous(breaks = 2021:2040) +
  geom_line() +
  theme_light() + 
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = mycolors) +
  labs(title = paste("Average Annual",title), y = y_lab, x = "Year")
print(p)
if(print_png==T){ ggsave(filename = file.path(figures_dir,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}


variable = paste0(res,".PE")
title = paste(variable,first(yrs2show),"-",last(yrs2show))
y_lab = "EOCY Water Surface Elevation (ft)"
exc_month = 12
p <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(MonthNum%in%exc_month) %>%
  
  dplyr::group_by(ScenarioGroup, Year) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  ggplot(aes(x = Year, y = Value, color = ScenarioGroup))+#, group = ScenarioGroup)) +
  geom_line() +
  theme_light() + 
  scale_color_manual(values = mycolors) +
  labs(title = paste("Average EOCY",title), y = y_lab, x = "Year")
print(p)
if(print_png==T){ ggsave(filename = file.path(figures_dir,paste("Mean EOCY",variable,".png")), width = widths[1],height = heights[1])}


#print out res stats 
scen_res_stats_inout <- scen_res %>%
  dplyr::filter(Variable %in% paste0(res,c(".Inflow",".Outflow"))) %>%
  dplyr::group_by(ScenarioGroup, Year,Variable,TraceNumber) %>% #by leaving Variable in I keep the name in the resulting df
  summarise(Value = sum(Value)) #first sum by year, keeping scens, traces, and years together
scen_res_stats_eocy <- scen_res %>%
  dplyr::filter(Variable %in% paste0(res,c(".Storage",".PE"))) %>%
  dplyr::filter(MonthNum%in%12) #%>%
scen_res_stats <- rbind.data.frame(scen_res_stats_eocy[,names(scen_res_stats_inout)],scen_res_stats_inout)
scen_res_stats %>%
  dplyr::group_by(ScenarioGroup, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value), #summarize over the traces
                   'q10' = quantile(Value,.1),'q90' = quantile(Value,.9),
                   'Min' = min(Value),'Max' = max(Value)) %>% 
  pivot_wider(names_from = ScenarioGroup,values_from=c("Mean","Med","Min","q10","q90","Max")) %>% 
  arrange(Variable,Year) %>%
  write.csv(file = file.path(results_dir,"figure_data",paste(res,"_Stats.csv")))

} # end UB res plotting loop  

if (res == F) { #if UB res was not plotted than open a pdf just for Powell 
  pdf(file.path(results_dir,paste0(results_nm,"_AnnualPowell_",first(yrs2show),"-",last(yrs2show),'.pdf')), width=9, height=6)
}  

if (T){ #always plot Powell
variable = "Powell.Inflow"
title = paste(variable,first(yrs2show),"-",last(yrs2show))
y_lab = "Annual Flow (1,000 ac-ft/yr)"

p <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
  dplyr::group_by(ScenarioGroup,TraceNumber,Year) %>%
  summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
  dplyr::group_by(ScenarioGroup,Year)  %>%
  summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
  ggplot(aes(x = Year, y = Value, color = ScenarioGroup)) +
  # scale_x_continuous(breaks = 2021:2040) +
  geom_line() +
  theme_light() + 
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = mycolors) +
  labs(title = paste("Average Annual",title), y = y_lab, x = "Year")
print(p)
if(print_png==T){ ggsave(filename = file.path(figures_dir,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}

variable = "Powell.Storage"
title = paste(variable,first(yrs2show),"-",last(yrs2show))
y_lab = "EOCY Storage (1,000 ac-ft)"
exc_month = 12
p <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(MonthNum%in%exc_month) %>%
  dplyr::group_by(ScenarioGroup, Year) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
  ggplot(aes(x = factor(Year), y = Value, color = ScenarioGroup, group = ScenarioGroup)) +
  geom_line() +
  theme_light() + 
  scale_color_manual(values = mycolors) +
  labs(title = paste("Average EOCY",title), y = y_lab, x = "Year")
print(p)
if(print_png==T){ ggsave(filename = file.path(figures_dir,paste("Mean EOCY",variable,".png")), width = widths[1],height = heights[1])}

variable = "Powell.Outflow"
title = paste(variable,first(yrs2show),"-",last(yrs2show))
y_lab = "Annual Flow (1,000 ac-ft/yr)"

p <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
  dplyr::group_by(ScenarioGroup,TraceNumber,Year) %>%
  summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
  dplyr::group_by(ScenarioGroup,Year)  %>%
  summarise(Value = mean(Value)) %>% #then avg all traces, keeping scens and years together 
  ggplot(aes(x = Year, y = Value, color = ScenarioGroup)) +
  # scale_x_continuous(breaks = 2021:2040) +
  geom_line() +
  theme_light() + 
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = mycolors) +
  labs(title = paste("Average Annual",title), y = y_lab, x = "Year")
print(p)
if(print_png==T){ ggsave(filename = file.path(figures_dir,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}

variable = "Powell.PE"
title = paste(variable,first(yrs2show),"-",last(yrs2show))
y_lab = "EOCY Water Surface Elevation (ft)"
exc_month = 12
p <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(MonthNum%in%exc_month) %>%
  dplyr::group_by(ScenarioGroup, Year) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  ggplot(aes(x = factor(Year), y = Value, color = ScenarioGroup, group = ScenarioGroup)) +
  geom_line() +
  theme_light() + 
  ylim(c(3400,3700))+
  scale_color_manual(values = mycolors) +
  labs(title = paste("Average EOCY",title), y = y_lab, x = "Year")
print(p)
if(print_png==T){ ggsave(filename = file.path(figures_dir,paste("Mean EOCY",variable,".png")), width = widths[1],height = heights[1])}

p <- scen_res %>%
  dplyr::filter(Year%in%2022:2026) %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(MonthNum%in%exc_month) %>%
  dplyr::group_by(ScenarioGroup, Year) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  ggplot(aes(x = factor(Year), y = Value, color = ScenarioGroup, group = ScenarioGroup)) +
  geom_line() +
  theme_light() + 
  ylim(c(3400,3700))+
  scale_color_manual(values = mycolors) +
  labs(title = paste("Average EOCY",title), y = y_lab, x = "Year")
print(p)

#print out res stats 
scen_res_stats_inout <- scen_res %>%
  dplyr::filter(Variable %in% paste0("Powell",c(".Inflow",".Outflow"))) %>%
  dplyr::group_by(ScenarioGroup, Year,Variable,TraceNumber) %>% #by leaving Variable in I keep the name in the resulting df
  summarise(Value = sum(Value)) #first sum by year, keeping scens, traces, and years together
scen_res_stats_eocy <- scen_res %>%
  dplyr::filter(Variable %in% paste0("Powell",c(".Storage",".PE"))) %>%
  dplyr::filter(MonthNum%in%12) #%>%
scen_res_stats <- rbind.data.frame(scen_res_stats_eocy[,names(scen_res_stats_inout)],scen_res_stats_inout)
scen_res_stats %>%
  dplyr::group_by(ScenarioGroup, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value), #summarize over the traces
                   'q10' = quantile(Value,.1),'q90' = quantile(Value,.9),
                   'Min' = min(Value),'Max' = max(Value)) %>% 
  pivot_wider(names_from = ScenarioGroup,values_from=c("Mean","Med","Min","q10","q90","Max")) %>% 
  arrange(Variable,Year) %>%
  write.csv(file = file.path(results_dir,"figure_data",paste0("Powell_",first(yrs2show),"-",last(yrs2show),'_Stats.csv')))
message(paste('Writing stats file to',file.path(results_dir,"figure_data",paste("res_Stats.csv"))))

dev.off()
}


