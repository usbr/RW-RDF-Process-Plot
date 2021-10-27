# from crspopsdata.feather plot Powell & 1 generic UBres In/PE/Out and write stats 
# CF Oct 2021 
# # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
warning('Run Process_CRSS_rdf_generic_feather.R before this')
# ## Load Feather with Processed Results 

scen_res <- feather::read_feather(path = file.path(feather_data_dir,feather_file_nm)) 
summary(scen_res)
scens <- unique(scen_res$ScenarioGroup)
scens
unique(scen_res$Variable)
length(unique(scen_res$Scenario))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#pick which res to graph along with Powell in/PE/out
res <- "Navajo" #BlueMesa #FlamingGorge #Fontenelle #TaylorPark
# res <- F #if F only plot Powell 

startyr = 2022 #filter out all years > this year
endyr = 2040 #2060

print_png <- F #F = don't make seperate png figures 
 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                               END USER INPUT
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!file.exists(results_nm)) {
  message(paste('Ploting',results_nm,'for Powell &',res))
} else {
  stop('Run Process_CRSS_rdf_generic_feather.R first')
}

# check folders
if (!file.exists(results_dir)) {
  message(paste('Creating folder:', results_dir))
  dir.create(results_dir)
}
message('PDF will be saved to: ', results_dir)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Plot annual figures  
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
yrs2show <- startyr:endyr 
scen_res <- scen_res %>%
  dplyr::filter(Year %in% yrs2show) 

scen_res_stats <- scen_res %>%
  dplyr::group_by(ScenarioGroup, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'q10' = quantile(Value,.1),'q90' = quantile(Value,.9),
                   'Min' = min(Value),'Max' = max(Value)) #add in outliers for plot 

#get everything on a date 
scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
#get a numeric month number
scen_res$MonthNum = as.numeric(format.Date(scen_res$MonthNum, format = "%m"))

if (res != F) {
pdf(file.path(results_dir,paste0(results_nm,"_AnnualPowell+",res,'.pdf')), width=9, height=6)


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
  # scale_color_manual(values = mycolors) +
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
  # scale_color_manual(values = mycolors) +
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
  # scale_color_manual(values = mycolors) +
  labs(title = paste("Average Annual",title), y = y_lab, x = "Year")
print(p)
if(print_png==T){ ggsave(filename = file.path(figures_dir,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}

scen_res_stats %>%
  dplyr::filter(Variable %in% paste0("Powell",c(".Inflow",".PE",".Outflow"))) %>%
  pivot_wider(names_from = ScenarioGroup,values_from=c("Mean","Med","Min","q10","q90","Max")) %>% 
  arrange(Variable,Year) %>%
write.csv(file = file.path(results_dir,"figure_data",paste("Powell_Stats.csv")))

} # end UB res plotting loop  

if (res == F) { #if UB res was not plotted than open a pdf just for Powell 
  pdf(file.path(results_dir,paste0(results_nm,"_AnnualPowell.pdf")), width=9, height=6)
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
  # scale_color_manual(values = mycolors) +
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
  # scale_color_manual(values = mycolors) +
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
  # scale_color_manual(values = mycolors) +
  labs(title = paste("Average Annual",title), y = y_lab, x = "Year")
print(p)
if(print_png==T){ ggsave(filename = file.path(figures_dir,paste("Average Annual",variable,".png")), width = widths[1],height = heights[1])}

#print out res stats 
scen_res_stats %>%
  dplyr::filter(Variable %in% paste0(res,c(".Inflow",".PE",".Outflow"))) %>%
  pivot_wider(names_from = ScenarioGroup,values_from=c("Mean","Med","Min","q10","q90","Max")) %>% 
  arrange(Variable,Year) %>%
  write.csv(file = file.path(results_dir,"figure_data",paste0(res,"_Stats.csv")))
message(paste('Writing stats file to',file.path(results_dir,"figure_data",paste("res_Stats.csv"))))

dev.off()
}
