#don't need this if just ran 2_Plot_CRSS_UBRes
if(F){ #if you've already processed just Load Feather with Processed Results 
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
  
  startyr = 2022 #filter out all years > this year
  endyr = 2026#2060
  yrs2show <- startyr:endyr 
  scen_res <- scen_res %>%
    dplyr::filter(Year %in% yrs2show) 
  
  #get everything on a date 
  scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
  #get a numeric month number
  scen_res$MonthNum = as.numeric(format.Date(scen_res$MonthNum, format = "%m"))
  
}

if(F){
  # ### rename scen groups 
  # scen1 <- scens[1]
  # scen2 <- scens[2]
  # new_scen_nms <- c("Powell El-Vol","Baseline") 
  # scen1_new_nm <- new_scen_nms[1]
  # scen2_new_nm <- new_scen_nms[2]
  # scen_res <- scen_res %>%
  #   mutate(ScenarioGroup = case_when(
  #     ScenarioGroup %in% scen1 ~ scen1_new_nm,
  #     ScenarioGroup %in% scen2 ~ scen2_new_nm, 
  #     TRUE ~ "BAD"))
  # unique(scen_res$ScenarioGroup)
}


#Powell In/Out/PE CDFs 
if(T){
  variable = "Powell.Inflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "CY Inflow (KAF/yr)"
  
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::group_by(ScenarioGroup,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    ggplot(aes(x = Value, color = ScenarioGroup, group = ScenarioGroup)) +
    stat_ecdf(geom = "step") +
    crssplot::theme_crss() +
    # ylim(c(3400,3700))+
    scale_color_manual(values = mycolors) +
    labs(title = paste(title), y = "", x = y_lab)
  print(p)
  ggsave(filename = file.path(figures_dir,paste("CDF_",title,".png")), width = widths[1],height = heights[1])
  
  variable = "Powell.Outflow"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "CY Release (KAF/yr)"
  p <- 
    scen_res %>%
    dplyr::filter(Variable == variable) %>%
    mutate(Value = Value/1000) %>% #convert to KAF after we convert to AF  
    dplyr::group_by(ScenarioGroup,TraceNumber,Year) %>%
    summarise(Value = sum(Value)) %>% #first sum by year, keeping scens, traces, and years together
    # write.csv(file = file.path(results_dir,"figure_data",paste("Powell.Outflow.Annual.csv")))
    ggplot(aes(x = Value, color = ScenarioGroup, group = ScenarioGroup)) +
    stat_ecdf(geom = "step") +
    crssplot::theme_crss() +
    # ylim(c(3400,3700))+
    scale_color_manual(values = mycolors) +
    labs(title = paste(title), y = "", x = y_lab)
  print(p)
  ggsave(filename = file.path(figures_dir,paste("CDF_",title,".png")), width = widths[1],height = heights[1])
  
  variable = "Powell.PE"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "EOCY Water Surface Elevation (ft)"
  exc_month = 12
  p <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(MonthNum%in%exc_month) %>%
    # dplyr::group_by(ScenarioGroup, Year) %>%
    # dplyr::summarise(Value = mean(Value)) %>%
    ggplot(aes(x = Value, color = ScenarioGroup, group = ScenarioGroup)) +
    stat_ecdf(geom = "step") +
    crssplot::theme_crss() +
    # ylim(c(3400,3700))+
    scale_color_manual(values = mycolors) +
    labs(title = paste(title), y = "", x = y_lab)
  print(p)
  ggsave(filename = file.path(figures_dir,paste("CDF_",title,".png")), width = widths[1],height = heights[1])
  
}

#process and plot GainsAboveLeesFerry.feather
if(F){
  df <- feather::read_feather(path = file.path(feather_data_dir,'GainsAboveLeesFerry.feather')) 
  unique(df$Variable)
  scens <- unique(df$ScenarioGroup)
  scens
  
  df <- df %>%
    mutate(ScenarioGroup = case_when(
      ScenarioGroup %in% scens[1] ~ scen1_shrt_nm,
      ScenarioGroup %in% scens[2] ~ scen2_shrt_nm, 
      TRUE ~ "BAD"))
  unique(df$ScenarioGroup)
  
  length(unique(df$Scenario))
  df <- df %>%
    dplyr::filter(Year %in% yrs2show) 
  #make ggplot keep this order rather than alpha
  df$ScenarioGroup <- factor(df$ScenarioGroup, levels=scens)
  
  variable = "GainsAboveLeesFerry"
  title = paste(variable,first(yrs2show),"-",last(yrs2show))
  y_lab = "CY Local Inflow (KAF/yr)"
  p <- df %>%
    # dplyr::filter(Variable == variable) %>%
    dplyr::group_by(ScenarioGroup, Year) %>%
    # dplyr::summarise(Value = sum(Value)) %>% #### this was already done with rw_agg
    dplyr::summarise(Value = Value/1000)  %>% #KAF
    ggplot(aes(x = Value, color = ScenarioGroup, group = ScenarioGroup)) +
    stat_ecdf(geom = "step") +
    crssplot::theme_crss() +
    # ylim(c(3400,3700))+
    scale_color_manual(values = mycolors) +
    labs(title = paste(title), y = "", x = y_lab)
  print(p)
  ggsave(filename = file.path(figures_dir,paste("CDF_",title,".png")), width = widths[1],height = heights[1])
  
}
