# from MeadPowellPE.feather plot Powell & Mead clouds 
# CF Jan 2022
####### NOT TESTED #########
# # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
warning('Run CRSS R process code before this')


results_nm <- "Shoshone_9003"# 
Hydro <- "ST"


scen_dir_overwrite=FALSE # don't need this for already processed, just give F so doesn't error on libs_n_dirs
yaml_nm=FALSE # don't get scen information from .yml file
#libraries and setup directories, just use getwd()
source(file.path(getwd(),"code","libs_n_dirs.R"))
ofigs <- figures_dir #need this for Cloud_plot_woHistv2.R

if(T){ #if you've already processed just Load Feather with Processed Results
  scen_res <- feather::read_feather(path = file.path(feather_data_dir,'MeadPowellPE.feather')) 
  summary(scen_res)
  scens <- unique(scen_res$ScenarioGroup)
  scens
  #make ggplot keep this order rather than alpha
  scen_res$ScenarioGroup <- factor(scen_res$ScenarioGroup, levels=scens) 
  unique(scen_res$Variable)
  length(unique(scen_res$Scenario))
}

scen_res$Scenario = scen_res$ScenarioGroup
zz_all <- scen_res %>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 

#don't change any of these generic inputs 
if(T){
  library('readxl') #read_xlsx()
  library('cowplot') #get_legend()
  library('scales') #comma in y axis label 
  
  ### check you have inputs read in from June2021_UBDRO_...R to get scen_res
  unique(scen_res$ScenarioGroup)
  unique(scen_res$Variable)
  
  #set up folders for stats 
  figstats <- file.path(ofigs,"Stats") 
  if (!file.exists(figstats)) {
    message(paste('Creating folder:', figstats))
    dir.create(figstats)
  }
  message('Stats will be saved to: ', figstats)
  
  widths <- 11
  heights <- 6 #fits under title text in pptx 
  
  MinMaxLines<-F # T is want dotted line as min max of any given trace 
  
  plotColors <- c("#407ec9" , "#6b8f00", "#9a3324" ) #Reclamation blue, green, red
  plotColors <- c( "#6b8f00", "#9a3324" ) #green, red
  colorNames <- c(names(scens))
  exc_month <-12
  
  #Text
  TitleSize = 13
  AxisText = 11
  LegendLabText = 9.5
  
  AxisLab = 9
  LabSize = 2.9
  LegendText = 8
  
  #Lines
  IGStartLine = .8
  OpsLines = 1
  Medians = 1
  GridMaj = .25
  GridMin = .25
  
  # #Y axis limits
  # yaxmin = floor(min(zz$Min)/50)*50
  # yaxmax = ceiling(max(zz$Max)/50)*50
  # 
  # #Other
  LegendWidth = 1
  LegendHeight = 2.5
  
  # # Set tick marks for x and y axis
  # myXLabs <- seq(1990,3000,2)
  # myYLabs <- seq(-500,1200,50)
  
  yrs <- startyr:endyr #simplify 
}

NumCrit <- NA #3490
powtiers <- F
variable = "powell_dec_pe"
y_lab = "EOCY Elevation (ft)"
title = "Lake Mead" 
source("code/Cloud_plot_woHistv2.R")

NumCrit <- NA #3490
powtiers <- F
variable = "mead_dec_pe"
y_lab = "EOCY Elevation (ft)"
title = "Lake Mead" 
source("code/Cloud_plot_woHistv2.R")


