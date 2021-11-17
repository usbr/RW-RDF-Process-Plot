#open normal libraries and setup directories 
# onBA = TRUE # which computer BA or my PC?
# # otherscenlocation <- NA #only if not manoa/Shared/CRSS/2021/Scenario 
# results_nm="default_results"
# scen_dir_overwrite=FALSE
# yaml_nm=FALSE
#   # results_nm #results dir folder 
#  
  #### =============== Libraries =============== ####
  
  library(tidyverse)
  library(zoo)
  library(RWDataPlyr)
  library(lubridate)
  library(crssplot) #scens_plot_cloud
  # remotes::install_github('rabutler-usbr/crssplot')
  # remotes::install_github("BoulderCodeHub/rhdb") #need github PAT env var set
  library(rhdb) 
  library(feather)
  library(scales) #comma labels for axis
  
  #### =============== Directories INPUTS =============== ####
  
  # if (onBA == TRUE) {
  #   rwprocess_dir <- "C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot"
  #   scen_dir <- "M:/Shared/CRSS/2021/Scenario" # Manoa CRSS shared from BA
  # } else {
  #   rwprocess_dir <- "C:/Users/cfelletter/Documents/RW-RDF-Process-Plot"
  #   scen_dir <- "Z:/Shared/CRSS/2021/Scenario" # Manoa CRSS shared from BA
  # }
  rwprocess_dir <- getwd()
  # list.dirs(path=scen_dir)
  
  source(file.path(rwprocess_dir,"code","stat-boxplot-custom.R")) #stat_boxplot_custom()
  
  date_to_wy <- function(x) {
    mm <- month(x)
    yy <- year(x)
    yy[mm >= 10] <- yy[mm >= 10] + 1
    yy
  }
  
  CRSSDIR <- Sys.getenv("CRSS_DIR")
  
  #### =============== yaml INPUTS =============== ####
  
  if(yaml_nm!=FALSE){
    # yaml_nm <- "aug2021_sensitivity.yml"
    
    yaml_path <- file.path(CRSSDIR,"code",yaml_nm)
    yaml <- yaml::read_yaml(yaml_path)
    
    scen_dir<-i_folder<-yaml$folders$i_folder
    results_nm <- crss_mont <- yaml$folders$crss_month
    
    scen1 <- yaml$scenarios[[1]]$folder
    scen1_shrt_nm <- yaml$scenarios[[1]]$name
    
    scen2 <- yaml$scenarios[[2]]$folder
    scen2_shrt_nm <- yaml$scenarios[[2]]$name
    
    scens <- list(scen1,scen2)
    names(scens) = c(scen1_shrt_nm,scen2_shrt_nm)
    
  }
  
  #### =============== DIRECTORIES CREATED AUTOMATICALLY =============== ####
  
  if(scen_dir_overwrite != F){
    scen_dir = scen_dir_overwrite
  }
  
  ## === results directores
  results_dir <- file.path(CRSSDIR,"results",results_nm) 
  feather_data_dir <- file.path(results_dir,"tempData")
  figures_dir <- file.path(results_dir,"figures")
  
  ## === Create directories if dont already exist
  dir_v = c(results_dir, feather_data_dir, figures_dir)
  for (dir_check in dir_v) {
    dir.create(file.path(dir_check), showWarnings = FALSE)
  }
  message('Figures will be saved to: ', results_dir)
  
  source(file.path(rwprocess_dir,"code","stat-boxplot-custom.R")) #stat_boxplot_custom()


