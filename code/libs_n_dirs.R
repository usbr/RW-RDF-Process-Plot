#open normal libraries and setup directories 
# results_nm #results dir folder 
# onBA = TRUE # which computer BA or my PC?
# otherscenlocation <- NA #only if not manoa/Shared/CRSS/2021/Scenario 

libs_n_dirs <- function(results_nm,onBA=T,otherscenlocation=T) {
  #### =============== Libraries =============== ####
  
  library(RWDataPlyr)
  library(tidyverse)
  library(feather)
  
  #### =============== Directories INPUTS =============== ####
  
  if (onBA == TRUE) {
    rwprocess_dir <- "C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot"
    scen_dir <- "M:/Shared/CRSS/2021/Scenario" # Manoa CRSS shared from BA
  } else {
    rwprocess_dir <- "C:/Users/cfelletter/Documents/RW-RDF-Process-Plot"
    scen_dir <- "Z:/Shared/CRSS/2021/Scenario" # Manoa CRSS shared from BA
  }
  
  if (!is.na(otherscenlocation)) {
    scen_dir <- otherscenlocation  #not on 
  } 
  # list.dirs(path=scen_dir)
  
  CRSSDIR <- Sys.getenv("CRSS_DIR")
  
  #### =============== DIRECTORIES CREATED AUTOMATICALLY =============== ####
  
  ## === results directores
  results_dir <- file.path(CRSSDIR,"results",results_nm) 
  feather_data_dir <- file.path(results_dir,"tempData",results_nm)
  figures_dir <- file.path(results_dir,"figures",results_nm)
  
  ## === Create directories if dont already exist
  dir_v = c(results_dir, feather_data_dir, figures_dir)
  for (dir_check in dir_v) {
    dir.create(file.path(dir_check), showWarnings = FALSE)
  }
  message('Figures will be saved to: ', results_dir)
}
