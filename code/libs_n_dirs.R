#open normal libraries and setup directories 
# CF July 2021 - for Power Office 3490 analysis

# results_nm #results dir folder 
# onBA = TRUE # which computer BA or my PC?
# otherscenlocation <- NA #only if not manoa/Shared/CRSS/2021/Scenario 

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

source(file.path(rwprocess_dir,"code","stat-boxplot-custom.R")) #stat_boxplot_custom()

date_to_wy <- function(x) {
  mm <- month(x)
  yy <- year(x)
  yy[mm >= 10] <- yy[mm >= 10] + 1
  yy
}

#### =============== Directories INPUTS =============== ####

if (onBA == TRUE) {
  rwprocess_dir <- "C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot"
  scen_dir <- "M:/Shared/CRSS/2021/Scenario" # Manoa CRSS shared from BA
} else {
  rwprocess_dir <- "C:/Users/cfelletter/Documents/RW-RDF-Process-Plot"
  scen_dir <- "Z:/Shared/CRSS/2021/Scenario" # Manoa CRSS shared from BA
}
# list.dirs(path=scen_dir)

CRSSDIR <- Sys.getenv("CRSS_DIR")

#### =============== DIRECTORIES CREATED AUTOMATICALLY =============== ####

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

