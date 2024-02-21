#Additional plotting functions and libraries 
library('tidyverse') #ggplot2,dplyr,tidyr
library('devtools')
library(RWDataPlyr)
library(CRSSIO)
library(xml2)
library(gridExtra)

CRSSDIR <- Sys.getenv("CRSS_DIR")
# CRSSDIR <- "C:/Users/cfelletter/Documents/CRSS"

results_dir <- file.path(CRSSDIR,"results") 
# # where rdf results folder is kept

#easier to make folder from output in the results dir than to move it 
scen_dir <- file.path(CRSSDIR,"results") #file.path(CRSSDIR,"Scenario")
# #containing the sub folders for each ensemble
scens <- "9002"

printfigs<-T#T#make png figures and dump data 

mycolors <- c("#61bd17","#009E73","#6bbd28","#0072B2") #for Sector plots mid dark green - schedule depl, dark green - depl rqst, light green - depletion, blue - CUL, 
mylinetypes <- c("dotdash","dashed","solid","solid","dotdash")  #schedule depl, depl rqst, depletion, CUL, 

nyears <- length(2000:2020) #currently only used one place to calculate average

#standard powerpoint figure sizes 
# gage an, an gage resid, gage mon, mon gage resid, total use  
gage_widths <- c(9.5,9.5,9.5,9.5,9.5)
gage_heights <- c(7,7,7,7,7)
# sect an, sect mon, mon sect dist  
sect_widths <- c(5,6.5,5)
sect_heights <- c(3,4.2,3)

# #whole basin experiment figure sizes 
# # sect an, sect mon, mon sect dist  
sect_widths <- c(3.33,5,3.33)
sect_heights <- c(2,3,2)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### File Checks #####

scens <- list(
  "V6.9002" = "9002",
  "V5.TriRvw" = "V5.TriRvw,CRSS.V5.3.0.203.Jan2022.2023TriRvw.10.2022SaltIC.2000start.mdl,20221115NFS"
)

file_dir <- file.path(results_dir,scens[1])
if (!file.exists(file_dir)) {
  message(paste('Creating folder:', file_dir,'move results rdfs into this dir and then proceed with code'))
  dir.create(file_dir)
  stop() #if created folder need to move results rdfs into this dir and then proceed with code
}

fig_dir <-  file.path(file_dir,"png_figures")
data_dir <-  file.path(file_dir,"csv_data")

if (!file.exists(fig_dir) | !file.exists(data_dir)) {
  dir.create(fig_dir)
  dir.create(data_dir)
}



oFigs <- file_dir
Model.Step.Name <- Figs <- scens #plot title and results/folder name #[plot type] identifying name .pdf
startyr <- 2000 #filter out all years > this year
endyr <- 2020
width=9# 10 #9
height=6 #6.67 #6
customcolorltpt <- F
lt_scale <- rep(1, 4)
pt_scale <- rep(19, 4)
mycolors <- c("#D55E00" , "#F0E442", "#009E73" , "#407ec9") #TRY 2 - color blind red, yellow, red (stop light), blue



unique(scen_res$Variable)

#SaltMassBal
source("code/Custom_MassBalAnn.R")  
