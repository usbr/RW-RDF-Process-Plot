library(RWDataPlyr)
library(tidyverse)
library(feather)

#Conor PC
# rwd <- read_rwd_agg("C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/rw_agg/rw_agg_CRSPPowerData_Energy.csv") 
#BA
rwd <- read_rwd_agg("C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/rw_agg/rw_agg_CRSPPowerData_Energy.csv") 
CRSSDIR <- Sys.getenv("CRSS_DIR")

## latest offical run #
scens_latest <- scens_latest_ST <- "JulIC_Jun2021_2022,ISM1988_2019,2016Dems,IG_DCP,24MS_Most"

## previous offical run #
scens_previous <- scens_previous_ST <- "JulIC_Jun2021_2022,ISM1988_2019,2016Dems,IG_DCPnoUBDRO,24MS_Most"

# # aggregate the combined scens list 
# # scens on Manoa 
rw_scen_aggregate(c(scens_latest,scens_previous), agg = rwd, scen_dir = "M:/Shared/CRSS/2021/Scenario",
                  file = "CRSPPowerData.feather")
# # # scens on D: drive of BA
# scen_dir <- "D:/2021/Scenario"
# list.dirs(path=scen_dir)
# rw_scen_aggregate(scens_latest, agg = rwd, 
#                   scen_dir = scen_dir,
#                   file = "CRSPPowerData.feather")
#ends up in the RW-Data.../code/
# zz <- feather::read_feather(file.path(CRSSDIR,"CRSPPowerData.feather")) 
zz <- feather::read_feather(file.path("C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/CRSPPowerData.feather")) 

zz <- zz %>%
  mutate(ScenarioGroup = case_when(
    Scenario %in% scens_previous_ST ~ "July 2021 no DRO", 
    Scenario %in% scens_latest_ST ~ "July 2021 w DRO", 
    TRUE ~ "BAD"
  ))

summary(zz)
unique(zz$ScenarioGroup)
unique(zz$Variable)
length(unique(zz$Scenario))

feather::write_feather(zz,file.path(CRSSDIR,"CRSPPowerData.feather")) 

# zz_check <- arrow::read_feather("C:/Users/fellette/Documents/GIT/CRSS/crsp_ops_data.feather") 
# summary(zz_check)
# unique(zz_check$ScenarioGroup)
# unique(zz_check$Variable)
# length(unique(zz_check$Scenario))