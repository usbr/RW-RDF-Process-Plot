library(RWDataPlyr)
library(tidyverse)
library(feather)

#Conor PC
rwd <- read_rwd_agg("C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/rw_agg/rw_agg_CRSPPowerData_Energy.csv") 
#BA
rwd <- read_rwd_agg("C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/rw_agg/rw_agg_CRSPPowerData_Energy.csv") 
CRSSDIR <- Sys.getenv("CRSS_DIR")

## latest offical run #
scens_latest <- rw_scen_gen_names("Jun2021_2022", 
                                  c("DNF", "ISM1988_2019"),
                                  "2016Dems,IG_DCP", 
                                  # paste0("2021DRO_Trace", 4:38))
                                  paste0("2021DRO_Trace", sprintf("%02d", 4:38))) # this creates a double digit number, e.g. 04 

scens_latest_DNF <- rw_scen_gen_names("Jun2021_2022", c("DNF"),
                                      "2016Dems,IG_DCP", 
                                      # paste0("2021DRO_Trace", 4:38))
                                      paste0("2021DRO_Trace", sprintf("%02d", 4:38))) # this creates a double digit number, e.g. 04 
scens_latest_ST <- rw_scen_gen_names("Jun2021_2022", c("ISM1988_2019"),
                                     "2016Dems,IG_DCP", 
                                     # paste0("2021DRO_Trace", 4:38))
                                     paste0("2021DRO_Trace", sprintf("%02d", 4:38))) # this creates a double digit number, e.g. 04 
scens_latest

## previous offical run #
scens_previous <- rw_scen_gen_names("Jun2021_2022", 
                                  c("DNF", "ISM1988_2019"),
                                  "2016Dems,IG_DCPnoUBDRO", 
                                  # paste0("2021DRO_Trace", 4:38))
                                  paste0("Trace", sprintf("%02d", 4:38))) # this creates a double digit number, e.g. 04 
scens_previous_DNF <- rw_scen_gen_names("Jun2021_2022", c("DNF"),
                                      "2016Dems,IG_DCPnoUBDRO", 
                                      # paste0("2021DRO_Trace", 4:38))
                                      paste0("Trace", sprintf("%02d", 4:38))) # this creates a double digit number, e.g. 04 
scens_previous_ST <- rw_scen_gen_names("Jun2021_2022", c("ISM1988_2019"),
                                     "2016Dems,IG_DCPnoUBDRO", 
                                     # paste0("2021DRO_Trace", 4:38))
                                     paste0("Trace", sprintf("%02d", 4:38))) # this creates a double digit number, e.g. 04 
scens_previous

# # aggregate the combined scens list 
# # scens on Manoa 
rw_scen_aggregate(c(scens_latest,scens_previous), agg = rwd, scen_dir = "M:/Shared/CRSS/2021/Scenario",
                  file = "CRSPPowerData.feather")
# # scens on D: drive of BA
scen_dir <- "D:/2021/Scenario"
list.dirs(path=scen_dir)
rw_scen_aggregate(scens_latest, agg = rwd, 
                  scen_dir = scen_dir,
                  file = "CRSPPowerData.feather")
#ends up in the RW-Data.../code/
# zz <- feather::read_feather(file.path(CRSSDIR,"CRSPPowerData.feather")) 
zz <- feather::read_feather(file.path("C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/code/CRSPPowerData.feather")) 

zz <- zz %>%
  mutate(ScenarioGroup = case_when(
    Scenario %in% scens_latest_DNF ~ "June 2021 - DNF IG", 
    Scenario %in% scens_latest_ST ~ "June 2021 - ST IG", 
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