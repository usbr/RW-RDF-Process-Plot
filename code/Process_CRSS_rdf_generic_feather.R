library(RWDataPlyr)
library(tidyverse)
library(feather)

#Conor PC
rw_agg_nm <- "rw_agg_CRSPPowerData_Energy"
rw_agg_nm <- "rw_agg_CRSPops"

rw_agg_file_path <- "C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/rw_agg/"
rwd <- read_rwd_agg(paste0(rw_agg_file_path,rw_agg_nm,".csv")) 
rwd$variable

CRSSDIR <- Sys.getenv("CRSS_DIR")

## latest offical run #
# #"DNF; with future DRO"          "ST; with future DRO" 
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
                                  paste0("2021DRO_Trace", sprintf("%02d", 4:38))) # this creates a double digit number, e.g. 04 
scens_previous_DNF <- rw_scen_gen_names("Jun2021_2022", c("DNF"),
                                      "2016Dems,IG_DCPnoUBDRO", 
                                      # paste0("2021DRO_Trace", 4:38))
                                      paste0("2021DRO_Trace", sprintf("%02d", 4:38))) # this creates a double digit number, e.g. 04 
scens_previous_ST <- rw_scen_gen_names("Jun2021_2022", c("ISM1988_2019"),
                                     "2016Dems,IG_DCPnoUBDRO", 
                                     # paste0("2021DRO_Trace", 4:38))
                                     paste0("2021DRO_Trace", sprintf("%02d", 4:38))) # this creates a double digit number, e.g. 04 
scens_previous

## more scens #
scens_more <- rw_scen_gen_names("Jun2021_2022", 
                                    c("DNF", "ISM1988_2019"),
                                    "2016Dems,IG_DCPnoUBDRO", 
                                    # paste0("2021DRO_Trace", 4:38))
                                    paste0("2021DRO_Trace", sprintf("%02d", 4:38))) # this creates a double digit number, e.g. 04 
scens_more_DNF <- rw_scen_gen_names("Jun2021_2022", c("DNF"),
                                        "2016Dems,IG_DCPnoUBDRO", 
                                        # paste0("2021DRO_Trace", 4:38))
                                        paste0("2021DRO_Trace", sprintf("%02d", 4:38))) # this creates a double digit number, e.g. 04 
scens_more_ST <- rw_scen_gen_names("Jun2021_2022", c("ISM1988_2019"),
                                       "2016Dems,IG_DCPnoUBDRO", 
                                       # paste0("2021DRO_Trace", 4:38))
                                       paste0("2021DRO_Trace", sprintf("%02d", 4:38))) # this creates a double digit number, e.g. 04 

# # aggregate the combined scens list 
feather_file_nm <- "CRSPPowerData.feather"
feather_file_nm <- "crsp_ops_data.feather"

# # scens on Manoa 
rw_scen_aggregate(c(scens_latest,scens_previous), 
                  agg = rwd, scen_dir = "M:/Shared/CRSS/2021/Scenario",
                  file = feather_file_nm)
# # scens on D: drive of BA
scen_dir <- "D:/2021/Scenario"
list.dirs(path=scen_dir)
# rw_scen_aggregate(c(scens_latest,scens_previous), 
rw_scen_aggregate(c(scens_latest_ST,scens_previous_ST), 
                  agg = rwd, 
                  scen_dir = scen_dir,
                  file = feather_file_nm)

# # latest only
# rw_scen_aggregate(scens_latest, agg = rwd, 
#                   scen_dir = scen_dir,
#                   file = feather_file_nm)
#ends up in the RW-Data.../
# zz <- feather::read_feather(file.path(CRSSDIR,feather_file_nm)) 
feather_path <- "C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/"
list.files(feather_path)
# zz <- arrow::read_feather(file.path(feather_path,feather_file_nm)) #arrow feather
zz <- feather::read_feather(file.path(feather_path,feather_file_nm)) #feather, normal
unique(zz$Scenario)


zz <- zz %>%
  mutate(ScenarioGroup = case_when(
    Scenario %in% scens_latest_DNF ~ "DNF - 2007 DROA delivery - future DRO",
    Scenario %in% scens_latest_ST ~ "ST - 2007 DROA delivery - future DRO",
    Scenario %in% scens_previous_DNF ~ "DNF; 2021 DRO + no future DRO"
    Scenario %in% scens_previous_ST ~ "ST - 2007 DROA delivery - no future DRO" 
    # Scenario %in% scens_latest_DNF ~ "DNF; 2021 DRO + future DRO",
    # Scenario %in% scens_latest_ST ~ "ST; 2021 DRO + future DRO",
    # Scenario %in% scens_previous_DNF ~ "DNF; 2021 DRO + no future DRO"
    # Scenario %in% scens_previous_ST ~ "ST; 2021 DRO + no future DRO" 
    TRUE ~ "BAD"
  ))

"ST - no 2007 DROA delivery - w/ future DRO"

# summary(zz)
unique(zz$ScenarioGroup)
# zz <- zz %>% 
#   dplyr::filter(ScenarioGroup != "BAD")
# unique(zz$ScenarioGroup)

unique(zz$Variable)
length(unique(zz$Scenario))
feather::write_feather(zz,file.path(CRSSDIR,feather_file_nm)) 

# zz_check <- arrow::read_feather("C:/Users/fellette/Documents/GIT/CRSS/crsp_ops_data.feather") 
# summary(zz_check)
# unique(zz_check$ScenarioGroup)
# unique(zz_check$Variable)
# length(unique(zz_check$Scenario))