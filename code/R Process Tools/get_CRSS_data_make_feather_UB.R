library(RWDataPlyr)
library(tidyverse)
library(feather)

rwd <- read_rwd_agg("C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/rw_agg/rw_agg_CRSPops.csv")

CRSSDIR <- Sys.getenv("CRSS_DIR")

## previous offical run #
previous_scens <- rw_scen_gen_names("Apr2021_2022.v5.1", c("DNF", "ISM1988_2019"),
                                    "2016Dems,IG_DCP.v5.1", 
                                    paste0("Trace", sprintf("%02d", 4:38))) # this creates a double digit number, e.g. 04 
previous_scens

previous_scens_DNF <- rw_scen_gen_names("Apr2021_2022.v5.1", c("DNF"),
                                    "2016Dems,IG_DCP.v5.1", 
                                    paste0("Trace", sprintf("%02d", 4:38))) # this creates a double digit number, e.g. 04 
previous_scens_ST <- rw_scen_gen_names("Apr2021_2022.v5.1", c("ISM1988_2019"),
                                    "2016Dems,IG_DCP.v5.1", 
                                    paste0("Trace", sprintf("%02d", 4:38))) # this creates a double digit number, e.g. 04 

## last offical run #
scens_latest <- rw_scen_gen_names("Jun2021_2022", c("DNF", "ISM1988_2019"),
                                  "2016Dems,IG_DCP", 
                                  paste0("Trace", 4:38))
scens_latest_DNF <- rw_scen_gen_names("Jun2021_2022", c("DNF"),
                                  "2016Dems,IG_DCP", 
                                  paste0("Trace", 4:38))
scens_latest_ST <- rw_scen_gen_names("Jun2021_2022", c("ISM1988_2019"),
                                  "2016Dems,IG_DCP", 
                                  paste0("Trace", 4:38))
scens_latest


## no DRO run #
scens_noUBDRO <- rw_scen_gen_names("Jun2021_2022", c("DNF", "ISM1988_2019"),
                                   "2016Dems,IG_DCPnoUBDRO", 
                                   paste0("Trace", 4:38))
scens_noUBDRO

scens_noUBDRO_DNF <- rw_scen_gen_names("Jun2021_2022", c("DNF"),
                                   "2016Dems,IG_DCPnoUBDRO", 
                                   paste0("Trace", 4:38))
scens_noUBDRO_ST <- rw_scen_gen_names("Jun2021_2022", c("ISM1988_2019"),
                                      "2016Dems,IG_DCPnoUBDRO", 
                                   paste0("Trace", 4:38))

scens <- c(previous_scens,scens_latest,scens_noUBDRO) #combine

# # aggregate the combined scens list 
# rw_scen_aggregate(scens_latest, agg = rwd, scen_dir = "M:/Shared/CRSS/2021/Scenario", 
#                   file = "crsp_ops_data_latest.feather")


zz1 <- feather::read_feather(file.path(CRSSDIR,"crsp_ops_data_prv.feather")) 
head(zz1)  
zz2 <- feather::read_feather(file.path(CRSSDIR,"crsp_ops_data_cur.feather")) 
head(zz2)  

zz <- rbind.data.frame(zz1,zz2)
summary(zz)

zz <- zz %>%
  mutate(ScenarioGroup = case_when(
    Scenario %in% previous_scens_DNF ~ "April 2021 - DNF IG", 
    Scenario %in% previous_scens_ST ~ "April 2021 - ST IG", 
    Scenario %in% scens_latest_DNF ~ "June 2021 - DNF IG", 
    Scenario %in% scens_latest_ST ~ "June 2021 - ST IG", 
    Scenario %in% scens_noUBDRO_DNF ~ "June 2021 - DNF IG noUBDRO", 
    Scenario %in% scens_noUBDRO_ST ~ "June 2021 - ST IG noUBDRO", 
    TRUE ~ "BAD"
  ))

summary(zz)
unique(zz$ScenarioGroup)
unique(zz$Variable)
length(unique(zz$Scenario))


arrow::write_feather(zz,file.path(CRSSDIR,"crsp_ops_data.feather")) 

zz_check <- arrow::read_feather("C:/Users/fellette/Documents/GIT/CRSS/crsp_ops_data.feather") 
summary(zz_check)
unique(zz_check$ScenarioGroup)
unique(zz_check$Variable)
length(unique(zz_check$Scenario))

# Error in coldataFeather(x, i) : 
#   embedded nul in string: 'Apr2021_2022.v5.1,DNF,2016Dems,I\0\0\0\0\v\0\0\0\026\0\0\0!\0\0\0,\0'
