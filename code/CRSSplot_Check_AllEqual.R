library(feather)
library(tidyverse)

CRSSDIR <- Sys.getenv("CRSS_DIR")
crss_month <- "apr2021.9000"
filepath <- file.path(CRSSDIR,"results",crss_month,"tempData","MeadPowellPE.feather")

scen_res<- read_feather(filepath)
scens <- unique(scen_res$ScenarioGroup)
scen_res1 <- scen_res %>% dplyr::filter(ScenarioGroup == scens[1])
scen_res2 <- scen_res %>% dplyr::filter(ScenarioGroup == scens[2])
all.equal(scen_res1,scen_res2)
