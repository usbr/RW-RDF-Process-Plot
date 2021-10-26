library(feather)
library(tidyverse)

CRSSDIR <- Sys.getenv("CRSS_DIR")
crss_month <- "RW831_AugOffcFix"
#MeadPowellPE.feather
filepath <- file.path(CRSSDIR,"results",crss_month,"tempData","MeadPowellPE.feather")

scen_res<- read_feather(filepath)
scens <- unique(scen_res$ScenarioGroup)
scen_res1 <- scen_res %>% dplyr::filter(ScenarioGroup == scens[1])
scen_res2 <- scen_res %>% dplyr::filter(ScenarioGroup == scens[2])
all.equal(scen_res1,scen_res2)

unique(scen_res1$Variable)
slotnm <-  "powell_dec_pe"
scen1_slot <- scen_res1 %>% dplyr::filter(Variable == slotnm)
scen2_slot <- scen_res2 %>% dplyr::filter(Variable == slotnm)
all.equal(scen1_slot,scen2_slot)

#SysCond.feather
filepath <- file.path(CRSSDIR,"results",crss_month,"tempData","SysCond.feather")

scen_res<- read_feather(filepath)
scens <- unique(scen_res$ScenarioGroup)
scen_res1 <- scen_res %>% dplyr::filter(ScenarioGroup == scens[1])
scen_res2 <- scen_res %>% dplyr::filter(ScenarioGroup == scens[2])
all.equal(scen_res1,scen_res2)

unique(scen_res1$Variable)
slotnm <-  c("mer823","uebLt823","eq823","uebGt823","ueb823","mer748","eq","lebGt823","lebLt823","leb823") 
scen1_slot <- scen_res1 %>% dplyr::filter(Variable%in%slotnm)
scen2_slot <- scen_res2 %>% dplyr::filter(Variable%in%slotnm)
all.equal(scen1_slot,scen2_slot)
