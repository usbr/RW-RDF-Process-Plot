# process CRSS scenarios and create .feather 
# CF July 2021 - for Power Office 3490 analysis
rm(list=ls())

#### =============== INPUTS =============== ####

results_nm <- "Jul2021_MostPowerRun" #results dir folder 

onBA <- TRUE # which computer BA or my PC? find RW-RDF-Process-Plot dir 
if (onBA == TRUE) {
  rwprocess_dir <- "C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot"
} else {
  rwprocess_dir <- "C:/Users/cfelletter/Documents/RW-RDF-Process-Plot"
}

#libraries and setup directories 
source(file.path(rwprocess_dir,"code","libs_n_dirs.R")) 
# scen_dir = #OVERWRITE if not manoa/Shared/CRSS/2021/Scenario

# which rdf/agg files to process? # ONLY ONE TRUE at a time 
CRSPops <- TRUE
CRSPpow <- FALSE
DRO <- FALSE

# which scens to process? 
singleIC <- TRUE #FALSE = multiple IC from CRMMS
if (singleIC == T) {
  ## latest offical run #
  scen1 <- scens_latest_ST <- "JulIC_Jun2021_2022,ISM1988_2019,2016Dems,IG_DCP,24MS_Most"
  
  ## compare to run #
  scen2 <- "JulIC_Jun2021_2022,ISM1988_2019,2016Dems,IG_DCPnoUBDRO,24MS_Most"
} else {
  ## latest offical run #
  mdl_nm_scen1 <- "Jun2021_2022"
  rls_nm_scen1 <- "2016Dems,IG_DCP"
  scen1_DNF <- rw_scen_gen_names(mdl_nm_scen1, c("DNF"),rls_nm_scen1, 
                                       # paste0("Trace", 4:38))
                                       paste0("Trace", sprintf("%02d", 4:38))) # this creates a double digit number, e.g. 04 
  scen1_ST <- rw_scen_gen_names(mdl_nm_scen1, c("ISM1988_2019"), rls_nm_scen1,paste0("Trace", sprintf("%02d", 4:38)))
  scen1 <- c(scen1_DNF,scen1_ST)
  
  ## compare to run #
  mdl_nm_scen2 <- "Jun2021_2022"
  rls_nm_scen2 <- "2016Dems,IG_DCP"
  scen2_DNF <- rw_scen_gen_names(mdl_nm_scen2, c("DNF"),rls_nm_scen2, 
                                       # paste0("Trace", 4:38))
                                       paste0("Trace", sprintf("%02d", 4:38))) # this creates a double digit number, e.g. 04 
  scen2_ST <- rw_scen_gen_names(mdl_nm_scen2, c("ISM1988_2019"), rls_nm_scen2,paste0("Trace", sprintf("%02d", 4:38)))
  scen2 <- c(scen2_DNF,scen2_ST)
}

#### =============== PROCESS =============== ####

#CRSP operations 
if (CRSPops == T){
  rw_agg_nm <- "rw_agg_CRSPops.csv"
  feather_file_nm <- "crspopsdata.feather"
  
} else if (CRSPpow == T){ #CRSP energy 
  rw_agg_nm <- "rw_agg_CRSPPowerData_Energy.csv"
  feather_file_nm <- "CRSPPowerData.feather"

} else if (DRO == T){ 
  rw_agg_nm <- "rw_agg_DRO.csv"
  feather_file_nm <- "DRO.feather"
  
} 
rwd <- read_rwd_agg(file.path(rwprocess_dir,"rw_agg",rw_agg_nm)) 

rw_scen_aggregate(c(scen1,scen2),agg = rwd, scen_dir = scen_dir,file = file.path(feather_data_dir,feather_file_nm))

#add scenario group 
zz <- feather::read_feather(file.path(feather_data_dir,feather_file_nm)) 

if (singleIC == T) {
  
  zz <- zz %>%
    mutate(ScenarioGroup = case_when(
      Scenario %in% scen1 ~ "July 2021 w DRO",
      Scenario %in% scen2 ~ "July 2021 no DRO", 
      TRUE ~ "BAD"))
} else { ## for multi IC 
  zz <- zz %>%
    mutate(ScenarioGroup = case_when(
      Scenario %in% scens_latest_DNF ~ "DNF - 2007 DROA delivery - future DRO",
      Scenario %in% scens_latest_ST ~ "ST - 2007 DROA delivery - future DRO",
      Scenario %in% scens_previous_DNF ~ "DNF; 2021 DRO + no future DRO",
      Scenario %in% scens_previous_ST ~ "ST - 2007 DROA delivery - no future DRO",
      TRUE ~ "BAD"))
}

# summary(zz)
unique(zz$ScenarioGroup)
# zz <- zz %>% 
#   dplyr::filter(ScenarioGroup != "BAD")
# unique(zz$ScenarioGroup)
# unique(zz$Variable)
feather::write_feather(zz,file.path(feather_data_dir,feather_file_nm)) 
