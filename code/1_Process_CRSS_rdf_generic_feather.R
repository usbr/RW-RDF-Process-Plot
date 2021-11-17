# process CRSS scenarios and create .feather 
# CF Oct 2021 
rm(list=ls())

warning('Run CRSS/code/create_results_package() first')

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

scen_dir_overwrite="C:/Users/fellette/Documents/GIT/CRSS/Scenario" #else provide a value for scen_dir_overwrite
scen_dir_overwrite=FALSE # onBA=T "M:/Shared/CRSS/2021/Scenario", onBA=F "Z:/Shared/CRSS/2021/Scenario"

# which scens to process? 
singleIC=T

#get scen information from .yml file
yaml_nm=FALSE #give it name e.g. 
yaml_nm="aug2021_sensitivity.yml"

#libraries and setup directories, just use getwd()
source(file.path(getwd(),"code","libs_n_dirs.R")) 

#if you didn't get scen information from .yml file fill in the below
if(yaml_nm==FALSE){
  singleIC <- TRUE #FALSE = multiple IC from CRMMS
  if (singleIC == T) {
    ## latest run #
    
    scen1 <- "Aug2021.9002.FG,ISM1988_2019_HvrEvap_NvjEV_PwlEV,2016Dems,IG_DCPnoUBDRO.9001.BM,CRMMS_Most"
    scen1_shrt_nm <- "FG_9002" ### ENSURE THIS IS WHAT YOU What results/[dir] to be named!!!! 
    results_nm <- scen1_shrt_nm #results name should match 
    #libraries and setup directories 
    source(file.path(rwprocess_dir,"code","libs_n_dirs.R")) 
    warning(paste('Output will be to',Sys.getenv("CRSS_DIR"),'/results/',scen1_shrt_nm))
    
    ## compare to run #
    scen2 <- "Aug2021.9001.BM,ISM1988_2019_HvrEvap_NvjEV_PwlEV,2016Dems,IG_DCPnoUBDRO.9001.BM,CRMMS_Most"
    scen2_shrt_nm <- "BM_9001"
  } else {
    ## latest offical run #
    mdl_nm_scen1 <- scen1_shrt_nm <- "Jun2021_2022"
    rls_nm_scen1 <- "2016Dems,IG_DCP"
    scen1 <- rw_scen_gen_names(mdl_nm_scen1, c("ISM1988_2019"), rls_nm_scen1,paste0("Trace", sprintf("%02d", 4:38)))
    scen1 <- c(scen1_ST)
    ## compare to run #
    mdl_nm_scen2 <- scen2_shrt_nm <- "Jun2021_2022"
    rls_nm_scen2 <- "2016Dems,IG_DCP"
    scen2 <- rw_scen_gen_names(mdl_nm_scen2, c("ISM1988_2019"), rls_nm_scen2,paste0("Trace", sprintf("%02d", 4:38)))
  }
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                               END USER INPUT
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### =============== PROCESS =============== ####

#CRSP operations 
  rw_agg_nm <- "rw_agg_CRSPops.csv"
  feather_file_nm <- "crspopsdata.feather"
  
  # rw_agg_nm <- "rw_agg_CRSPPowerData_Energy.csv"
  # feather_file_nm <- "CRSPPowerData.feather"

rwd <- read_rwd_agg(file.path(rwprocess_dir,"rw_agg",rw_agg_nm)) 

rw_scen_aggregate(c(scen1,scen2),agg = rwd, scen_dir = scen_dir,file = file.path(feather_data_dir,feather_file_nm))

#add scenario group 
scen_res <- feather::read_feather(file.path(feather_data_dir,feather_file_nm)) 

#if we are only doing ST we don't need multiple hydro assignments 
  scen_res <- scen_res %>%
    mutate(ScenarioGroup = case_when(
      Scenario %in% scen1 ~ scen1_shrt_nm,
      Scenario %in% scen2 ~ scen2_shrt_nm, 
      TRUE ~ "BAD"))


# summary(scen_res)
unique(scen_res$ScenarioGroup)
# scen_res <- scen_res %>% 
#   dplyr::filter(ScenarioGroup != "BAD")
# unique(scen_res$ScenarioGroup)
# unique(scen_res$Variable)
feather::write_feather(scen_res,file.path(feather_data_dir,feather_file_nm)) 

message(paste('Feather written to',file.path(feather_data_dir,feather_file_nm),'use Plot_CRSS_UBres_generic_annual.R'))
