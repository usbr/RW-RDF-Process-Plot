# process CRSS scenarios and create .feather 
# CF Oct 2021 
rm(list=ls())

warning('Run CRSS/code/create_results_package() first')

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




# which rdf/agg files to process? # ONLY ONE TRUE at a time 
CRSPops <- TRUE
CRSPpow <- FALSE

# which scens to process? 
onBA = FALSE # which computer BA or my PC?

scen_dir_overwrite=FALSE # onBA=T "M:/Shared/CRSS/2021/Scenario", onBA=F "Z:/Shared/CRSS/2021/Scenario"
#else provide a value for scen_dir_overwrite

#get scen information from .yml file
yaml_nm=FALSE #give it name e.g. 

if (onBA == TRUE) { #first set the rwprocess_dir so you can setup directories 

  rwprocess_dir <- "C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot"
} else {
  rwprocess_dir <- "C:/Users/cfelletter/Documents/RW-RDF-Process-Plot"
}
#libraries and setup directories 
source(file.path(rwprocess_dir,"code","libs_n_dirs.R")) 

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
    # "Aug2021_2022_RW831,ISM1988_2019,2016Dems,IG_DCPnoUBDRO,CRMMS_Most"
    # scen2_shrt_nm <- "Aug2021_RW831"
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
  
}


########## if don't need to process stop here ########


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                               END USER INPUT
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#### =============== PROCESS =============== ####



#CRSP operations 
if (CRSPops == T){
  rw_agg_nm <- "rw_agg_CRSPops.csv"
  feather_file_nm <- "crspopsdata.feather"
  
} else if (CRSPpow == T){ #CRSP energy 
  rw_agg_nm <- "rw_agg_CRSPPowerData_Energy.csv"
  feather_file_nm <- "CRSPPowerData.feather"

} 

rwd <- read_rwd_agg(file.path(rwprocess_dir,"rw_agg",rw_agg_nm)) 

rw_scen_aggregate(c(scen1,scen2),agg = rwd, scen_dir = scen_dir,file = file.path(feather_data_dir,feather_file_nm))

#add scenario group 
scen_res <- feather::read_feather(file.path(feather_data_dir,feather_file_nm)) 

if (singleIC == T) {
  
  scen_res <- scen_res %>%
    mutate(ScenarioGroup = case_when(
      Scenario %in% scen1 ~ scen1_shrt_nm,
      Scenario %in% scen2 ~ scen2_shrt_nm, 
      TRUE ~ "BAD"))
} else { ## for multi IC 
  scen_res <- scen_res %>%
    mutate(ScenarioGroup = case_when(
      Scenario %in% scens_latest_DNF ~ "DNF - 2007 DROA delivery - future DRO",
      Scenario %in% scens_latest_ST ~ "ST - 2007 DROA delivery - future DRO",
      Scenario %in% scens_previous_DNF ~ "DNF; 2021 DRO + no future DRO",
      Scenario %in% scens_previous_ST ~ "ST - 2007 DROA delivery - no future DRO",
      TRUE ~ "BAD"))
}

# summary(scen_res)
unique(scen_res$ScenarioGroup)
# scen_res <- scen_res %>% 
#   dplyr::filter(ScenarioGroup != "BAD")
# unique(scen_res$ScenarioGroup)
# unique(scen_res$Variable)
feather::write_feather(scen_res,file.path(feather_data_dir,feather_file_nm)) 

message(paste('Feather written to',file.path(feather_data_dir,feather_file_nm),'use Plot_CRSS_UBres_generic_annual.R'))
