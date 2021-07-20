#get_data.R creates data frames that are not one of the 'standard' files saved in feather format
#for use in explore_powell_power.R


library(RWDataPlyr)

mtom <- paste0("Trace", 4:38)
apr_dnf <- rw_scen_gen_names("Apr2021_2022.v5.1,DNF,2016Dems,IG_DCP.v5.1", mtom)
apr_st <- rw_scen_gen_names("Apr2021_2022.v5.1,ISM1988_2019,2016Dems,IG_DCP.v5.1",
                            mtom)
jun_dnf <- rw_scen_gen_names("Jun2021_2022,DNF,2016Dems,IG_DCP", mtom)
jun_st <- rw_scen_gen_names("Jun2021_2022,ISM1988_2019,2016Dems,IG_DCP", mtom)
jun_dnf_nodo <- rw_scen_gen_names("Jun2021_2022,DNF,2016Dems,IG_DCPnoUBDRO", mtom)
jun_st_nodo <- rw_scen_gen_names("Jun2021_2022,ISM1988_2019,2016Dems,IG_DCPnoUBDRO",
                                 mtom)

scens <- c(apr_dnf, apr_st, jun_dnf, jun_st, jun_dnf_nodo, jun_st_nodo)

rwd2 <- rwd_agg(data.frame(
  file = c("UBDO.rdf", "KeySlots.rdf", "KeySlots.rdf"), 
  slot = c("ExtendedOperations.PowellForecastDeficitFlag", "PowellOperation.PowellWYRelease",
           "TotVal.Powell"),
  period = c("cy", "asis", "cy"), 
  summary = c("max", NA, "sum"), 
  eval = NA, t_s = NA, 
  variable = c("ubdo_flag", "powell_wy_rel", "lf_nf")
))

rw_scen_aggregate(scens, agg = rwd2, scen_dir = "M:/Shared/CRSS/2021/Scenario", 
                  file = "jun_extra_powell.feather")