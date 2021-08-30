#get_data.R creates data frames that are not one of the 'standard' files saved in feather format
#for use in explore_powell_power.R

results_nm <- "Aug2021" #results dir folder 

### get data ###
if (FALSE) {
  
onBA <- TRUE # which computer BA or my PC? find RW-RDF-Process-Plot dir 
if (onBA == TRUE) {
  rwprocess_dir <- "C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot"
} else {
  rwprocess_dir <- "C:/Users/cfelletter/Documents/RW-RDF-Process-Plot"
}

#libraries and setup directories 
source(file.path(rwprocess_dir,"code","libs_n_dirs.R")) 
# scen_dir = #OVERWRITE if not manoa/Shared/CRSS/2021/Scenario

#set up folders for stats 
figstats <- file.path(ofigs,"Stats") 
if (!file.exists(figstats)) {
  message(paste('Creating folder:', figstats))
  dir.create(figstats)
}
message('Stats will be saved to: ', figstats)

mtom <- paste0("Trace", 4:38)

jun_st <- rw_scen_gen_names("Jun2021_2022,ISM1988_2019,2016Dems,IG_DCP", mtom)
jun_st_nodo <- rw_scen_gen_names("Jun2021_2022,ISM1988_2019,2016Dems,IG_DCPnoUBDRO",
                                 mtom)
aug_st <- "Aug2021_2022,ISM1988_2019,2016Dems,IG_DCP,CRMMS_Most"
aug_st_nodo <- "Aug2021_2022,ISM1988_2019,2016Dems,IG_DCPnoUBDRO,CRMMS_Most"

scens <- c(aug_st_nodo, aug_st, jun_st, jun_st_nodo)
scens <- c(aug_st_nodo, aug_st)


rw_agg_nm <- "rw_agg_DRO_augjun.csv"
feather_file_nm <- "augjun2021_drodata.feather"

print(paste("reading from",scen_dir))

rwd <- read_rwd_agg(file.path(rwprocess_dir,"rw_agg",rw_agg_nm)) 
rw_scen_aggregate(scens,agg = rwd, scen_dir = scen_dir,file = file.path(feather_data_dir,feather_file_nm))

#add scenario group 
zz <- feather::read_feather(file.path(feather_data_dir,feather_file_nm)) 

zz <- zz %>%
  mutate(Scenario = case_when(
    Scenario %in% aug_st ~ "July 2021 w DRO",
    Scenario %in% aug_st_nodo ~ "July 2021 no DRO", 
    TRUE ~ "BAD"))

feather::write_feather(zz,file.path(feather_data_dir,feather_file_nm)) 

}
unique(zz$Scenario)

#### process ###



### Read Data ###
zz_inout <- zz %>%
  dplyr::filter(Variable %in% c('FlamingGorge.Outflow', 'BlueMesa.Outflow','Navajo.Outflow','Powell.Inflow')) %>%
  dplyr::group_by(Variable,ScenarioGroup,Scenario,TraceNumber,Year) %>%
  dplyr::summarise(Value = sum(Value)) %>% #inflow and outflow this needs to be sum(Value)
  mutate(Value = Value/1000) #convert to MAF after we convert to AF
head(zz_inout)
unique(zz_inout$Variable)



zz_all <- rbind.data.frame(zz_inout,zz_pe,zz_stor) %>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(ScenarioGroup, Variable, Year) %>% #don't use scenario here 
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 

zz_all$Scenario = zz_all$ScenarioGroup
# summary(zz_all)
