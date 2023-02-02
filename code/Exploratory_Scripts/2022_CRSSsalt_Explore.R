CRSSDIR <- Sys.getenv("CRSS_DIR")
scen_dir <- file.path(CRSSDIR,"Scenario")
scen_dir <- 'C:/Users/cfelletter/Documents/crss.2023TRIRVW/Scenario'

Model.Step.Name <- Figs <- "Jan2022_2020Scen3" #plot title and results/folder name #[plot type] identifying name .pdf
scens <- list(
  # "2020TriRvw_Scen3" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200409"#,
  "Jan2022_2020Scen3" = "CRMMS_Most,DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.0,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0"
)


scen_dir_overwrite="C:/Users/cfelletter/Documents/CRSS working/2023 TriRvw/CRSS_Sensitivity_2020v2022" #else provide a value for scen_dir_overwrite
feather_data_dir = scen_dir_overwrite

# 
# scen1 <- "CRMMS_Most,DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.0,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0"
# scen1_shrt_nm <- "FG_9002" ### ENSURE THIS IS WHAT YOU What results/[dir] to be named!!!! 
# scens <- scens <- list(scen1_shrt_nm = scen1)
# results_nm <- scen1_shrt_nm #results name should match 
# #libraries and setup directories 
# source(file.path(rwprocess_dir,"code","libs_n_dirs.R")) 
# warning(paste('Output will be to',Sys.getenv("CRSS_DIR"),'/results/',scen1_shrt_nm))



#libraries and setup directories, just use getwd()
source(file.path(getwd(),"code","libs_n_dirs.R")) #requires reults_nm 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                               END USER INPUT
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### =============== PROCESS =============== ####

# #CRSP operations 
# feather_file_nm <- "MeadPowellPE_jan2020_obs.feather"
# scen_res <- feather::read_feather(file.path(feather_data_dir,feather_file_nm)) 
# unique(scen_res$Variable)


rw_agg_file <- "WQAnn.csv" #only FWAAC
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE))
scen_res_TDS <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)
# summary(scen_res_TDS)
# unique(scen_res_TDS$Variable)


rw_agg_file <- "PoolElevation.csv" #only EOCY elevations 
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE))
scen_res_PE <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)


rw_agg_file <- "SaltResCheck.csv" #only EOCY elevations 
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE))
scen_res_ressalt <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)

scen_res <- rbind.data.frame(scen_res_TDS,scen_res_ressalt,scen_res_PE)
unique(scen_res$Year)

# scen_res_2022 <- scen_res
# scen_res_TriRvw <- scen_res #%>% 
# dplyr::filter(Year %in% 2020:2040) 
dim(scen_res_TriRvw) # 40920     6 vs 2022 having 23760     6

scen_res <- rbind.data.frame(scen_res_2022,scen_res_TriRvw)


# unique(scen_res$Variable)
unique(scen_res$Scenario)

lees <- scen_res %>%
  dplyr::filter(Variable == "AnnlSlntyLsFrry_FWAAC") %>%
  # dplyr::filter(Scenario == names(scens[1])) #%>%
low <- lees %>%
  dplyr::filter(Value < 360 ) 
dim(low) #there are no scenarios 
# low %>%
# write.csv(file = file.path(scen_dir_overwrite,'TriRvw_LsFrry_FWAAC_less360.csv'))

scen_res_TriRvw = scen_res_TriRvw %>%
  # dplyr::filter(Scenario == names(scens[1])) %>%
  group_by(Variable) %>%
  dplyr::filter(Year %in% 2020:2040) 

scen_res_TriRvw %>%
  # dplyr::filter(Scenario == names(scens[1])) %>%
  group_by(Variable) %>%
  dplyr::filter(Year %in% 2020:2040) %>%
  summarise('Mean' = mean(Value), StdDev = sd(Value)) %>%
  filter(Variable %in% c('AnnlSlntyLsFrry_FWAAC',"Powell.InSaltMass"       
                   ,"Powell.ReservoirSaltMass","Powell.Inflow","Powell.PoolElevation"    
                   ,"Mead.PoolElevation","TotVal.Powell")) %>%
  write.csv(file = file.path(scen_dir_overwrite,'TriRvw_PulRmv_Stats.csv'))


scen_res %>% 
  dplyr::filter(Scenario == names(scens[1])) %>%
  dplyr::filter(AnnlSlntyLsFrry_FWAAC < 360 ) #%>%
    pivot_wider(names_from = Variable, values_from = Value) %>%
  select(c('Year','TraceNumber','AnnlSlntyLsFrry_FWAAC',"Powell.InSaltMass"       
           ,"Powell.ReservoirSaltMass","Powell.Inflow","Powell.PoolElevation"    
           ,"Mead.PoolElevation","TotVal.Powell")) %>%
# write.csv(file = file.path(scen_dir_overwrite,'LsFrry_FWAAC_less360.csv'))
# write.csv(file = file.path(scen_dir_overwrite,'LsFrry_FWAAC_all.csv'))



# dtrace <-
      scen_res %>%
      # dplyr::filter(Variable == y_lab) %>%
  dplyr::filter(TraceNumber == 64) #%>% 
    # Error: cannot allocate vector of size 8.1 Gb
    # In addition: Warning message:
    #   In NextMethod() :
    #   number of items to replace is not a multiple of replacement length
    
  # write.csv(file = file.path(scen_dir_overwrite,'LsFrry_FWAAC_all.csv'))
View(dtrace)
    
    
y_lab = "AnnlSlntyLsFrry_FWAAC"
dtrace %>%
  dplyr::filter(Variable == y_lab) %>%
  group_by(Scenario) #%>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  # scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  # geom_line() +
  geom_point() +
  # ylim(ylims) +
  labs(title = y_lab, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)


unique(low$TraceNumber)

View(low)
unique(scen_res$Variable)

which(scen_res$Variable)


summary(scen_res_PE)


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


### Means ###

variable = "AnnlSlntyLsFrry_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River at Lees Ferry" 
subtitle = "Average Annual Concentration Comparison"
ylims <- c(400,600)


res_data <- scen_res_2022 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

# View(df)

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  # ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)
