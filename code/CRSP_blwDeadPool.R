# process CRSS scenarios and check which CRSP res are below dead pool 
# CF created July 2021
rm(list=ls())

#### =============== INPUTS =============== ####

results_nm <- "Jul2021_MostPowerRun" #results dir folder 

onBA <- F # which computer BA or my PC? find RW-RDF-Process-Plot dir 
if (onBA == TRUE) {
  rwprocess_dir <- "C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot"
} else {
  rwprocess_dir <- "C:/Users/cfelletter/Documents/RW-RDF-Process-Plot"
}

#libraries and setup directories 
source(file.path(rwprocess_dir,"code","libs_n_dirs.R")) 
# scen_dir = #OVERWRITE if not manoa/Shared/CRSS/2021/Scenario

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

rw_agg_nm <- "rw_agg_CRSP_pe_blwdead.csv"
# feather_file_nm <- "crspopsdata.feather"

rwd <- read_rwd_agg(file.path(rwprocess_dir,"rw_agg",rw_agg_nm)) 

zz <- rw_scen_aggregate(c(scen1,scen2),agg = rwd, scen_dir = scen_dir)#file = file.path(feather_data_dir,feather_file_nm))

#add scenario group 
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


slotnames <- unique(zz$Variable)
slotnames
i=7
for (i in 1:length(slotnames)) {
  df <- zz %>% filter(Variable == slotnames[i]) %>% 
    group_by(Variable,ScenarioGroup,Year) %>% 
    summarise(Value = mean(Value))  
  df<-df %>%pivot_wider(names_from = ScenarioGroup,values_from= Value)
  
  if(i == 1){
    dfcsv = df
  } else {
    dfcsv = rbind.data.frame(dfcsv,df)
  }
  # df1 <- df %>% filter(ScenarioGroup == unique(df$ScenarioGroup)[1]) 
  # df2 <- df %>% filter(ScenarioGroup == unique(df$ScenarioGroup)[2]) 
  
  # blws1 <- which(df1$Value > 0)
  # print(unique(df$ScenarioGroup)[1],df1$Year[blws1])
}
View(dfcsv)

# writexl::write_xlsx()
write.csv(dfcsv,file=file.path(results_dir,'BlwDeadPool.csv'))

#### =============== PLOT =============== ####
df <- read.csv(file=file.path(results_dir,'BlwDeadPool.csv'),header = T)
slotnames <- unique(df$Variable)
slotnames
plotnames<-c("Blue Mesa < 7393'","Fontenelle < 6460'","Flaming Gorge < 5871'",
             "Navajo < 5990'","Morrow Point < 7100'","Crystal < 6700'",
             "Powell < 3490'" )

df<- df %>% pivot_longer(cols = July.2021.no.DRO:July.2021.w.DRO, names_to= "ScenarioGroup",values_to= "Value")
# head(df)

df <- df %>%
  mutate(ScenarioGroup = case_when(
    ScenarioGroup %in% "July.2021.w.DRO" ~ "July 2021 w DRO",
    ScenarioGroup %in% "July.2021.no.DRO" ~ "July 2021 no DRO", 
    TRUE ~ "BAD"))
unique(df$ScenarioGroup)

ww <- 10
hh <- 7
start_yr <- 2022

pdf(file.path(figures_dir,'riskofbelowdead.pdf'))


for(j in 1:length(slotnames)){
  p<-df %>% filter(Variable == slotnames[j]) %>%
    group_by(ScenarioGroup) %>% 
    filter(Year >= start_yr) %>%
    ggplot(aes(x=Year,y=Value,color = ScenarioGroup)) + 
    geom_line() +
    scale_y_continuous("Percent Trace Below Dead Pool Any Month",labels = scales::percent,
                       limits = c(0,1)) +
    theme(plot.caption = element_text(hjust = 0)) #left justify 
  p <- p + labs(title = paste(plotnames[j])) 
  ggsave(p,filename = file.path(figures_dir,paste0(slotnames[j],".png")), width = ww,height = hh)#width= width, height= height)
  print(p)
  
  
}

dev.off()



####### feather file, no process 

xx <- feather::read_feather(file.path(feather_data_dir,"crspopsdata.feather"))
unique(xx$Variable)


xx %>% filter(Variable == "FlamingGorge.PE") %>%
  group_by(TraceNumber, ScenarioGroup) %>% 
  mutate(BlwDead = ifelse(Value<=5890,1,0)) %>%
  group_by(ScenarioGroup,Year) %>% 
  summarise(BlwDead = mean(BlwDead))  %>%
  ggplot(aes(x=Year,y=BlwDead,color = ScenarioGroup)) + 
  geom_line() +
  scale_y_continuous("Percent Trace Below Dead Pool Any Month",labels = scales::percent,
                     limits = c(0,1)) +
  theme(plot.caption = element_text(hjust = 0)) + 
  labs(title = "Flaming Gorge < 5890'") 
ggsave(filename = file.path(figures_dir,"FG5890Risk.png"), width = ww,height = hh)#width= width, height= height)

