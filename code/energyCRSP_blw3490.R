# What is total CRSP generation during those traces when Lake Powell is below MPP?  
rm(list=ls())

#### =============== INPUTS =============== ####

results_nm <- "Jul2021_MostPowerRun" #results dir folder 

onBA <- FALSE # which computer BA or my PC? find RW-RDF-Process-Plot dir 
if (onBA == TRUE) {
  rwprocess_dir <- "C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot"
} else {
  rwprocess_dir <- "C:/Users/cfelletter/Documents/RW-RDF-Process-Plot"
}

#libraries and setup directories 
library(CRSSIO) #elevation_to_storage()
source(file.path(rwprocess_dir,"code","libs_n_dirs.R")) 
# scen_dir = #OVERWRITE if not manoa/Shared/CRSS/2021/Scenario 

ww <- 6
hh <- 7
start_yr <- 2022

# feather_file_nm <- "MeadPowellPE.feather"
  
xx <- feather::read_feather(file.path(feather_data_dir,"crspopsdata.feather"))
xx <- filter(xx, Variable == "Powell.PE")
xenergy <- feather::read_feather(file.path(feather_data_dir,"CRSPPowerData.feather"))  

df <- rbind.data.frame(xx,xenergy)
df <- df %>%pivot_wider(names_from= Variable,values_from= Value) %>% 
  # filter(Powell.PE <= 3490) %>%
  mutate(CRSP.Energy = Powell.Energy+ BlueMesa.Energy+Crystal.Energy+FlamingGorge.Energy+Fontenelle.Energy+MorrowPoint.Energy)

head(df)
View(df)
 
# df %>%pivot_longer(cols = July.2021.no.DRO:July.2021.w.DRO, names_to= "ScenarioGroup",values_to= "Value")


# pdf(file.path(figures_dir,'defvol.pdf'))


#CRSP Energy Generation when Powell < 3490  
df %>% filter(Year <= 2026,Powell.PE <= 3490) %>%
    dplyr::group_by(Year) %>%
    ggplot(aes(x=factor(Year),y=CRSP.Energy,color=ScenarioGroup)) +
    geom_boxplot() +
  ylim(0,1000) +
  labs(x = "Year", y = "Energy (GWH/mo)" ,title = "CRSP Energy Generation when Powell < 3490") 
ggsave(filename = file.path(figures_dir,paste0("CRSPEnergyPow3490.png")), width = ww,height = hh)#width= width, height= height)

# CRSP Energy Generation All Traces
df %>% filter(Year <= 2026) %>%
  dplyr::group_by(Year) %>%
  ggplot(aes(x=factor(Year),y=CRSP.Energy,color=ScenarioGroup)) +
  geom_boxplot() +
  ylim(0,1000) +
  labs(x = "Year", y = "Energy (GWH/mo)" ,title = "CRSP Energy Generation All Traces") 
ggsave(filename = file.path(figures_dir,paste0("CRSPEnergyAllTraces.png")), width = ww,height = hh)


#report out numbers
#CRSP Energy Generation when Powell < 3490  
dat <- df %>% filter(Year <= 2026,Powell.PE <= 3490) %>%
  dplyr::group_by(ScenarioGroup, Year) %>%
  summarise(Med = median(CRSP.Energy),Mean = mean(CRSP.Energy),Max = max(CRSP.Energy),Min = min(CRSP.Energy))
write.csv(dat,file = file.path(figures_dir,paste0("CRSPEnergyPow3490.csv")))

# CRSP Energy Generation All Traces
dat <- df %>% filter(Year <= 2026) %>%
  dplyr::group_by(ScenarioGroup, Year) %>%
  summarise(Med = median(CRSP.Energy),Mean = mean(CRSP.Energy),Max = max(CRSP.Energy),Min = min(CRSP.Energy))
write.csv(dat,file = file.path(figures_dir,paste0("CRSPEnergyAllTraces.csv")))

