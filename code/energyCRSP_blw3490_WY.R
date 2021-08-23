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

#### =============== PROCESS =============== ####

#get powell PE data and compute WY
crss_powPE <- feather::read_feather(file.path(feather_data_dir,"crspopsdata.feather"))
crss_powPE <- filter(crss_powPE, Variable == "Powell.PE") %>%
  mutate(
    Date = ymd(paste(Year, match(Month, month.name), "01", sep = "-")), 
    water_year = date_to_wy(Date)) %>%
  select(-Year, -Month) 

#crss energy 
crss_energy <- read_feather(file.path(feather_data_dir,"CRSPPowerData.feather")) 

unique(crss_energy$ScenarioGroup)

crss_energy <- crss_energy %>%
  mutate(
    Date = ymd(paste(Year, match(Month, month.name), "01", sep = "-")), 
    water_year = date_to_wy(Date)) %>%
  select(-Year, -Month) 
# head(crss_energy)

scengroups <- unique(crss_energy$ScenarioGroup)

# 24MS - OND (Oct,Nov,Dec) 2021 to combine with crss
ond2021_24MS <- readxl::read_xlsx(file.path(results_dir,"24MS_CRSP_Energy.xlsx")) #24MS results
# head(ond2021_24MS)
ond2021_24MS <- ond2021_24MS %>% 
  pivot_longer(!Date, names_to = "Variable",values_to = "Value") %>%
  mutate(water_year = date_to_wy(Date), Date = ymd(Date))

ond2021_24MS$Date = ymd(ond2021_24MS$Date)

# expand it to have correct scenarios and number of traces
# ond = Oct, Nov, Dec
ond21 <- bind_rows(
  mutate(ond2021_24MS, ScenarioGroup = scengroups[1]),
  mutate(ond2021_24MS, ScenarioGroup = scengroups[2])
)

for (i in 1:length(unique(crss_energy$TraceNumber))) {
  if (i == 1){
    df_ond =  mutate(ond21, TraceNumber = i)
  } else {
    df_ond =  bind_rows(df_ond,mutate(ond21, TraceNumber = i))
  }
}

#### MUST FILTER OUT 24MS after EOCY 2021!!!! ##### 
df_ond <- df_ond %>% filter(Date <= "2021-12-31")

head(df_ond)
unique(df_ond$water_year)
unique(df_ond$Date)

#combine crss energy with 24MS Oct-Dec energy 
energy_all <- bind_rows(crss_energy[,names(df_ond)], df_ond) #%>% # crss %>%

#combine all energy with powell PE
names(crss_powPE)
df <- rbind.data.frame(crss_powPE[,names(energy_all)],energy_all)
df <- df %>%pivot_wider(names_from= Variable,values_from= Value) %>% 
  # filter(Powell.PE <= 3490) %>%
  mutate(CRSP.Energy = Powell.Energy+ BlueMesa.Energy+Crystal.Energy+FlamingGorge.Energy+Fontenelle.Energy+MorrowPoint.Energy)

#### =============== PLOT =============== ####
# pdf(file.path(figures_dir,'defvol.pdf'))

#CRSP Energy Generation when Powell < 3490  
df %>% filter(water_year <= 2026,water_year >= 2022,Powell.PE <= 3490) %>%
  dplyr::group_by(water_year,ScenarioGroup,TraceNumber) %>%
  summarise(CRSP.Energy = sum(CRSP.Energy)) %>%
  # report out
  dplyr::group_by(water_year,ScenarioGroup) %>%
  summarise(mean = mean(CRSP.Energy),med = median(CRSP.Energy),max = max(CRSP.Energy),min = min(CRSP.Energy)) %>%
  write.csv(file = file.path(figures_dir,paste0("CRSPEnergy3490_WY_plotstats.csv")))
  # plot 
  ggplot(aes(x=factor(water_year),y=CRSP.Energy,color=ScenarioGroup)) +
  geom_boxplot() +
  ylim(0,7500) +
  labs(x = "WY Year", y = "Energy (GWH/yr)" ,title = "CRSP Energy Generation when Powell < 3490") 
ggsave(filename = file.path(figures_dir,paste0("CRSPEnergyPow3490_WY_2226.png")), width = ww,height = hh)#width= width, height= height)

# CRSP Energy Generation All Traces
df %>%   filter(water_year <= 2026,water_year >= 2022) %>%
  dplyr::group_by(water_year,ScenarioGroup,TraceNumber) %>%
  summarise(CRSP.Energy = sum(CRSP.Energy)) %>%
  ## report out
  # dplyr::group_by(water_year,ScenarioGroup) %>%
  # summarise(mean = mean(CRSP.Energy),med = median(CRSP.Energy),max = max(CRSP.Energy),min = min(CRSP.Energy)) %>%
  # write.csv(file = file.path(figures_dir,paste0("CRSPEnergy_WY_plotstats.csv")))
  # plot 
  ggplot(aes(x=factor(water_year),y=CRSP.Energy,color=ScenarioGroup)) +
  geom_boxplot() +
  ylim(0,7500) +
  labs(x = "WY Year", y = "Energy (GWH/yr)" ,title = "CRSP Energy Generation All Traces") 
ggsave(filename = file.path(figures_dir,paste0("CRSPEnergy_WY_alltraces_2226.png")), width = ww,height = hh)#width= width, height= height)

