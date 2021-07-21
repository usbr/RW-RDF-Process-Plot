library(tidyverse)
library(CRSSIO)
library(RWDataPlyr)

# •	What are the frequency and duration of minimum power pool (MPP) at Lake Powell? 
#   o	Frequency – X number of traces are below MPP in a given year – see slide ‘Risk of Lake Powell falling below 3490’
# o	Duration – Analyze the traces below MPP to understand the duration of the events.  Which duration is most frequent.  
# •	What is total CRSP generation during the those traces when Lake Powell is below MPP.  
# •	Are there traces where other CRSP reservoirs fall below their respective minimum power pools?  
#   o	If Yes we would probably want to research these further

xx <- feather::read_feather("C:/alan/CRSS/CRSS.2021/results/Jun2021/tempData/june.feather") %>% 
  filter(Year <= 2036)

mtom <- paste0("Trace", sprintf('%02d', 4:38))
mtom2 <- paste0("Trace", 4:38)   

jun_dnf <- rw_scen_gen_names("Jun2021_2022,DNF,2016Dems,IG_DCP", mtom)
jun_st <- rw_scen_gen_names("Jun2021_2022,ISM1988_2019,2016Dems,IG_DCP", mtom)
jun_dnf_nodo <- rw_scen_gen_names("Jun2021_2022,DNF,2016Dems,IG_DCPnoUBDRO", mtom2)
jun_st_nodo <- rw_scen_gen_names("Jun2021_2022,ISM1988_2019,2016Dems,IG_DCPnoUBDRO",
                                 mtom2)

dnf_2021dro <- rw_scen_gen_names('Jun2021_2022,DNF,2016Dems,IG_DCP_2021DRO', paste0('Trace', sprintf('%02d', 4:38)))
st_2021dro <- rw_scen_gen_names('Jun2021_2022,ISM1988_2019,2016Dems,IG_DCP_2021DRO', paste0('Trace', sprintf('%02d', 4:38)))
dnf_2021dro_nodo <- rw_scen_gen_names('Jun2021_2022,DNF,2016Dems,IG_DCPnoUBDRO_2021DRO', paste0('Trace', sprintf('%02d',4:38)))
st_2021dro_nodo <- rw_scen_gen_names('Jun2021_2022,ISM1988_2019,2016Dems,IG_DCPnoUBDRO_2021DRO', paste0('Trace', sprintf('%02d',4:38)))


xx <- mutate(xx, ScenarioGroup = case_when(
  Scenario %in% jun_dnf ~ "DNF - no 2107 DROA delivery - w/future DRO",
  Scenario %in% jun_st ~ "ST - no 2107 DROA delivery - w/future DRO",
  Scenario %in% jun_dnf_nodo ~ "DNF - no 2107 DROA delivery - no future DRO",
  Scenario %in% jun_st_nodo ~ "ST - no 2107 DROA delivery - no future DRO",
  Scenario %in% dnf_2021dro ~ "DNF - 2107 DROA delivery - w/future DRO",
  Scenario %in% st_2021dro ~ "ST - 2107 DROA delivery - w/future DRO",
  Scenario %in% dnf_2021dro_nodo ~ "DNF - 2107 DROA delivery - no future DRO",
  Scenario %in% st_2021dro_nodo ~ "ST - 2107 DROA delivery - no future DRO",
  TRUE ~ "BAD"
))

v3490 <- elevation_to_storage(3490, "powell")
scen_order <- c("ST - no 2107 DROA delivery - no future DRO",
                "ST - 2107 DROA delivery - no future DRO",
                "DNF - no 2107 DROA delivery - no future DRO",
                "DNF - 2107 DROA delivery - no future DRO")

tmp <- filter(xx, Variable == "powell_pe", Year == 2022, Month %in% month.name[1:9]) %>%
  group_by(TraceNumber, ScenarioGroup, Scenario) %>%
  summarise(Value = min(Value)) %>%
  filter(Value <= 3490) %>%
  mutate(
    storage = elevation_to_storage(Value, "powell"),
    deficit = v3490 - storage,
    ScenarioGroup = factor(ScenarioGroup, levels = scen_order)
  ) 

tmp_n <- group_by(tmp, ScenarioGroup) %>% count() %>%
  mutate(p = if_else(str_detect(ScenarioGroup, "ST"), n / (35*32), n / (35 * 114))) %>%
  mutate(p = paste(formatC(round(p * 100, 1), digits = 1, format = "f"), "%")) %>%
  mutate(n = paste("N =", n))

ggplot(tmp, aes(ScenarioGroup, deficit)) + 
  geom_boxplot() +
  geom_point(position = "jitter", color = "grey70") +
  scale_x_discrete(labels = function(x){str_wrap(x, width = 20)}) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, scale = 1/1000)) +
  labs(x = "Scenario", y = "volume (kaf)", 
       title = "Maximum volume below 3,490' at Powell in WY2022") +
  geom_label(data = tmp_n, aes(x = ScenarioGroup, y = 1300000, label = p))


tmp <- filter(xx, Variable == "powell_pe", ScenarioGroup %in% scen_order) %>%
  mutate(water_year = if_else(Month %in% month.name[10:12], Year + 1, Year)) %>%
  filter(water_year == 2023) %>%
  group_by(TraceNumber, ScenarioGroup, Scenario) %>%
  summarise(Value = min(Value)) %>%
  filter(Value <= 3490) %>%
  mutate(
    storage = elevation_to_storage(Value, "powell"),
    deficit = v3490 - storage,
    ScenarioGroup = factor(ScenarioGroup, levels = scen_order)
  ) 

tmp_n <- group_by(tmp, ScenarioGroup) %>% count() %>%
  mutate(p = if_else(str_detect(ScenarioGroup, "ST"), n / (35*32), n / (35 * 114))) %>%
  mutate(p = paste(formatC(round(p * 100, 1), digits = 1, format = "f"), "%")) %>%
  mutate(n = paste("N =", n))

ggplot(tmp, aes(ScenarioGroup, deficit)) + 
  geom_boxplot() +
  geom_point(position = "jitter", color = "grey70") +
  scale_x_discrete(labels = function(x){str_wrap(x, width = 20)}) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1, scale = 1/1000)) +
  labs(x = "Scenario", y = "volume (kaf)", 
       title = "Maximum volume below 3,490' at Powell in WY2023") +
  geom_label(data = tmp_n, aes(x = ScenarioGroup, y = 1300000, label = p))


