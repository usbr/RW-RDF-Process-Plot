# powell deficit volume by WY
# CF July 2021 - for Power Office 3490 analysis
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

ww <- 10
ww_narrow <-2.5 #for multiple graphs 
hh <- 7
start_yr <- 2022
# end_yr <- 2030



feather_file_nm <- "crspopsdata.feather"
# feather_file_nm <- "CRSPPowerData.feather"
# feather_file_nm <- "MeadPowellPE.feather"
  

xx <- feather::read_feather(file.path(feather_data_dir,feather_file_nm))  
unique(xx$Variable)
xx <- xx %>%
  mutate(ScenarioGroup = case_when(
    ScenarioGroup %in% "July 2021 w DRO" ~ "wDRO",
    ScenarioGroup %in% "July 2021 no DRO" ~ "noDRO", 
    TRUE ~ "BAD"))
unique(xx$ScenarioGroup)


head(xx)
v3490 <- elevation_to_storage(3490, "powell")
scen_order <- unique(xx$ScenarioGroup)

############ Volume ################


tmp <- filter(xx, Variable == "Powell.PE", Year == 2022, Month %in% month.name[1:9]) %>%
# tmp <- filter(xx, Variable == "Powell.PE",Year == yrs[i]) %>%
  group_by(TraceNumber, ScenarioGroup) %>%
  summarise(Value = min(Value)) %>%
  filter(Value <= 3490) %>%
  mutate(
    storage = elevation_to_storage(Value, "powell"),
    deficit = v3490 - storage,
    ScenarioGroup = factor(ScenarioGroup))#, levels = scen_order)
head(tmp)
#only Trace 15 July 2021 no DRO 3472. deficit = 855861.

# tmp_n <- group_by(tmp, ScenarioGroup) %>% count() %>%
#   mutate(p = if_else(str_detect(ScenarioGroup, "ST"), n / (35*32), n / (35 * 114))) %>%
#   mutate(p = paste(formatC(round(p * 100, 1), digits = 1, format = "f"), "%")) %>%
#   mutate(n = paste("N =", n))
# 
# ggplot(tmp, aes(ScenarioGroup, deficit)) + 
#   geom_boxplot() +
#   geom_point(position = "jitter", color = "grey70") +
#   scale_x_discrete(labels = function(x){str_wrap(x, width = 20)}) +
#   scale_y_continuous(labels = scales::comma_format(accuracy = 1, scale = 1/1000)) +
#   labs(x = "Scenario", y = "volume (kaf)", 
#        title = "Maximum volume below 3,490' at Powell in WY2022") +
#   geom_label(data = tmp_n, aes(x = ScenarioGroup, y = 1300000, label = p))

pdf(file.path(figures_dir,'defvol.pdf'))

for (i in 2023:2026) {
  
  tmp <- filter(xx, Variable == "Powell.PE", ScenarioGroup %in% scen_order) %>%
    mutate(water_year = if_else(Month %in% month.name[10:12], Year + 1, Year)) %>%
    filter(water_year == i) %>%
    group_by(TraceNumber, ScenarioGroup, Scenario) %>%
    summarise(Value = min(Value)) %>%
    filter(Value <= 3490) %>%
    mutate(
      storage = elevation_to_storage(Value, "powell"),
      deficit = v3490 - storage,
      ScenarioGroup = factor(ScenarioGroup, levels = scen_order)
    ) 
  
  tmp_n <- group_by(tmp, ScenarioGroup) %>% count() %>%
    # mutate(p = if_else(str_detect(ScenarioGroup, "ST"), n / (35*32), n / (35 * 114))) %>%
    mutate(p = n / 32) %>% #only 1 IC 
    mutate(p = paste(formatC(round(p * 100, 1), digits = 1, format = "f"), "%")) %>%
    mutate(n = paste("N =", n))
  
  pp <- ggplot(tmp, aes(ScenarioGroup, deficit)) + 
    geom_boxplot() +
    geom_point(position = "jitter", color = "grey70") +
    scale_x_discrete(labels = function(x){str_wrap(x, width = 20)}) +
    scale_y_continuous(labels = scales::comma_format(accuracy = 1, scale = 1/1000)) +
    labs(y = "volume (kaf)", 
         x = "",#"Scenario",
         title = paste0("WY",i)) +
         # title = paste("Maximum volume below 3,490' at Powell in WY",i)) +
    geom_label(data = tmp_n, aes(x = ScenarioGroup, y = 1300000, label = p),position = position_stack(vjust = 0.5))
  
  print(pp)
  ggsave(pp,filename = file.path(figures_dir,paste0("DefVol",i,".png")), width = ww_narrow,height = hh)#width= width, height= height)
  
} 

dev.off()

