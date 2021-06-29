# create cloud plots of FY/WY data
library(tidyverse)
library(zoo)
library(RWDataPlyr)
library(lubridate)
library(crssplot)
# remotes::install_github("BoulderCodeHub/rhdb") #need github PAT env var set
library(rhdb) 
library(feather)

setwd('code') ### this all needs to be placed in the CRSS folder 
source("C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/code/get_mtom_data.R")
source("C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/code/plot_data.R")

# mtom_res_file <- "C:/Users/cfelletter/Documents/MTOM/Output Data/CRMMS_EnsembleOutput.xlsm" #my PC
mtom_res_file <- "C:/Users/fellette/Documents/GIT/crmms/Output Data/CRMMS_EnsembleOutput.xlsm" #BA

#plot_years <- 2019:2025
#nrg_w <- 9.5
#nrg_h <- 6.5

dnf_name <- 'Jun2021_2022,DNF,2016Dems,IG_DCP'
st_name <- 'Jun2021_2022,ISM1988_2019,2016Dems,IG_DCP'
dnf_scens <- rw_scen_gen_names(dnf_name,  paste0('Trace', 4:38))
st_scens <- rw_scen_gen_names(st_name,  paste0('Trace', 4:38))

#st_scens <- c("st")

date_to_wy <- function(x) {
  mm <- month(x)
  yy <- year(x)
  
  yy[mm >= 10] <- yy[mm >= 10] + 1
  
  yy
}

# data ---------------------------------------
# CRSS

crss <- zz #from Process_CRSS_crsp_energy.feather.R OR... 
crss <- bind_rows(
  read_feather(file.path(CRSSDIR,"crsp_energy.feather"))
) 

slotnames <- c("Powell.Energy", "BlueMesa.Energy","Crystal.Energy","FlamingGorge.Energy","Fontenelle.Energy","MorrowPoint.Energy" )

# MTOM - OND (Oct,Nov,Dec) 2021 to combine with crss
powell_ond2021 <- get_mtom_ond(mtom_res_file, slotnames[1], ymd("2021-10-01"), ymd("2021-12-31")) 
crystal_ond2021 <- get_mtom_ond(mtom_res_file, slotnames[3], ymd("2021-10-01"), ymd("2021-12-31")) 
fontenelle_ond2021 <- get_mtom_ond(mtom_res_file, slotnames[5], ymd("2021-10-01"), ymd("2021-12-31")) 
morrow_point_ond2021 <- get_mtom_ond(mtom_res_file, slotnames[6], ymd("2021-10-01"), ymd("2021-12-31")) 
blue_mesa_ond2021 <- get_mtom_ond(mtom_res_file, slotnames[2], ymd("2021-10-01"), ymd("2021-12-31"))
flaming_gorge_ond2021 <- get_mtom_ond(mtom_res_file, slotnames[4], ymd("2021-10-01"), ymd("2021-12-31"))

# and expand it to have correct scenarios and number of traces
crss_ond <- bind_rows(
  expand_ond_to_crss_scenarios(
    powell_ond2021, 114, dnf_name
  ) %>%
    mutate(water_year = date_to_wy(Date), Variable = "powell_energy"),
  expand_ond_to_crss_scenarios(
    powell_ond2021, 32, st_name
  ) %>%
    mutate(water_year = date_to_wy(Date), Variable = "powell_energy"),
  
  expand_ond_to_crss_scenarios(
    blue_mesa_ond2021, 114, dnf_name
  ) %>%
    mutate(water_year = date_to_wy(Date), Variable = "blue_mesa_energy"),
  expand_ond_to_crss_scenarios(
    blue_mesa_ond2021, 32, st_name
  ) %>%
    mutate(water_year = date_to_wy(Date), Variable = "blue_mesa_energy"),
  
  expand_ond_to_crss_scenarios(
    flaming_gorge_ond2021, 114, dnf_name
  ) %>%
    mutate(water_year = date_to_wy(Date), Variable = "flaming_gorge_energy"),
  expand_ond_to_crss_scenarios(
    flaming_gorge_ond2021, 32, st_name
  ) %>%
    mutate(water_year = date_to_wy(Date), Variable = "flaming_gorge_energy"),
  
  expand_ond_to_crss_scenarios(
    fontenelle_ond2021, 114, dnf_name
  ) %>%
    mutate(water_year = date_to_wy(Date), Variable = "fontenelle_energy"),
  expand_ond_to_crss_scenarios(
    fontenelle_ond2021, 32, st_name
  ) %>%
    mutate(water_year = date_to_wy(Date), Variable = "fontenelle_energy"),

    expand_ond_to_crss_scenarios(
      morrow_point_ond2021, 114, dnf_name
    ) %>%
    mutate(water_year = date_to_wy(Date), Variable = "morrow_point_energy"),
  expand_ond_to_crss_scenarios(
    morrow_point_ond2021, 32, st_name
  ) %>%
    mutate(water_year = date_to_wy(Date), Variable = "morrow_point_energy"),

  expand_ond_to_crss_scenarios(
    crystal_ond2021, 114, dnf_name
  ) %>%
    mutate(water_year = date_to_wy(Date), Variable = "crystal_energy"),
  expand_ond_to_crss_scenarios(
    crystal_ond2021, 32, st_name
  ) %>%
    mutate(water_year = date_to_wy(Date), Variable = "crystal_energy")

  )

crss <- crss %>%
  mutate(
    # Date = ymd(paste(Year, match(Month, month.name), "01", sep = "-")), #already have col date, don't do this? 
         water_year = date_to_wy(Date)) #%>%
  # select(-Year, -Month) #### why not have? Must be how I processed feather 

# crss <- bind_rows(crss, crss_ond) %>% #already have scengroup for crss from Process_CRSS_crsp_energy.feather.R 
crss <- crss_ond %>%
  mutate(ScenarioGroup = case_when(
    Scenario %in% dnf_scens ~ "June 2021 - DNF IG", 
    Scenario %in% st_scens ~ "June 2021 - ST IG", 
    TRUE ~ "bad"
  ))

crss <- bind_rows(crss, crss_ond) 

# Historical
ssids <- c("flaming_gorge_energy" = 2266, "blue_mesa_energy" = 2265, "powell_energy" = 2305,
           "crystal_energy"  = 2270, "fontenelle_energy" = 2267, "morrow_point_energy" = 2268)
hdata <- hdb_query(ssids, "uc", "m", "1999-10", "2021-4")
summary(hdata)

# hdata <- read.csv('hist.csv') #old hack way 
# hdata <- hdata %>%
# mutate(time_step = ymd(Date), ###csv hack 
#        Date = ymd(Date),
#      water_year = date_to_wy(time_step))

# # add variables and split off october 2020 - April 2021 and convert from MHW to 
# # GWH

hdata <- hdata %>%
  rename(Value = value) %>%
  mutate(Variable = names(ssids)[match(sdi, ssids)], Value = Value / 1000) %>%
  mutate(time_step = parse_date_time(time_step, "mdy HMS"),
  water_year = date_to_wy(time_step))
  

h2021 <- filter(hdata, time_step >= ymd("2020-10-01")) %>%
  rename(Date = time_step)
hdata <- filter(hdata, time_step < ymd("2020-10-01"))

# MTOM May 2021 - Sep 2021
mtom21 <- bind_rows(
  get_mtom_data(mtom_res_file, "Powell.Energy", ymd("2021-09-01"), "powell_energy"),
  
  get_mtom_data(mtom_res_file, "Fontenelle.Energy", ymd("2021-09-01"), "fontenelle_energy"),
  get_mtom_data(mtom_res_file, "Crystal.Energy", ymd("2021-09-01"), "crystal_energy"),
  get_mtom_data(mtom_res_file, "MorrowPoint.Energy", ymd("2021-09-01"), "morrow_point_energy"),
  
  get_mtom_data(mtom_res_file, "BlueMesa.Energy", ymd("2021-09-01"), "flaming_gorge_energy"),
  
  get_mtom_data(mtom_res_file, "FlamingGorge.Energy", ymd("2021-09-01"), "blue_mesa_energy")
) %>%
  mutate(water_year = date_to_wy(Date)) %>%
  filter(Date != ymd("2021-04-01"))

# MTOM for 2021 + historical dat for 2021
mtom21 <- bind_rows(lapply(4:38, function(i) {
  tmp <- h2021
  tmp$Trace = paste0("Trace", sprintf("%02d", i))
  tmp
})) %>%
  bind_rows(mtom21) %>%
  mutate(Trace = as.numeric(stringr::str_remove(Trace, "Trace")))

mtom21 <- bind_rows(
  mutate(mtom21, ScenarioGroup = "Full Hydrology"),
  mutate(mtom21, ScenarioGroup = "Stress Test Hydrology")
) %>%
  mutate(Scenario = ScenarioGroup) %>%
  select(-sdi, -mrid, -units) %>%
  rename(TraceNumber = Trace)

# plotting data ---------------------------
zz <- bind_rows(mtom21, crss) %>%
  group_by(Variable, water_year, TraceNumber, ScenarioGroup, Scenario) %>%
  summarise(Value = sum(Value)) %>%
  rename(Year = water_year)

# zz22 <- zz  %>% filter(Year > 2021)
  
hdata <- hdata %>%
  group_by(Variable, water_year) %>%
  summarise(Value = sum(Value)) %>%
  rename(Year = water_year)

avg_hist <- hdata %>%
  group_by(Variable) %>%
  summarise(Value = mean(Value,na.rm = T))

# plot individual -------------------------
custom_colors <- c("Full Hydrology" = "#138d75", 
                   "Stress Test Hydrology" = "#f1c40f")

ww <- 10
hh <- 7

start_yr <- 2015
# end_yr <- 2030 

custom_cloud <- function(x, h, vv, tt, ss = 1.02) {
    
  scens_plot_cloud(
    x, 
    vars = vv,
    historical = filter(h, Variable == vv) %>% 
      # filter(h, Year >= start_yr) %>% 
      ungroup() %>% select(-Variable),
    years = 2000:2031,
    plot_colors = custom_colors,
    title = paste(tt, "FY/WY Energy"), y_lab = "GWH",
    legend_wrap = 15
  ) +
    labs(x = "FY/WY") +
    geom_hline(yintercept = filter(avg_hist, Variable == vv)$Value * 1.05,
               linetype = 2) +
    annotate(
      "text",
      x = mean(start_yr:2031),
      y = filter(avg_hist, Variable == vv)$Value * ss,
      label = paste0("2000 - 2020 average"))
}
  
### add a filter for hdata since some res have incomplete data   
h_filter <- hdata %>%
  filter(Year >= start_yr)  

pdf('CRSP_Energy_wHist.pdf')

custom_cloud(zz, h_filter, "fontenelle_energy", "Fontenelle",1.008)
ggsave(filename = paste0("fontenelle_energy","_cloud.png"), width = ww,height = hh)#width= width, height= height)

custom_cloud(zz, h_filter, "flaming_gorge_energy", "Flaming Gorge", 1.008)
ggsave(filename = paste0("flaming_gorge_energy","_cloud.png"), width = ww,height = hh)#width= width, height= height)

custom_cloud(zz, h_filter, "blue_mesa_energy", "Blue Mesa", 1.008)
ggsave(filename = paste0("blue_mesa_energy","_cloud.png"), width = ww,height = hh)#width= width, height= height)

custom_cloud(zz, h_filter, "morrow_point_energy", "Morrow Point", 1.008)
ggsave(filename = paste0("morrow_point_energy","_cloud.png"), width = ww,height = hh)#width= width, height= height)

custom_cloud(zz, h_filter, "crystal_energy", "Crystal", 1.008)
ggsave(filename = paste0("crystal_energy","_cloud.png"), width = ww,height = hh)#width= width, height= height)

custom_cloud(zz, h_filter, "powell_energy", "Powell",1.008)
ggsave(filename = paste0("powell_energy","_cloud.png"), width = ww,height = hh)#width= width, height= height)

dev.off()


### write stats to csv 
xx <- zz %>% dplyr::group_by(ScenarioGroup, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
             '10th' = quantile(Value,.1),'90th' = quantile(Value,.9),
             'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot

write.csv(xx,'CRSP_Energy_Stats.csv')#paste0(vars[i],'_',scengroups[j],'.csv') )
 

#### extra plots ####
# 
# gg_m <- custom_cloud(zz22, h_filter, "powell_energy", "Powell")
# gg_p <- custom_cloud(zz, h_filter, "blue_mesa_energy", "blue_mesa", 1.008)
# gg_d <- custom_cloud(zz, h_filter, "flaming_gorge_energy", "flaming_gorge", 1.008)
# 
# # blue_mesa flaming_gorge together -------------------
# pd <- filter(zz, Variable %in% c("flaming_gorge_energy", "blue_mesa_energy")) %>%
#   group_by(Year, TraceNumber, ScenarioGroup, Scenario) %>%
#   summarise(Value = sum(Value)) %>%
#   mutate(Variable = "pd_energy")
# 
# pdh <- filter(h_filter, Variable %in% c("flaming_gorge_energy", "blue_mesa_energy")) %>%
#   group_by(Year) %>%
#   summarise(Value = sum(Value)) %>%
#   mutate(Variable = "pd_energy")
# 
# avg_hist <- bind_rows(
#   avg_hist, 
#   data.frame("Variable" = "pd_energy", Value = mean(pdh$Value))
# )
# 
# gg_pd <- custom_cloud(pd, pdh, "pd_energy", "blue_mesa-flaming_gorge", 1.008)
# 
# ww <- 10
# hh <- 7
# 
# ggsave("figures/hoover.png", gg_m, width = ww, height = hh)
# ggsave("figures/flaming_gorge.png", gg_d, width = ww, height = hh)
# ggsave("figures/blue_mesa.png", gg_p, width = ww, height = hh)
# ggsave("figures/blue_mesa_flaming_gorge.png", gg_pd, width = ww, height = hh)
