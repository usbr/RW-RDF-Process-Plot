# create cloud plots of FY/WY data w/ CRMMS Ensemble w/ Hist - June 2021 CRSS Offc 
# powell 

library(tidyverse)
library(zoo)
library(RWDataPlyr)
library(lubridate)
source("code/get_mtom_data.R")
source("code/plot_data.R")
library(crssplot)
library(rhdb)
library(feather)

CRSSDIR <- Sys.getenv("CRSS_DIR")
# CRSSDIR <- "C:/Users/cfelletter/Documents/crss.offc"
# CRSSDIR <- "C:/Users/cfelletter/Documents/crss.trirvw2020"

# where scenarios are folder are kept
scen_dir <- file.path(CRSSDIR,"Scenario")

mtom_res_file <- "C:/Users/cfelletter/Documents/MTOM/Output Data/CRMMS_EnsembleOutput.xlsm" #my PC
# mtom_res_file <- "C:/Users/fellette/Documents/GIT/crmms/Output Data/CRMMS_EnsembleOutput.xlsm" #BA

# get CRMMS start/end data 
startcrmms <- ymd("2022-05-01")
endcrmms <- ymd("2021-12-31")

#plot_years <- 2019:2025
#nrg_w <- 9.5
#nrg_h <- 6.5

# dnf_base <- "Jun2021_2022,DNF,2016Dems,IG_DCP"
st_base <- "Jan2022_2023,ISM1988_2019,2016Dems,IG_DCPnoUBDRO"

results_dir <- file.path(CRSSDIR,'results',st_base) 
if (!file.exists(results_dir)) {
  message(paste('Creating folder:', results_dir))
  dir.create(results_dir)
}

# dnf_scens <- rw_scen_gen_names(dnf_base, paste0("Trace", sprintf("%02d", 4:38)))
st_scens <- rw_scen_gen_names(st_base, paste0("Trace", sprintf("%02d", 4:33)))
#st_scens <- c("st")

date_to_wy <- function(x) {
  mm <- month(x)
  yy <- year(x)
  
  yy[mm >= 10] <- yy[mm >= 10] + 1
  
  yy
}

# data ---------------------------------------
# get CRSS data (Alan's get_scenario_data.R)
if (FALSE){
  library(RWDataPlyr)
  
  #Powell
  rwa <- rwd_agg(data.frame(
    file = "xtraRes.rdf",
    slot = c("Powell.Energy"),
    period = "asis",
    summary = NA,
    eval = NA,
    t_s = NA,
    variable = c("powell"),
    stringsAsFactors = FALSE
  ))
  
  rw_scen_aggregate(c(st_scens), #dnf_scens
                    rwa, 
                    scen_dir = "//manoa.colorado.edu/BOR/Shared/CRSS/2022/Scenario",
                    # scen_dir = "M:/Shared/CRSS/2022/Scenario", 
                    file = "powellenergy.feather")
  
  #Mead-Moh-Hav
  rwa <- rwd_agg(data.frame(
    file = "LBEnergy.rdf",
    slot = c("Mohave.Energy", "Havasu.Energy", "Mead.Energy"),
    period = "asis",
    summary = NA,
    eval = NA,
    t_s = NA,
    variable = c("davis", "parker", "mead"),
    stringsAsFactors = FALSE
  ))
  
  rw_scen_aggregate(c(st_scens), rwa, 
                    scen_dir = "//manoa.colorado.edu/BOR/Shared/CRSS/2022/Scenario",
                    # scen_dir = "M:/Shared/CRSS/2022/Scenario", 
                    file = "mpd_energy.feather")
}

#the default feather file location is getwd() 
powell <- read_feather(file.path("powellenergy.feather"))
lbres <- read_feather(file.path("mpd_energy.feather"))

crss <- rbind.data.frame(powell,lbres) %>%
  mutate(Variable = paste(Variable, "energy", sep = "_"))


# CRMMS months to combine with crss
powell_ond2021 <- get_mtom_ond(mtom_res_file, "Powell.Energy", startcrmms, endcrmms)
mead_ond2021 <- get_mtom_ond(mtom_res_file, "Mead.Energy", startcrmms, endcrmms)
parker_ond2021 <- get_mtom_ond(mtom_res_file, "Havasu.Energy", startcrmms, endcrmms)
davis_ond2021 <- get_mtom_ond(mtom_res_file, "Mohave.Energy", startcrmms, endcrmms)


###### development not complete below this, still need to intergrate Pow & LB Res codes
# I stopped because now we are only using crmms, see allLBres_cloud_energy_plots_CRMMSonly_whist.R


# and expand it to have correct scenarios and number of traces
crss_ond <- bind_rows(
  expand_ond_to_crss_scenarios(
    mead_ond2021, 114, dnf_base
  ) %>%
    mutate(water_year = date_to_wy(Date), Variable = "mead_energy"),
  expand_ond_to_crss_scenarios(
    mead_ond2021, 32, st_base
  ) %>%
    mutate(water_year = date_to_wy(Date), Variable = "mead_energy")#,
  # expand_ond_to_crss_scenarios(
  #   parker_ond2021, 114, dnf_base
  # ) %>%
  #   mutate(water_year = date_to_wy(Date), Variable = "parker_energy"),
  # expand_ond_to_crss_scenarios(
  #   parker_ond2021, 32, st_base
  # ) %>%
  #   mutate(water_year = date_to_wy(Date), Variable = "parker_energy"),
  # expand_ond_to_crss_scenarios(
  #   davis_ond2021, 114, dnf_base
  # ) %>%
  #   mutate(water_year = date_to_wy(Date), Variable = "davis_energy"),
  # expand_ond_to_crss_scenarios(
  #   davis_ond2021, 32, st_base
  # ) %>%
  #   mutate(water_year = date_to_wy(Date), Variable = "davis_energy")
)

crss <- crss %>%
  mutate(Date = ymd(paste(Year, match(Month, month.name), "01", sep = "-")),
         water_year = date_to_wy(Date)) %>%
  select(-Year, -Month)

crss <- bind_rows(crss, crss_ond) %>%
  mutate(ScenarioGroup = case_when(
    Scenario %in% dnf_scens ~ "Full Hydrology",
    Scenario %in% st_scens ~ "Stress Test Hydrology",
    TRUE ~ "bad"
  ))

# Historical
ssids <- c("davis_energy" = 2071, "parker_energy" = 2072, "mead_energy" = 2070)
hdata <- hdb_query(ssids, "lc", "m", "1999-10", "2021-5")

# add variables and split off october 2020 - May 2021 and convert from MHW to 
# GWH
hdata <- hdata %>% 
  rename(Value = value) %>%
  mutate(Variable = names(ssids)[match(sdi, ssids)], Value = Value / 1000) %>%
  mutate(time_step = parse_date_time(time_step, "mdy HMS"), 
         water_year = date_to_wy(time_step))

h2021 <- filter(hdata, time_step >= ymd("2020-10-01")) %>%
  rename(Date = time_step)
hdata <- filter(hdata, time_step < ymd("2020-10-01"))

# MTOM June 2021 - Sep 2021
mtom21 <- bind_rows(
  get_mtom_data(mtom_res_file, "Mead.Energy", ymd("2021-09-01"), "mead_energy"),
  get_mtom_data(mtom_res_file, "Mohave.Energy", ymd("2021-09-01"), "davis_energy"),
  get_mtom_data(mtom_res_file, "Havasu.Energy", ymd("2021-09-01"), "parker_energy")
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

hdata <- hdata %>% 
  group_by(Variable, water_year) %>%
  summarise(Value = sum(Value)) %>%
  rename(Year = water_year)

avg_hist <- hdata %>%
  group_by(Variable) %>%
  summarise(Value = mean(Value))

# plot individual -------------------------
custom_colors <- c("Full Hydrology" = "#138d75", 
                   "Stress Test Hydrology" = "#f1c40f")


custom_cloud <- function(x, h, vv, tt, ss = 1.02) {
  
  h=h%>%filter(Year >= 2015)
  
  scens_plot_cloud(
    x, 
    vars = vv,
    historical = filter(h, Variable == vv) %>% ungroup() %>% select(-Variable),
    years = 2015:2031,
    plot_colors = custom_colors,
    title = paste(tt, "FY/WY Energy"), y_lab = "GWH",
    legend_wrap = 15
  ) +
    labs(x = "FY/WY") +
    geom_hline(yintercept = filter(avg_hist, Variable == vv)$Value,
               linetype = 2) +
    annotate(
      "text", 
      x = mean(2015:2031), 
      y = filter(avg_hist, Variable == vv)$Value * ss,
      label = "2000 - 2020 average")
}

gg_m <- custom_cloud(zz, hdata, "mead_energy", "Hoover") +
  labs(caption = "June 2021 CRSS")
gg_p <- custom_cloud(zz, hdata, "parker_energy", "Parker", 1.008) +
  labs(caption = "June 2021 CRSS")
gg_d <- custom_cloud(zz, hdata, "davis_energy", "Davis", 1.008) + 
  labs(caption = "June 2021 CRSS")

# parker davis together -------------------
pd <- filter(zz, Variable %in% c("davis_energy", "parker_energy")) %>%
  group_by(Year, TraceNumber, ScenarioGroup, Scenario) %>%
  summarise(Value = sum(Value)) %>%
  mutate(Variable = "pd_energy")

pdh <- filter(hdata, Variable %in% c("davis_energy", "parker_energy")) %>%
  group_by(Year) %>%
  summarise(Value = sum(Value)) %>%
  mutate(Variable = "pd_energy")

avg_hist <- bind_rows(
  avg_hist, 
  data.frame("Variable" = "pd_energy", Value = mean(pdh$Value))
)

gg_pd <- custom_cloud(pd, pdh, "pd_energy", "Parker-Davis", 1.008)

ww <- 10
hh <- 7

ggsave("hoover.png", gg_m, width = ww, height = hh)
ggsave("figures/davis.png", gg_d, width = ww, height = hh)
ggsave("figures/parker.png", gg_p, width = ww, height = hh)
ggsave("figures/parker_davis.png", gg_pd, width = ww, height = hh)

