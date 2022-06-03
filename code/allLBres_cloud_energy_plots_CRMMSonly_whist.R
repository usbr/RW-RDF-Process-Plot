# create cloud plots of FY/WY data from CRMMS Ensemble w/ Hist (no CRSS)

library(tidyverse)
library(zoo)
library(RWDataPlyr)
library(lubridate)
source("code/get_mtom_data.R")
source("code/plot_data.R")
library(crssplot)
library(rhdb)
library(feather)

mtom_res_file <- "C:/Users/cfelletter/Documents/MTOM/Output Data/CRMMS_EnsembleOutput.xlsm" #my PC
# mtom_res_file <- "C:/Users/fellette/Documents/GIT/crmms/Output Data/CRMMS_EnsembleOutput.xlsm" #BA

crmmstraces <- 4:33 #need for my updated get_mtom_data.R functions 

crmmsname <- "May2022_ESP"

# get CRMMS start/end data 
startcrmms <- ymd("2022-05-01")
endcrmms <- ymd("2027-09-30") #must end at EOWY
start_histwy <- ymd("2021-10-01") #data for first WY must go from Oct to CRMMS start 
end_first_crmms_wy <- ymd("2022-09-30") #data for first WY must go from Oct to CRMMS start 


endhist = "2022-4" #endhist = "2021-5"

#plot_years <- 2019:2025
#nrg_w <- 9.5
#nrg_h <- 6.5

date_to_wy <- function(x) {
  mm <- month(x)
  yy <- year(x)
  
  yy[mm >= 10] <- yy[mm >= 10] + 1
  
  yy
}

# CRMMS months to pull, the get_mtom_ond function was expanded to be any start-end date 
powell_ond2021 <- get_mtom_data(mtom_res_file, "Powell.Energy", endcrmms,crmmstraces)
mead_ond2021 <- get_mtom_data(mtom_res_file, "Mead.Energy", endcrmms,crmmstraces)
parker_ond2021 <- get_mtom_data(mtom_res_file, "Havasu.Energy", endcrmms,crmmstraces)
davis_ond2021 <- get_mtom_data(mtom_res_file, "Mohave.Energy", endcrmms,crmmstraces)

crmms <- bind_rows(powell_ond2021, mead_ond2021,parker_ond2021,davis_ond2021) %>%
  mutate(ScenarioGroup = crmmsname)

# Historical
ssids_pow <- c("powell_energy" = 2305)
ssids_lb <- c("davis_energy" = 2071, "parker_energy" = 2072, "mead_energy" = 2070)
ssids <- c(ssids_pow,ssids_lb)

hdatasave <- rbind.data.frame(
  hdb_query(ssids_pow, "uc", "m", "1999-10", endhist),
  hdb_query(ssids_lb, "lc", "m", "1999-10", endhist))

# add variables and split off october 2020 - May 2021 and convert from MHW to 
# GWH
hdata <- hdatasave %>% 
  rename(Value = value) %>%
  mutate(Variable = names(ssids)[match(sdi, ssids)], Value = Value / 1000) %>%
  mutate(time_step = parse_date_time(time_step, "mdy HMS"), 
         water_year = date_to_wy(time_step))

hdata$Date = hdata$time_step
unique(hdata$time_step)

#filter so you have hdata only before first WY and hdata_SOWY_to_CRMMSstart
hdata_SOWY_to_CRMMSstart <- filter(hdata, time_step >= start_histwy) 
hdata <- filter(hdata, time_step < start_histwy)

#check to make sure the data is what you want
head(hdata)
head(hdata_SOWY_to_CRMMSstart)
unique(hdata_SOWY_to_CRMMSstart$Date) #should be only WY mons < CRMMS
unique(hdata$Date) #everything else up to first WY of CRMMS data 

#### CRMMS EOWY data + historical SOWY data for first WY ####
first_WY_hist_astraces <- bind_rows(lapply(crmmstraces, function(i) {
  tmp <- hdata_SOWY_to_CRMMSstart
  tmp$Trace = paste0("Trace", sprintf("%02d", i))
  tmp
})) #%>%

first_WY_hist_astraces$Trace = first_WY_hist_astraces$TraceNumber
 
#   bind_rows(first_WY_hist_astraces) # %>% #CF: not sure what this was needed for

first_WY_hist_astraces <- mutate(first_WY_hist_astraces, ScenarioGroup = crmmsname) %>%
  mutate(Scenario = ScenarioGroup) %>%
  select(-sdi, -mrid, -units) %>%
  rename(TraceNumber = Trace)

first_WY_hist_astraces <- first_WY_hist_astraces  %>%
  mutate(water_year = date_to_wy(Date)) 

#new data col Trace that only has numeric trace # 
first_WY_hist_astraces <- first_WY_hist_astraces %>%
  mutate(Trace = as.numeric(stringr::str_remove(TraceNumber, "Trace")))

#### prep CRMMS > 1st WY data ####

crmms <- filter(crmms, Date >= startcrmms)  %>%
  mutate(water_year = date_to_wy(Date)) 

crmms$TraceNumber = crmms$Trace
#reformat Trace data col to be only numeric trace # 
crmms <- crmms %>%
  mutate(Trace = as.numeric(stringr::str_remove(Trace, "Trace")))

#check to make sure the data is what you want
# head(first_WY_hist_astraces)
# head(crmms)
unique(first_WY_hist_astraces$Date)
unique(crmms$Date) #everything else up to first WY of CRMMS data 
unique(first_WY_hist_astraces$Trace)
unique(crmms$Trace) #should be the same as above 
unique(first_WY_hist_astraces$Variable)
unique(crmms$Variable) #must get same names

crmms$properVarnm = crmms$Variable

#standard names
crmms <- crmms %>%
  mutate(Variable = case_when(
    properVarnm %in% "Powell.Energy" ~ "powell_energy",
    properVarnm %in% "Mead.Energy" ~ "mead_energy",
    properVarnm %in% "Havasu.Energy" ~ "parker_energy",
    properVarnm %in% "Mohave.Energy" ~ "davis_energy",
    TRUE ~ "bad"
  ))


# plotting data ---------------------------
zz <- bind_rows(first_WY_hist_astraces,crmms) %>%
  group_by(Variable, water_year, Trace, ScenarioGroup) %>%
  summarise(Value = sum(Value)) %>%
  rename(Year = water_year) %>%
  rename(TraceNumber = Trace)

#tests
unique(zz$Variable)
unique(zz$ScenarioGroup)
unique(zz$Year) #should be half hist WY to EOWY CRMMS run 
# summary(zz)

hdata_sumd <- hdata %>% 
  group_by(Variable, water_year) %>%
  summarise(Value = sum(Value)) %>%
  rename(Year = water_year)

avg_hist <- hdata_sumd %>%
  group_by(Variable) %>%
  summarise(Value = mean(Value))

unique(hdata_sumd$Variable)

# plot individual -------------------------
warning(paste(crmmsname,"must be manually entered into the custom_colors"))
custom_colors <- c("May2022_ESP" = "#138d75")
                   #"Stress Test Hydrology" = "#f1c40f")

years <- 2015:2027

custom_cloud <- function(x, h, vv, tt, ss = 1.02) {
  
  h=h%>%filter(Year >= 2015)
  
  scens_plot_cloud(
    x, 
    vars = vv,
    historical = filter(h, Variable == vv) %>% ungroup() %>% select(-Variable),
    years = years,
    plot_colors = custom_colors,
    title = paste(tt, "FY/WY Energy"), y_lab = "GWH",
    legend_wrap = 15
  ) +
    labs(x = "FY/WY") +
    geom_hline(yintercept = filter(avg_hist, Variable == vv)$Value,
               linetype = 2) +
    annotate(
      "text", 
      x = mean(years), 
      y = filter(avg_hist, Variable == vv)$Value * ss,
      label = "2000 - 2020 average")
}

# write.csv(x = dput(zz),file = "dput(zz).csv")
# write.csv(x = dput(hdata_sumd),file = "dput(hdata).csv")

custom_cloud(zz, hdata_sumd, "mead_energy", "Hoover")
custom_cloud(zz, hdata_sumd, "powell_energy", "Glen")

custom_caption = "May 2022 CRMMS"

gg_g <- custom_cloud(zz, hdata_sumd, "powell_energy", "Glen") +
  labs(caption = custom_caption)
gg_m <- custom_cloud(zz, hdata_sumd, "mead_energy", "Hoover") +
  labs(caption = custom_caption)
gg_p <- custom_cloud(zz, hdata_sumd, "parker_energy", "Parker", 1.004) +
  labs(caption = custom_caption)
gg_d <- custom_cloud(zz, hdata_sumd, "davis_energy", "Davis", 1.008) + 
  labs(caption = custom_caption)

gg_p
gg_d

# # parker davis together -------------------
# pd <- filter(zz, Variable %in% c("davis_energy", "parker_energy")) %>%
#   group_by(Year, Trace, ScenarioGroup) %>%
#   summarise(Value = sum(Value)) %>%
#   mutate(Variable = "pd_energy")
# 
# pdh <- filter(hdata_sumd, Variable %in% c("davis_energy", "parker_energy")) %>%
#   group_by(Year) %>%
#   summarise(Value = sum(Value)) %>%
#   mutate(Variable = "pd_energy")
# 
# avg_hist <- bind_rows(
#   avg_hist, 
#   data.frame("Variable" = "pd_energy", Value = mean(pdh$Value))
# )
# 
# gg_pd <- custom_cloud(pd, pdh, "pd_energy", "Parker-Davis", 1.008)

### Save pngs ### 

ww <- 10
hh <- 7

results_dir <- file.path("C:/Users/cfelletter/Documents/MTOM/ScenarioSet",crmmsname) 
if (!file.exists(results_dir)) {
  message(paste('Creating folder:', results_dir))
  dir.create(results_dir)
}


ggsave(file.path(results_dir,"hoover.png"), gg_m, width = ww, height = hh)
ggsave(file.path(results_dir,"davis.png"), gg_d, width = ww, height = hh)
ggsave(file.path(results_dir,"parker.png"), gg_p, width = ww, height = hh)
ggsave(file.path(results_dir,"glen.png"), gg_g, width = ww, height = hh)

