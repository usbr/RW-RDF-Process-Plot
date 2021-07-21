# create cloud plots of FY/WY data w/o Hist
rm(list=ls())

library(tidyverse)
library(zoo)
library(RWDataPlyr)
library(lubridate)
library(crssplot) #scens_plot_cloud
# remotes::install_github('rabutler-usbr/crssplot')
# remotes::install_github("BoulderCodeHub/rhdb") #need github PAT env var set
library(rhdb) 
library(feather)

date_to_wy <- function(x) {
  mm <- month(x)
  yy <- year(x)
  yy[mm >= 10] <- yy[mm >= 10] + 1
  yy
}

CRSSDIR <- Sys.getenv("CRSS_DIR")


results_dir <- file.path(CRSSDIR,"results") 
# results_dir <- "M:/felletter"

Figs <- "Jul2021_MostPowerRun" #results dir folder 

# scengroups
custom_colors <- c("July 2021 w DRO" = "#138d75", 
                   "Jule 2021 no DRO" = "#f1c40f")
ww <- 10
hh <- 7
start_yr <- 2022
# end_yr <- 2030 

ofigs <- file.path(results_dir,Figs) 
if (!file.exists(ofigs)) {
  message(paste('Creating folder:', ofigs))
  dir.create(ofigs)
}
message('Figures will be saved to: ', ofigs)


# data ---------------------------------------
# CRSS

crss <- bind_rows(
  read_feather(file.path(ofigs,"tempData","CRSPPowerData.feather")) #if you created with Alan's main.R code
) 
unique(crss$Variable)
slotnames <- c("Powell.Energy", "BlueMesa.Energy","Crystal.Energy","FlamingGorge.Energy","Fontenelle.Energy","MorrowPoint.Energy" )

crss <- crss %>%
  mutate(
    Date = ymd(paste(Year, match(Month, month.name), "01", sep = "-")), 
    water_year = date_to_wy(Date)) %>%
  select(-Year, -Month) 
# head(crss)

scengroups <- unique(crss$ScenarioGroup)


# 24MS - OND (Oct,Nov,Dec) 2021 to combine with crss
ond2021_24MS <- readxl::read_xlsx(file.path(ofigs,"24MS_CRSP_Energy.xlsx")) #24MS results
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

for (i in 1:length(unique(crss$TraceNumber))) {
  if (i == 1){
    df =  mutate(ond21, TraceNumber = i)
  } else {
    df =  bind_rows(df,mutate(ond21, TraceNumber = i))
  }
}

unique(df$TraceNumber)
unique(df$ScenarioGroup)



# names(df)
# names(crss)
# zz <- bind_rows(crss[,names(df)], df) 
# write.csv(zz,file.path(ofigs,'zz_monthly.csv'))
# unique(df$TraceNumber)
# unique(df$ScenarioGroup)


# plotting data ---------------------------
zz <- bind_rows(crss[,names(df)], df) %>% # crss %>%
  group_by(Variable, water_year, TraceNumber, ScenarioGroup) %>%
  summarise(Value = sum(Value)) %>%
  rename(Year = water_year) %>%
  filter(Year >= start_yr) 
write.csv(zz,file.path(ofigs,'zz_wy.csv'))

# plot individual -------------------------

pdf(file.path(ofigs,'CRSP_Energy_WY.pdf'))
# 
# for(j in 1:length(slotnames)){
#   scens_plot_cloud(zz,vars=slotnames[j],title=slotnames[j])
# }
scens_plot_cloud(zz,vars="BlueMesa.Energy",title="BlueMesa")
scens_plot_cloud(zz,vars="MorrowPoint.Energy",title="MorrowPoint")
scens_plot_cloud(zz,vars="Crystal.Energy",title="Crystal")
scens_plot_cloud(zz,vars="Fontenelle.Energy",title="Fontenelle")
scens_plot_cloud(zz,vars="FlamingGorge.Energy",title="FlamingGorge")
scens_plot_cloud(zz,vars="Powell.Energy",title="Powell")


dev.off()


# custom_cloud <- function(x, h, vv, tt, ss = 1.02) {
#   
#   scens_plot_cloud(
#     x, 
#     vars = vv,
#     # historical = NA,
#     years = 2000:2031,
#     plot_colors = custom_colors,
#     title = paste(tt, "FY/WY Energy"), y_lab = "GWH",
#     legend_wrap = 15
#   ) +
#     labs(x = "FY/WY") 
# }

### add a filter for hdata since some res have incomplete data   
# write_feather(h_filter,"h_filter.feather") #hdb_query only works on my computer
# h_filter <- read_feather("h_filter.feather")
# write_feather(zz,"zz.feather") #hdb_query only works on my computer
# zz <- read_feather("zz.feather")
# 
# 
# h_filter <- hdata %>%
#   filter(Year >= start_yr)  
# 
# 
# #debug
# unique(zz$Year)
# unique(zz$Variable)
# ## replace CRSS names with _energy names
# zz[zz == "BlueMesa.Energy"] <- "blue_mesa_energy"
# zz[zz == "Crystal.Energy"] <- "crystal_energy"
# zz[zz == "FlamingGorge.Energy"] <- "flaming_gorge_energy"
# zz[zz == "Fontenelle.Energy"] <- "fontenelle_energy"
# zz[zz == "MorrowPoint.Energy"] <- "morrow_point_energy"
# zz[zz == "Powell.Energy"] <- "powell_energy"
# 
# unique(zz$ScenarioGroup)
# dim(zz)
# zz <- zz %>%
#   filter(ScenarioGroup %in% scengroups)  
# # 1210440-1205184 = 5256 'bad'
# setwd(ofigs)
# 
# pdf('CRSP_Energy_wHist.pdf')
# 
# custom_cloud(zz, h_filter, "fontenelle_energy", "Fontenelle",1.008)
# ggsave(filename = paste0("fontenelle_energy","_cloud.png"), width = ww,height = hh)#width= width, height= height)
# 
# custom_cloud(zz, h_filter, "flaming_gorge_energy", "Flaming Gorge", 1.008)
# ggsave(filename = paste0("flaming_gorge_energy","_cloud.png"), width = ww,height = hh)#width= width, height= height)
# 
# custom_cloud(zz, h_filter, "blue_mesa_energy", "Blue Mesa", 1.008)
# ggsave(filename = paste0("blue_mesa_energy","_cloud.png"), width = ww,height = hh)#width= width, height= height)
# 
# custom_cloud(zz, h_filter, "morrow_point_energy", "Morrow Point", 1.008)
# ggsave(filename = paste0("morrow_point_energy","_cloud.png"), width = ww,height = hh)#width= width, height= height)
# 
# custom_cloud(zz, h_filter, "crystal_energy", "Crystal", 1.008)
# ggsave(filename = paste0("crystal_energy","_cloud.png"), width = ww,height = hh)#width= width, height= height)
# 
# custom_cloud(zz, h_filter, "powell_energy", "Powell",1.008)
# ggsave(filename = paste0("powell_energy","_cloud.png"), width = ww,height = hh)#width= width, height= height)
# 
# 
# 
# ### write stats to csv 
# xx <- zz %>% dplyr::group_by(ScenarioGroup, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
#   dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
#                    '10th' = quantile(Value,.1),'90th' = quantile(Value,.9),
#                    'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot
# 
# write.csv(xx,'CRSP_Energy_Stats.csv')#paste0(vars[i],'_',scengroups[j],'.csv') )
# 
# 
# #### extra plots ####
# # 
# # gg_m <- custom_cloud(zz22, h_filter, "powell_energy", "Powell")
# # gg_p <- custom_cloud(zz, h_filter, "blue_mesa_energy", "blue_mesa", 1.008)
# # gg_d <- custom_cloud(zz, h_filter, "flaming_gorge_energy", "flaming_gorge", 1.008)
# # 
# # # blue_mesa flaming_gorge together -------------------
# # pd <- filter(zz, Variable %in% c("flaming_gorge_energy", "blue_mesa_energy")) %>%
# #   group_by(Year, TraceNumber, ScenarioGroup, Scenario) %>%
# #   summarise(Value = sum(Value)) %>%
# #   mutate(Variable = "pd_energy")
# # 
# # pdh <- filter(h_filter, Variable %in% c("flaming_gorge_energy", "blue_mesa_energy")) %>%
# #   group_by(Year) %>%
# #   summarise(Value = sum(Value)) %>%
# #   mutate(Variable = "pd_energy")
# # 
# # avg_hist <- bind_rows(
# #   avg_hist, 
# #   data.frame("Variable" = "pd_energy", Value = mean(pdh$Value))
# # )
# # 
# # gg_pd <- custom_cloud(pd, pdh, "pd_energy", "blue_mesa-flaming_gorge", 1.008)
# # 
# # ww <- 10
# # hh <- 7
# # 
# # ggsave("figures/hoover.png", gg_m, width = ww, height = hh)
# # ggsave("figures/flaming_gorge.png", gg_d, width = ww, height = hh)
# # ggsave("figures/blue_mesa.png", gg_p, width = ww, height = hh)
# # ggsave("figures/blue_mesa_flaming_gorge.png", gg_pd, width = ww, height = hh)
