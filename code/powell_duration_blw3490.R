# how many consecutive months is powell blw 3490?
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
hh <- 7
end_yr <- 2026

feather_file_nm <- "crspopsdata.feather"

dat <- feather::read_feather(file.path(feather_data_dir,feather_file_nm))  

############ Duration ################

# durations of events 
# tmp <- filter(xx, Variable == "Powell.PE",Year <= 2026)  %>%
#   mutate(p = if_else(Value <= 3490, 1,0)) #%>%
# rle(tmp$p)$lengths #no idea what this is showing! 
# boxplot(rle(tmp$p)$lengths)

dat <- dat %>% 
  filter(Year <= end_yr) %>% # rm this line if want all years 
  filter(Variable == "Powell.PE") 


run <- rle(dat$Value < 3490)
dat$exceeds <- rep(run$values, run$lengths)
dat$duration <- rep(run$lengths, run$lengths)
starts <- dat[head(c(1, cumsum(run$length) + 1), length(run$length)),]
result <- subset(starts, duration > 1 & exceeds)
# View(result)

result %>% 
  # dplyr::group_by(ScenarioGroup) %>%
  ggplot(aes(x=factor(Year),y=duration,color=ScenarioGroup)) +
  geom_boxplot() +
  labs(x = "", y = "Consercutive Months Below 3490'" ,title = "Duration of Powell < 3490") 

ggsave(filename = file.path(figures_dir,paste0("DurationPow3490_2226.png")), width = ww,height = hh)#width= width, height= height)
# ggsave(filename = file.path(figures_dir,paste0("DurationPow3490_allyrs.png")), width = ww,height = hh)#width= width, height= height)

write.csv(result,file = file.path(figures_dir,paste0("Blw3490_durations.csv")))



#line plot 
filter(xx, Variable == "Powell.PE")  %>%
  mutate(blw = if_else(Value <= 3490, 1,0)) %>%
  dplyr::group_by(ScenarioGroup, Year) %>%
  summarise(blw = sum(blw)/12) %>%
  ggplot(aes(x=Year,y=blw,color=ScenarioGroup)) + 
  geom_line() #+
# scale_x_discrete("Month",labels = 2022:2053,breaks=2022:2053) + #display abb. month names


xx %>% filter(Variable == "Powell.PE", Year <= 2026)  %>%
  mutate(blw = if_else(Value <= 3490, 1,0)) %>%
  dplyr::group_by(TraceNumber,ScenarioGroup, Year) %>%
  summarise(blw = sum(blw)/12) %>%
  ggplot(aes(x=factor(Year),y=blw,color=ScenarioGroup)) +
  geom_boxplot() +
  scale_y_continuous("",labels = scales::percent,
                     limits = c(0,1)) +
  labs(x = "Year", title = "Precentage of CY Year Powell < 3490") 
ggsave(filename = file.path(figures_dir,paste0("DurationPow3490.png")), width = ww,height = hh)#width= width, height= height)

xx %>% filter(Variable == "Powell.PE")  %>%
  mutate(blw = if_else(Value <= 3490, 1,0)) %>%
  dplyr::group_by(TraceNumber,ScenarioGroup, Year) %>%
  summarise(blw = sum(blw)/12) %>%
  ggplot(aes(x=factor(Year),y=blw,color=ScenarioGroup)) +
  geom_boxplot() +
  scale_y_continuous("",labels = scales::percent,
                     limits = c(0,1)) +
  labs(x = "Year", title = "Precentage of CY Year Powell < 3490") 
ggsave(filename = file.path(figures_dir,paste0("DurationPow3490_allyrs.png")), width = ww,height = hh)#width= width, height= height)

dev.off()

