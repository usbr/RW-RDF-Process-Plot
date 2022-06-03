library(tidyverse)
library(openxlsx) #read.xlsx

data_path <- "C:/Users/cfelletter/Documents/MTOM working/2022/Improve UB Rules vs 24MS/24MS_Compare_CRMMSMost.xlsx"
df<-read.xlsx(xlsxFile = data_path,sheet = "Test", detectDates = T)
colnames(df)

#the formating here is supposed to match Sarah's 1_raw_reservoir data table 

#get a numeric month number
df$MonthNum = as.numeric(format.Date(df$Timestep, format = "%m"))


#### Check FG bias to over release ###########
# if PE Diff for MAY 1 is negative -->
# then CRMMS must be releasing too much water this confirms bias 

res <- "FlamingGorge"
# mon <- 5 
unique(df$Timestep)
mydate <- unique(df$Timestep)[12]
mydate #want 4-30 ~ May 1
variable = paste0(res,".Pool Elevation")
title = paste(variable,mydate)
y_lab = "PE Diff (ft, 24MS-CRMMS)"


# p <- 
  df %>%
  dplyr::filter(Timestep %in% mydate) %>%
  ggplot(aes(x = Run.Date, y = FlamingGorge.Pool.Elevation)) +
  geom_line() +
  theme_light() + 
  scale_y_continuous(labels = scales::comma) +
  labs(title = paste(title), y = y_lab, x = "Study Run Date - Mons are -1")
  
  
#### Check BM bias to double peak ########
  res <- "BlueMesa"
  
  ### May ###
  unique(df$Timestep)
  mydate <- unique(df$Timestep)[1]
  mydate #may, when rules think BM should release peak
  variable = paste0(res,".Pool Elevation")
  title = paste(variable,mydate)
  y_lab = "Outflow Diff (kaf/mo, 24MS-CRMMS)"
  
  # p <- 
     df %>%
    dplyr::filter(Timestep %in% mydate) %>%
     ggplot(aes(x = Run.Date, y = BlueMesa.Outflow)) +
    geom_line() +
    theme_light() + 
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste(title), y = y_lab, x = "Study Run Date - Mons are -1")    
  
     ### June ###
     unique(df$Timestep)
     mydate <- unique(df$Timestep)[2]
     mydate #may, when rules think BM should release peak
     variable = paste0(res,".Pool Elevation")
     title = paste(variable,mydate)
     y_lab = "Outflow Diff (kaf/mo, 24MS-CRMMS)"
     
     # p <- 
     df %>%
       dplyr::filter(Timestep %in% mydate) %>%
       ggplot(aes(x = Run.Date, y = BlueMesa.Outflow)) +
       geom_line() +
       theme_light() + 
       scale_y_continuous(labels = scales::comma) +
       labs(title = paste(title), y = y_lab, x = "Study Run Date - Mons are -1")    
  
  