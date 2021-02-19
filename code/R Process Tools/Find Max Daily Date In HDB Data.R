
#figure out the conc to intialize CRSS run with 

#Sarah says this method matches what she did to come up with ICs
library(tidyverse)
library(RWDataPlyr)
library(readxl)
setwd("C:/Users/cfelletter/Documents/CRSS working/Aspinall")

# df=read.csv("Jan18IC.csv",header = T)
df=read_xlsx("NorthFork.xlsx")
df$Yr = format(df$Date,format = "%Y")
# fcstmo = unique(df$YrMo)


#create a monthly data
df_m <- 
  df %>% #this means "pipe" the data to the next function 
  group_by(Yr) %>% 
  summarise(max = max(Flow)) #want average flow over month

#build place to hold peak date
df_m$PeakDate = df$Date[1:length(df_m$max)] #format(df_m$Yr,format = "%Y-%m-%d") 

i = 1
for(i in 1:length(df_m$max)){
  
  df_m$PeakDate[i] = df$Date[which(df$Flow == df_m$max[i] & df$Yr == df_m$Yr[i])]
  
  
}

mean.Date(format(df_m$PeakDate,format = "%m-%d"))

write.csv(format(df_m$PeakDate,format = "%m-%d"),file = "NorthForkPeaks.csv")

# filter(df,)