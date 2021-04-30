
#Get monthly values from HDB
library(tidyverse)
library(RWDataPlyr)
library(readxl)
setwd("C:/Users/cfelletter/Documents/CRSS working/Aspinall")

#read in as xlsx
df=read_xlsx("FontEvap.xlsx") #xlsx is read in with date as Date_Time POSIXct
# df=read.csv("FontEvap.csv",header = T) #csv is read in with date as Date_Time factor which won't work for next line
names(df) = c("Date","Value")

df$Month = format(df$Date,format = "%m")
# fcstmo = unique(df$YrMo)


#create a monthly data
df_m <- 
  df %>% #this means "pipe" the data to the next function 
  group_by(Month) %>% 
  summarise(Mean = mean(Value, na.rm = T)) #want average flow over month

#build place to hold peak date
write.csv(df_m,file = "FontAvgEvap.csv")

