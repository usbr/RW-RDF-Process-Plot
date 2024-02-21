
library(dataRetrieval)
library(tidyverse)
# Choptank River near Greensboro, MD
siteNumber <- "09302000"
siteInfo <- readNWISsite(siteNumber)
parameterCd <- "00060"

# Raw daily data:
rawDailyData <- readNWISdv(
  siteNumber, parameterCd,
  "1990-10-01", "2020-09-31"
)

df <- addWaterYear(rawDailyData)
df <- df %>%  
  select(Date,waterYear,X_00060_00003) %>% 
  mutate(flowvol = 1.98347 *X_00060_00003) %>%
  rename(flow = X_00060_00003) %>% 
  mutate(year= year(df$Date)) %>%
  mutate(month= month(df$Date)) %>% 
  mutate(day = day(df$Date)) 

df %>% 
  summarise()
