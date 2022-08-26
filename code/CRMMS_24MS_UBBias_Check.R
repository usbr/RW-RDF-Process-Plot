library(tidyverse)
library(openxlsx) #read.xlsx

data_path <- "C:/Users/cfelletter/Documents/MTOM working/2022/Improve UB Rules vs 24MS/Comparison24MSvsEnsembleMode"
# df<-read.xlsx(xlsxFile = data_path,sheet = "Test", detectDates = T)
colnames(df)

#the formating here is supposed to match Sarah's 1_raw_reservoir data table 

files <- list.files(data_path)

library(stringr)

rundates<-str_remove(files,"MTOM_24MS_Comparison_MOST.xlsm")

rundates<-as.Date(rundates,format = "%Y%m%d")


# colnms <- read.xlsx(xlsxFile = file.path(data_path,"ColNames.xlsm"),sheet = "Compare_Data", detectDates = T,startRow = 4,colNames = F)[1,]
# "File"                                        "Timestep"                                       
# [3] "Fontenelle.Elevation"                        "Fontenelle.Inflow"                          
# [5] "Fontenelle.Outflow"                          "Fontenelle.Spill"                           
# [7] "FlamingGorge.Elevation"                      "FlamingGorge.Inflow"                        
# [9] "FlamingGorge.UnregInflow"                    "FlamingGorge.Outflow"                       
# [11] "FlamingGorge.Spill"                          "TaylorPark.Elevation"                       
# [13] "TaylorPark.Inflow"                           "TaylorPark.Outflow"                         
# [15] "BlueMesa.Elevation"                          "BlueMesa.Inflow"                            
# [17] "BlueMesa.UnregInflow"                        "BlueMesa.Outflow"                           
# [19] "BlueMesa.Spill"                              "Crystal.Elevation"                          
# [21] "Crystal.Outflow"                             "Crystal.Spill"                              
# [23] "Vallecito.Elevation"                         "Vallecito.Inflow"                           
# [25] "Vallecito.Outflow"                           "Navajo.Elevation"                           
# [27] "Navajo.Inflow"                               "Navajo.UnregInflow"                         
# [29] "Navajo.Outflow"                              "Navajo.Spill"                               
# [31] "Powell.Elevation"                            "Powell.Storage"                             
# [33] "Powell.Inflow"                               "Powell.UnregInflow"                         
# [35] "Powell.Evaporation"                          "Powell.Outflow"                             
# [37] "Powell.Spill"                                "Mead.Elevation"                             
# [39] "Mead.Outflow"                                "Mead.Energy"                                
# [41] "Mohave.Elevation"                            "Mohave.Outflow"                             
# [43] "Mohave.Energy"                               "Havasu.Elevation"                           
# [45] "Havasu.Outflow"                              "Havasu.Energy"                              
# [47] "PumpingFromLakeMead:SNWP.DiversionRequested" "MWDDiversion.DiversionRequested"            
# [49] "CAPDiversion.DiversionRequested"            


for (i in 1:length(files)) {
  if (i == 1) {
    zz<-read.xlsx(xlsxFile = file.path(data_path,files[i]),sheet = "Compare_Data", detectDates = T,startRow = 4,colNames = F)
    zz=cbind.data.frame("Run.Date"=rep(rundates[i],dim(zz)[1]),zz)
    df <- zz
  }
  
  zz<-read.xlsx(xlsxFile = file.path(data_path,files[i]),sheet = "Compare_Data", detectDates = T,startRow = 4,colNames = F)
  zz=cbind.data.frame("Run.Date"=rep(rundates[i],dim(zz)[1]),zz)
  df = rbind.data.frame(df,zz)
  print(paste("reading",files[i]))

}

head(df)
unique(df$File)

colnms <- read.csv(file.path("C:/Users/cfelletter/Documents/MTOM working/2022/Improve UB Rules vs 24MS/ColNames.csv"),header = F)


colnames(df)=c("Run.Date","Timestep",colnms)
colnames(df)[3]="Fontenelle.Elevation" #something goes wrong with read 

colnames(df)

#get a numeric month number
df$MonthNum = as.numeric(format.Date(df$Timestep, format = "%m"))

#save the data 
dput(df,"df_CRMMS_most_biascheck.csv")


####### RESUME HERE MONDAY #######
# can I find a pattern to when the error is off? 
# are we more off in May for PE than we are in Oct-Mar? 
####### RESUME HERE MONDAY #######




#### Check FG bias to over release ###########
# if PE Diff for MAY 1 is negative -->
# then CRMMS must be releasing too much water this confirms bias 

res <- "FlamingGorge"
# mon <- 5 
unique(df$Timestep)
unique(df$MonthNum)

# mydate <- unique(df$Timestep)[12]
# mydate #want 4-30 ~ May 1
variable = paste0(res,".Elevation")
title = paste(variable,mydate)
y_lab = "PE Diff (ft, 24MS-CRMMS)"


# p <- 
  df %>%
  # dplyr::filter(Timestep %in% mydate) %>%
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
  
  