rw_agg_file <- "ResStor_rwagg.csv" 
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) #ubres.rdf res.rdf
df_monthly <- rdf_aggregate(rwa1, rdf_dir = file_dir) #MUST be in Scenario FOLDER! not results folder
#get everything on a date 
df_monthly$Date = as.Date(paste0(df_monthly$Year,df_monthly$Month,"01"), format = "%Y%B%d")
#get a numeric month number
df_monthly$MonthNum = as.numeric(format.Date(df_monthly$Date, format = "%m"))

df_monthly$DataType = rep("Sim",times=dim(df_monthly)[1])



#observed
df_monthly_obs <- readxl::read_xlsx('data/HistResStorageMonthly.xlsx',col_names = T, ) 

res <- c("Powell","Mead","Mohave","Havasu","FlamingGorge","Navajo") #don't include "Starvation" since no obs
stornm <- paste0(res,rep(".Storage",length(res)))

df_monthly_obs =
  df_monthly_obs %>% pivot_longer(cols=all_of(stornm),names_to = 'Variable',values_to = 'Value')
df_monthly_obs$MonthNum = as.numeric(format.Date(df_monthly_obs$Date, format = "%m"))
df_monthly_obs$Year = as.numeric(format.Date(df_monthly_obs$Date, format = "%Y"))

#combine sim with obs
df_monthly <- rbind.data.frame(df_monthly[,names(df_monthly_obs)],df_monthly_obs)

df_monthly <- as.data.frame(df_monthly) #fix issues with class #i have to do this to get it to work, not sure why 

# add on starvation only after you process obs 
res <- c("Powell","Mead","Mohave","Havasu","FlamingGorge","Navajo","Starvation") 
stornm <- paste0(res,rep(".Storage",length(res)))
massnm <- paste0(res,rep(".SaltMass",length(res)))
concnm <- paste0(res,rep(".SaltConc",length(res)))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Node Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pdf(file = file.path(file_dir,paste("ResSalt_",scens,".pdf")), width=9, height=6)

for (i in 1:length(res)) {

  
  print(paste(res[i]))
  
  
  if(res[i] == "Starvation"){
    
  }
  
  # 3 panel annual plot
  unique(df_monthly$Variable)
  ###### ERRROR ############ Error in NextMethod() : only 0's may be mixed with negative subscripts
  p1 <-
    df_monthly %>%
    dplyr::filter(Variable == stornm[i])  %>%
    dplyr::filter(MonthNum == 12)  %>%
    group_by(DataType,Variable,Year) %>%
    mutate(Value = Value/1000) %>%
    ggplot(aes(x = Year, y = Value, color = DataType)) +
    geom_line() +
    theme_light() +
    theme(axis.title.x = element_blank()) +
    labs(title = paste(res[i]), y = "EOCY Storage (KAF)",x="")
  # print(p1)
  
  p2 <- df_monthly %>%
    dplyr::filter(Variable == massnm[i])  %>%
    dplyr::filter(MonthNum == 12)  %>%
    group_by(DataType,Variable,Year) %>%
    mutate(Value = Value/1000) %>%
    ggplot(aes(x = Year, y = Value, color = DataType)) +
    geom_line() +
    theme_light() +
    theme(axis.title.x = element_blank()) +
    labs(y = "Mass (ktons/yr)",x="")
  # print(p2)

  p3 <- df_monthly %>%
    dplyr::filter(Variable == concnm[i])  %>%
    dplyr::filter(MonthNum == 12)  %>%
    group_by(DataType,Variable,Year) %>%
    ggplot(aes(x = Year, y = Value, color = DataType)) +
    geom_line() +
    theme_light() +
    theme(axis.title.x = element_blank()) +
    labs(y = "FWAAC (mg/l)",x="")
  # print(p3)

  grid.arrange(p1,p2,p3,ncol=1)
  
  if(printfigs==T){ ggsave(filename = file.path(fig_dir,paste(res[i],"AnnPlot",scens,".png")), width = gage_widths[1],height = gage_heights[1])}
  
  # #calculate residual
  # gage <- df_monthly %>%
  #   dplyr::filter(Variable == massnm[i] & DataType == "Obs")
  # 
  # simulated <- df_monthly %>%
  #   dplyr::filter(Variable == massnm[i] & DataType == "Sim")
  # 
  # diff <- gage
  # diff$Value = simulated$Value - gage$Value
  # diff$Variable = rep("Residual",times = length(diff$Variable))
  # 
  # df_csv <- df_monthly %>%
  #   dplyr::filter(Variable == flownm[i] | Variable == massnm[i] | Variable == concnm[i])  %>%
  #   pivot_wider(names_from = Variable,values_from = Value)
  # #dump out the data
  # if(printfigs==T){ write.csv(x = df_csv,
  #                             file = file.path(data_dir,paste(nodenums[i],nodes[i],"Data",scens,".csv")))}

  
} #end node loop

dev.off() #mega plot

