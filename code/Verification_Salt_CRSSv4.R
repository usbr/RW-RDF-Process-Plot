#### Salt Verification Script ########
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls()) #clear the enviornment 

#Additional plotting functions and libraries 
library('tidyverse') #ggplot2,dplyr,tidyr
library('devtools')
library(RWDataPlyr)
library(CRSSIO)
library(xml2)
library(gridExtra)

CRSSDIR <- Sys.getenv("CRSS_DIR")
# CRSSDIR <- "C:/Users/cfelletter/Documents/crss.trirvw2020"
results_dir <- file.path(CRSSDIR,"results") #results end up in the crss_dir
# # where rdf results folder is kept

#easier to make folder from output in the results dir than to move it 
scen_dir <- file.path(CRSSDIR,"Scenario") # file.path(CRSSDIR,"results")
# #containing the sub folders for each ensemble
scens <- "CRSSv4_2020TriRvw_SaltVerification" 

printfigs<-T#T#make png figures and dump data 

mycolors <- c("#61bd17","#009E73","#6bbd28","#0072B2") #for Sector plots mid dark green - schedule depl, dark green - depl rqst, light green - depletion, blue - CUL, 
mylinetypes <- c("dotdash","dashed","solid","solid","dotdash")  #schedule depl, depl rqst, depletion, CUL, 

nyears <- length(2000:2019) #currently only used one place to calculate average

#standard powerpoint figure sizes 
# gage an, an gage resid, gage mon, mon gage resid, total use  
gage_widths <- c(9.5,9.5,9.5,9.5,9.5)
gage_heights <- c(7,7,7,7,7)
# sect an, sect mon, mon sect dist  
sect_widths <- c(5,6.5,5)
sect_heights <- c(3,4.2,3)

# #whole basin experiment figure sizes 
# # sect an, sect mon, mon sect dist  
sect_widths <- c(3.33,5,3.33)
sect_heights <- c(2,3,2)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### File Checks #####
file_dir <- file.path(results_dir,scens[1])
if (!file.exists(file_dir)) {
  message(paste('Creating folder:', file_dir,'move results rdfs into this dir and then proceed with code'))
  dir.create(file_dir)
  stop() #if created folder need to move results rdfs into this dir and then proceed with code
}

fig_dir <-  file.path(file_dir,"png_figures")
data_dir <-  file.path(file_dir,"csv_data")

if (!file.exists(fig_dir) | !file.exists(data_dir)) {
  dir.create(fig_dir)
  dir.create(data_dir)
}
################################################################################
#### #### A. Read flow data  ####  ####
################################################################################

#do this with rwagg becasue you need different FWAAC slots 
rw_agg_file <- "SaltVerificationAnn_rwagg_CRSSv4.csv" 
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) #ubres.rdf res.rdf
# rwa1 <- rwa1[c(1:16,18:63),] #remove virgin 

df_annual <- rdf_aggregate(rwa1, rdf_dir = file_dir) #MUST be in Scenario FOLDER! not results folder

df_annual <- as.data.frame(df_annual) #fix issues with class #i have to do this to get it to work, not sure why 

nodes <- c('glen','cameo','gunn','dolor','cisco','grwy','gdale','yampa','duch',
           'white','grut','sanraf','arch','bluf',"powellin",'lees','grcan','virgin','hoover',
           'parker','imper')
nodenums <- c(1,2,6:8,10:12,14:19,20,20,23:25,28,29)
flownm <- paste0(nodenums,rep("_flow_",length(nodes)),nodes)
massnm <- paste0(nodenums,rep("_mass_",length(nodes)),nodes)
concnm <- paste0(nodenums,rep("_conc_",length(nodes)),nodes)

df_obs <- readxl::read_xlsx('data/HistFlowMassConcAnn.xlsx',col_names = T, ) 
df_obs = df_obs %>% pivot_longer(cols=names(df_obs)[3:65],names_to = 'Variable',values_to = 'Value')

df_annual$DataType = rep("Sim",times=dim(df_annual)[1])

df_annual <- rbind.data.frame(df_annual[,names(df_obs)],df_obs)

#### monthly ####

rw_agg_file <- "SaltVerificationMon_rwagg.csv" 
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) #ubres.rdf res.rdf
df_monthly <- rdf_aggregate(rwa1, rdf_dir = file_dir) #MUST be in Scenario FOLDER! not results folder
#get everything on a date 
df_monthly$Date = as.Date(paste0(df_monthly$Year,df_monthly$Month,"01"), format = "%Y%B%d")
#get a numeric month number
df_monthly$MonthNum = as.numeric(format.Date(df_monthly$Date, format = "%m"))

df_monthly$DataType = rep("Sim",times=dim(df_monthly)[1])

df_monthly_obs <- readxl::read_xlsx('data/HistFlowMassConcMonthly.xlsx',col_names = T, ) 
df_monthly_obs = df_monthly_obs %>% pivot_longer(cols=names(df_monthly_obs)[3:62],names_to = 'Variable',values_to = 'Value')

#convert EOM Date to 1st of M to match df_monthly (could just make df_monthly EOM)
df_monthly_obs$Month = format.Date(df_monthly_obs$Date, format = "%B")
df_monthly_obs$Year = format.Date(df_monthly_obs$Date, format = "%Y")
df_monthly_obs$Date = as.Date(paste0(df_monthly_obs$Year,df_monthly_obs$Month,"01"), format = "%Y%B%d")
# unique(df_monthly_obs$Date) %in% unique(df_monthly$Date)

df_monthly <- rbind.data.frame(df_monthly[,names(df_monthly_obs)],df_monthly_obs)

df_monthly <- as.data.frame(df_monthly) #fix issues with class #i have to do this to get it to work, not sure why 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Node Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pdf(file = file.path(file_dir,paste("SaltVerification_",scens,".pdf")), width=9, height=6)

for (i in 1:length(nodes)) {
  # for (i in 1) {

  # #create a figure folder
  # if(printfigs==T){
  #   if (!file.exists(file.path(file_dir,nodes[i]))) {
  #     message(paste('Creating folder:',nodes[i]))
  #     dir.create(file.path(file_dir,nodes[i]))
  #   }
  # }

  print(paste("Node",nodes[i]))

  # 3 panel annual plot
  p1 <-
    df_annual %>%
    dplyr::filter(Variable == flownm[i])  %>%
    group_by(DataType,Variable,Year) %>%
    mutate(Value = Value/1000) %>%
    ggplot(aes(x = Year, y = Value, color = DataType)) +
    geom_line() +
    theme_light() +
    theme(axis.title.x = element_blank()) +
    labs(title = paste(nodes[i]), y = "Flow (KAF/yr)",x="")
  print(p1)

  p2 <- df_annual %>%
    dplyr::filter(Variable == massnm[i])  %>%
    group_by(DataType,Variable,Year) %>%
    mutate(Value = Value/1000) %>%
    ggplot(aes(x = Year, y = Value, color = DataType)) +
    geom_line() +
    theme_light() +
    theme(axis.title.x = element_blank()) +
    labs(y = "Mass (ktons/yr)",x="")
  # print(p2)

  p3 <- df_annual %>%
    dplyr::filter(Variable == concnm[i])  %>%
    group_by(DataType,Variable,Year) %>%
    ggplot(aes(x = Year, y = Value, color = DataType)) +
    geom_line() +
    theme_light() +
    theme(axis.title.x = element_blank()) +
    labs(y = "FWAAC (mg/l)",x="")
  # print(p3)

  grid.arrange(p1,p2,p3,ncol=1)

  if(printfigs==T){ ggsave(filename = file.path(fig_dir,paste(nodenums[i],nodes[i],"AnnPlot",scens,".png")), width = gage_widths[1],height = gage_heights[1])}

  #calculate residual
  gage <- df_annual %>%
    dplyr::filter(Variable == massnm[i] & DataType == "Obs")

  simulated <- df_annual %>%
    dplyr::filter(Variable == massnm[i] & DataType == "Sim")

  diff <- gage
  diff$Value = simulated$Value - gage$Value
  diff$Variable = rep("Residual",times = length(diff$Variable))

  df_csv <- df_annual %>%
    dplyr::filter(Variable == flownm[i] | Variable == massnm[i] | Variable == concnm[i])  %>%
    pivot_wider(names_from = Variable,values_from = Value)
  #dump out the data
  if(printfigs==T){ write.csv(x = df_csv,
                              file = file.path(data_dir,paste(nodenums[i],nodes[i],"Data",scens,".csv")))}
  #### annual residuals ####
  if(T){
  # annual residual
  r1 <- diff %>%
    mutate(Value = Value/1000) %>%
    dplyr::group_by(Year, Variable) %>%
    ggplot(aes(x = Year, y = Value, color = Variable)) +
    geom_line() +
    theme_light() +
    theme(legend.position = "none", axis.title.x = element_blank()) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste(nodes[i],"Annual Mass Residual"), y = "Mass (ktons/yr)")
  # if(printfigs==T){ ggsave(filename = file.path(file_dir,nodes[i],paste0("Grph Ann Resid",nodes[i]," ",scens,".png")), width = gage_widths[2],height = gage_heights[2])}
  
  cumsum_err <- cumsum(diff$Value)
  cumsum_df <- diff
  cumsum_df$Value = cumsum_err
  
  #annual cumsum residual
  r2 <- cumsum_df %>%
    mutate(Value = Value/1000) %>%
    ggplot(aes(x = Year, y = Value)) +
    geom_line() +
    theme_light() +
    scale_y_continuous(labels = scales::comma) +
    geom_hline(yintercept = 0) +
    theme(legend.position = "none", axis.title.x = element_blank()) +
    
    labs(title = paste(nodes[i],"Annual Cummulative Mass Residual"), y = "Mass (ktons/yr)")
  # if(printfigs==T){ ggsave(filename = file.path(ofigs,nodes[i],paste0("Grph Ann Cumsum Resid",nodes[i]," ",scens,".png")), width = gage_widths[2],height = gage_heights[2])}
  
  grid.arrange(r1,r2,ncol=1)
} #### end annual residuals ####
  
  #annual metrics
  mae <- round(sum(abs(diff$Value))/length(diff$Value))
  bias <- round(sum(diff$Value)/length(diff$Value))
  error_perc <- round(mae/mean(gage$Value)*100)
  ann_avgmass <- round(mean(gage$Value))
  # print(paste(title,"mae",mae,"bias",bias,"Error % of avg mass",error_perc*100))

  #flow for stats
  gage <- df_annual %>%
    dplyr::filter(Variable == flownm[i] & DataType == "Obs")

  simulated <- df_annual %>%
    dplyr::filter(Variable == flownm[i] & DataType == "Sim")
  diff <- gage
  diff$Value = simulated$Value - gage$Value
  #annual metrics
  mae_flow <- round(sum(abs(diff$Value))/length(diff$Value))
  bias_flow <- round(sum(diff$Value)/length(diff$Value))
  error_perc_flow <- round(mae_flow/mean(gage$Value)*100)
  ann_avgflow <- round(mean(gage$Value))
  
  #conc for stats
  gage <- df_annual %>%
    dplyr::filter(Variable == concnm[i] & DataType == "Obs")
  
  simulated <- df_annual %>%
    dplyr::filter(Variable == concnm[i] & DataType == "Sim")
  diff <- gage
  diff$Value = simulated$Value - gage$Value
  #annual metrics
  mae_conc <- round(sum(abs(diff$Value))/length(diff$Value))
  bias_conc <- round(sum(diff$Value)/length(diff$Value))
  error_perc_conc <- round(mae_conc/mean(gage$Value)*100)
  ann_avgconc <- round(mean(gage$Value))

  #create a sperate matrix of annual stats to store the % of gage erorr
  if(!exists("annstats") | i==1){
    annstats <- array(c(nodes[i],mae,bias,ann_avgmass,error_perc,
                        mae_flow,bias_flow,ann_avgflow,error_perc_flow,
                        mae_conc,bias_conc,ann_avgconc,error_perc_conc))
  } else {
    annstats <- rbind(annstats,
                      c(nodes[i],mae,bias,ann_avgmass,error_perc,
                        mae_flow,bias_flow,ann_avgflow,error_perc_flow,
                        mae_conc,bias_conc,ann_avgconc,error_perc_conc))
  }

  #make row names then don't print reach
  if (dim(annstats)[1]==length(nodes)) { #this should mean you ran all of the traces
    colnames(annstats) <- c("Reach","MAE Mass","Bias Mass","Avg Mass","Error % of Avg Mass",
                            "MAE Flow","Bias Flow","Avg Flow","Error % of Avg Flow",
                            "MAE conc","Bias conc","Avg conc","Error % of Avg conc")
    rownames(annstats) <- nodes
    write.csv(annstats[,2:dim(annstats)[2]],file = file.path(file_dir,paste0("AnnualVerificationStats",scens,".csv")))
  } else {
    write.csv(annstats,file = file.path(file_dir,paste0("Partial_AnnualVerificationStats",scens,".csv")))
  }

#### monthly #####
if(T){
  # 3 panel monthly plot
  p1 <- df_monthly %>%
    dplyr::filter(Variable == flownm[i])  %>%
    group_by(DataType,Variable,Date) %>%
    mutate(Value = Value/1000) %>%
    ggplot(aes(x = Date, y = Value, color = DataType)) +
    geom_line() +
    theme_light() + 
    theme(axis.title.x = element_blank()) +
    labs(title = paste(nodes[i]), y = "Flow (KAF/mo)",x="")
  # print(p1)
  
  p2 <- df_monthly %>%
    dplyr::filter(Variable == massnm[i])  %>%
    group_by(DataType,Variable,Date) %>%
    mutate(Value = Value/1000) %>%
    ggplot(aes(x = Date, y = Value, color = DataType)) +
    geom_line() +
    theme_light() +
    theme(axis.title.x = element_blank()) +
    labs(y = "Mass (ktons/mo)",x="")
  # print(p2)
  
  p3 <- df_monthly %>%
    dplyr::filter(Variable == concnm[i])  %>%
    group_by(DataType,Variable,Date) %>%
    ggplot(aes(x = Date, y = Value, color = DataType)) +
    geom_line() +
    theme_light() +
    theme(axis.title.x = element_blank()) +
    labs(y = "Concentration (mg/l)",x="")
  # print(p3)
  
  grid.arrange(p1,p2,p3,ncol=1)
  
  #calculate residual
  gage <- df_monthly %>%
    dplyr::filter(Variable == massnm[i] & DataType == "Obs")
  
  simulated <- df_monthly %>%
    dplyr::filter(Variable == massnm[i] & DataType == "Sim")
  
  diff <- gage
  diff$Value = simulated$Value - gage$Value
  diff$Variable = rep("Residual",times = length(diff$Variable))
  
  #monthly metrics
  mae <- round(sum(abs(diff$Value))/length(diff$Value))
  bias <- round(sum(diff$Value)/length(diff$Value))
  error_perc <- round(mae/mean(gage$Value)*100)
  mon_avgmass <- round(mean(gage$Value))
  # print(paste(title,"mae",mae,"bias",bias,"Error % of avg mass",error_perc*100))
  
  #flow for stats
  gage <- df_monthly %>%
    dplyr::filter(Variable == flownm[i] & DataType == "Obs")
  
  simulated <- df_monthly %>%
    dplyr::filter(Variable == flownm[i] & DataType == "Sim")
  diff <- gage
  diff$Value = simulated$Value - gage$Value
  #monthly metrics
  mae_flow <- round(sum(abs(diff$Value))/length(diff$Value))
  bias_flow <- round(sum(diff$Value)/length(diff$Value))
  error_perc_flow <- round(mae_flow/mean(gage$Value)*100)
  mon_avgflow <- round(mean(gage$Value))
  
  #create a sperate matrix of monthly stats to store the % of gage erorr
  if(!exists("monstats") | i==1){
    monstats <- array(c(nodes[i],mae,bias,mon_avgmass,error_perc,mae_flow,bias_flow,mon_avgflow,error_perc_flow))
  } else {
    monstats <- rbind(monstats,c(nodes[i],mae,bias,mon_avgmass,error_perc,mae_flow,bias_flow,mon_avgflow,error_perc_flow))
  }
  
  #make row names then don't print reach
  if (dim(monstats)[1]==length(nodes)) { #this should mean you ran all of the traces
    colnames(monstats) <- c("Reach","MAE Mon Mass","Bias Mon Mass","Avg Mon Mass","Error % of Avg Mon Mass",
                            "MAE Mon Flow","Bias Mon Flow","Avg Mon Flow","Error % of Avg Mon Flow")
    rownames(monstats) <- nodes
    write.csv(monstats[,2:dim(monstats)[2]],file = file.path(file_dir,paste0("MonthlyVerificationStats",scens,".csv")))
  } else {
    write.csv(monstats,file = file.path(file_dir,paste0("Partial_MonthlyVerificationStats",scens,".csv")))
  }
} #### end monthly ####

  } #end node loop

dev.off() #mega plot

df_annual$Scenario = "CRSSv4_2020TriRvw"
df_annual_2020 = df_annual

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5. Plot Mass Balance 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# match Inputs used in TriRvw_Master.R
if (F){
  oFigs <- fig_dir
  Model.Step.Name <- Figs <- scens #plot title and results/folder name #[plot type] identifying name .pdf
  startyr <- 2000 #filter out all years > this year
  endyr <- 2019
  width=9# 10 #9
  height=6 #6.67 #6
  customcolorltpt <- F
  lt_scale <- rep(1, 4)
  pt_scale <- rep(19, 4)
  mycolors <- c("#D55E00" , "#F0E442", "#009E73" , "#407ec9") #TRY 2 - color blind red, yellow, red (stop light), blue
  
  #SaltMassBal
  source("code/Custom_MassBalAnn.R")  
}


