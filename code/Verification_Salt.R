#### Combined Gage and Use Verificaiton Script ########
#CF,20210824: Improve to not use VerificationGage.rdf and instead take observed
# from a xlsx sheet. Use R to annualize the data rather than RWDataPlyr
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

results_dir <- file.path(CRSSDIR,"results") 
# # where rdf results folder is kept

#easier to make folder from output in the results dir than to move it 
scen_dir <- file.path(CRSSDIR,"results") #file.path(CRSSDIR,"Scenario")
# #containing the sub folders for each ensemble
scens <- "BaselineSaltVerification" 

printfigs<-T#T#make png figures and dump data 

y_lab_yr = 


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
################################################################################
#### #### A. Read flow data  ####  ####
################################################################################

#agg file specifying which slots
rw_agg_file <- "SaltVerificationRun_rwagg.csv" 
#read agg file specifying which slots
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) #ubres.rdf res.rdf

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
df_annual <- rdf_aggregate(rwa1, rdf_dir = file_dir) #MUST be in Scenario FOLDER! not results folder

# ## create obs from excel not crss run ##
# df_obs <- readxl::read_xlsx('data/Verification_Obs_Gage_Data_Linked.xlsx',sheet = "R_Input",col_names = T, ) 
# df_obs = df_obs %>% pivot_longer(cols=names(df_obs)[6:23],names_to = 'Variable',values_to = 'Value')
# 
# df_monthly <- rbind.data.frame(df_monthly,df_obs)

#annual data
df_obs_annual<-df_obs %>% group_by(Variable,Year) %>%
  summarise(Value = sum(Value))

df_annual <- rbind.data.frame(df_annual[,names(df_obs_annual)],df_obs_annual)

#fix issues with class
df_annual <- as.data.frame(df_annual)

#use this later to filter CUL
years <- unique(df_annual$Year)

nodes <- c('glen','cameo','gunn','dolor','cisco','grwy','gdale','yampa','duch',
           'white','grut','sanraf','arch','bluf','lees','grcan','virgin','hoover',
           'parker','imper')
nodenums <- c(1,2,6:8,10:12,14:20,23:25,28,29)
flownm <- paste0(nodenums,rep("_flow_",length(nodes)),nodes)
massnm <- paste0(nodenums,rep("_mass_",length(nodes)),nodes)
concnm <- paste0(nodenums,rep("_FWAAC_",length(nodes)),nodes)

df_obs <- readxl::read_xlsx('data/HistFlowMassConc.xlsx',col_names = T, ) 
df_obs = df_obs %>% pivot_longer(cols=names(df_obs)[3:62],names_to = 'Variable',values_to = 'Value')

df_annual <- cbind.data.frame(df_annual, DataType = rep("Sim",times=dim(df_annual)[1]))

df_annual <- rbind.data.frame(df_annual[,names(df_obs)],df_obs)


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Figures 
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
  p1 <- df_annual %>%
    dplyr::filter(Variable == flownm[i])  %>%
    group_by(DataType,Variable,Year) %>%
    mutate(Value = Value/1000) %>%
    ggplot(aes(x = Year, y = Value, color = DataType)) +
    geom_line() +
    theme_light() + 
    theme(axis.title.x = element_blank()) +
    labs(title = paste(nodes[i]), y = "Flow (KAF/yr)",x="")
  # print(p1)
  
  p2 <- df_annual %>%
    dplyr::filter(Variable == massnm[i])  %>%
    group_by(DataType,Variable,Year) %>%
    mutate(Value = Value/1000) %>%
    ggplot(aes(x = Year, y = Value, color = DataType)) +
    geom_line() +
    theme_light() + 
    # theme(legend.position = "none", axis.title.x = element_blank()) +
    theme(axis.title.x = element_blank()) +
    
    labs(y = "Mass (ktons)",x="")
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
  
  
  if(printfigs==T){ ggsave(filename = file.path(file_dir,paste0(nodes[i]," Ann ",scens,".png")), width = gage_widths[1],height = gage_heights[1])}

  

    
  #calculate residual
  gage <- df_annual %>%
    dplyr::filter(Variable == massnm[i] & DataType == "Obs")

  simulated <- df_annual %>%
    dplyr::filter(Variable == massnm[i] & DataType == "Sim")
  
  diff <- gage
  diff$Value = simulated$Value - gage$Value
  diff$Variable = rep("Residual",times = length(diff$Variable))

  cumsum_err <- cumsum(diff$Value) #get cumulative error

  # df_csv <- cbind.data.frame(pivot_wider(rbind.data.frame(df,diff),
  #             names_from = Variable,values_from = Value),
  #             cumsum_err)
  # 
  # #dump out the data for Jim
  # if(printfigs==T){ write.csv(x = df_csv,
  #                            file = file.path(file_dir,nodes[i],paste0("Data Ann Gage ",nodes[i]," ",scens,".csv")))}

  #annual residual
  # diff <- diff %>%
  #   dplyr::group_by(Year, Variable)
  # p <- diff %>%
  #   ggplot(aes(x = Year, y = Value, color = Variable)) +
  #   geom_line() +
  #   theme_light() +
  #   scale_y_continuous(labels = scales::comma) +
  #   labs(title = paste("Annual Mass Residual"), y = "Mass (tons/yr)")
  # print(p)
  # if(printfigs==T){ ggsave(filename = file.path(file_dir,nodes[i],paste0("Grph Ann Resid",nodes[i]," ",scens,".png")), width = gage_widths[2],height = gage_heights[2])}

  #annual metrics
  mae <- round(sum(abs(diff$Value))/length(diff$Value))
  bias <- round(sum(diff$Value)/length(diff$Value))
  error_perc <- round(mae/mean(gage$Value)*100)
  ann_avgflow <- round(mean(gage$Value))
  # print(paste(title,"mae",mae,"bias",bias,"Error % of avg mass",error_perc*100))

  #create a sperate matrix of annual stats to store the % of gage erorr
  if(!exists("annstats") | i==1){
    annstats <- array(c(nodes[i],mae,bias,error_perc,ann_avgflow)) #c(outflows[i],mae,bias,error_perc*100)
  } else {
    annstats <- rbind(annstats,c(nodes[i],mae,bias,error_perc,ann_avgflow))
  }

  
  #   
  #   
    #make row names then don't print reach
    if (dim(annstats)[1]==length(nodes)) { #this should mean you ran all of the traces
      colnames(annstats) <- c("Reach","MAE","Bias","Error % of avg mass","AA Mass")
      annstats <- annstats[,c("Reach","MAE","Bias","AA Mass","Error % of avg mass")]
      rownames(annstats) <- nodes
      # rownames(annstats)[18] <- "Lees Ferry"
      write.csv(annstats[,2:5],file = file.path(file_dir,paste0("AnnualVerificationStats",scens,".csv")))
    } else {
      write.csv(annstats,file = file.path(file_dir,paste0("Partial_AnnualVerificationStats",scens,".csv")))
    }

  } #end node loop
  
dev.off() #mega plot



