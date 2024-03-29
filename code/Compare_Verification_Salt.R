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
results_dir <- file.path(CRSSDIR,"results") 
# # where rdf results folder is kept


scen_dir <- file.path(CRSSDIR,"Scenario") #file.path(CRSSDIR,"results") #easier to make folder from output in the results dir than to move it 
# #containing the sub folders for each ensemble
# scens <- "V6,CRSS.V6.2.0.2024.Aug2023_2000SaltVerification,20231002NFS" 
# "V5.TriRvw,CRSS.V5.3.0.203.Jan2022.2023TriRvw.10.2022SaltIC.2000start.mdl,20221115NFS"

printfigs<-T#T#make png figures and dump data 

mycolors <- c("#61bd17","#009E73","#6bbd28","#0072B2") #for Sector plots mid dark green - schedule depl, dark green - depl rqst, light green - depletion, blue - CUL, 
mylinetypes <- c("dotdash","dashed","solid","solid","dotdash")  #schedule depl, depl rqst, depletion, CUL, 

nyears <- length(2000:2020) #currently only used one place to calculate average

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
# file_dir <- file.path(results_dir,scens[1])
# if (!file.exists(file_dir)) {
#   message(paste('Creating folder:', file_dir,'move results rdfs into this dir and then proceed with code'))
#   dir.create(file_dir)
#   stop() #if created folder need to move results rdfs into this dir and then proceed with code
# }
# 
# fig_dir <-  file.path(file_dir,"png_figures")
# data_dir <-  file.path(file_dir,"csv_data")
# 
# if (!file.exists(fig_dir) | !file.exists(data_dir)) {
#   dir.create(fig_dir)
#   dir.create(data_dir)
# }
# ################################################################################
# #### #### A. Read flow data  ####  ####
# ################################################################################
# 
# 
# nodes <- c('glen','cameo','gunn','dolor','cisco','grwy','gdale','yampa','duch',
#            'white','grut','sanraf','arch','bluf',"powellin",'lees','grcan','virgin','hoover',
#            'parker','imper')
# nodenums <- c(1,2,6:8,10:12,14:19,20,20,23:25,28,29)
# flownm <- paste0(nodenums,rep("_flow_",length(nodes)),nodes)
# massnm <- paste0(nodenums,rep("_mass_",length(nodes)),nodes)
# concnm <- paste0(nodenums,rep("_conc_",length(nodes)),nodes)

#combine the data from 2 runs of Verification_Salt.R
df_obs$Scenario = "Obs" 
# df_annual_baseline$Scenario = "CRSSv5_2023TriRvw"

# df_annual <- readRDS(file = "AbvOnly.9001")

# df_annual$Scenario = "CRSSv5"
# saveRDS(df_annual,file = "df_annual_baselineTriRvw")

###TO DO turn this next part into a loop which process and handles two scenarios ###
scens
scen_nm <- scens
df_annual$Scenario = scen_nm #"V6.9005.McPhee"
saveRDS(df_annual,file = scen_nm) #should be saving it in a better place


# unique(df_annual_baseline$Variable) %in% unique(df_annual$Variable)

#### save the files before I mess it up #### 
# saveRDS(df_annual,file = "df_annual_baselineTriRvw")

# df_annual_baseline <- readRDS(file = "df_annual_v6")
df_annual_baseline <- readRDS(file = "df_annual_V6.9002")  #"df_annual_baselineTriRvw")


head(df_annual)
head(df_annual_baseline)

df1 = df_annual %>% 
  filter(DataType == "Sim")

df2 = df_annual_baseline %>% 
  filter(DataType == "Sim")

unique(df1$Variable) %in% unique(df2$Variable)
head(df1)
head(df2)
head(df_obs)



# which(unique(df2$Variable) %in% unique(df1$Variable))

df <- rbind.data.frame(df1,df2,df_obs)
head(df)
unique(df$Variable)
saveRDS(df,file = "df_compare_saltruns_9002vs9005")#"df_compare_saltruns_9002vsV5")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Node Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pdf(file = file.path(file_dir,paste("Compare2_SaltVerificationRuns.pdf")), width=9, height=6)

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
  p1 <- df %>%
    dplyr::filter(Variable == flownm[i])  %>%
    group_by(Variable,Year,Scenario) %>%
    mutate(Value = Value/1000) %>%
    ggplot(aes(x = Year, y = Value, color = Scenario)) +
    geom_line() +
    theme_light() +
    theme(axis.title.x = element_blank()) +
    labs(title = paste(nodes[i]), y = "Flow (KAF/yr)",x="")
  # print(p1)

  p2 <- df %>%
    dplyr::filter(Variable == massnm[i])  %>%
    group_by(Variable,Year,Scenario) %>%
    mutate(Value = Value/1000) %>%
    ggplot(aes(x = Year, y = Value, color = Scenario)) +
    geom_line() +
    theme_light() +
    theme(axis.title.x = element_blank()) +
    labs(y = "Mass (ktons/yr)",x="")
  # print(p2)

  p3 <- df %>%
    dplyr::filter(Variable == concnm[i])  %>%
    group_by(Variable,Year,Scenario) %>%
    ggplot(aes(x = Year, y = Value, color = Scenario)) +
    geom_line() +
    theme_light() +
    theme(axis.title.x = element_blank()) +
    labs(y = "FWAAC (mg/l)",x="")
  # print(p3)

  grid.arrange(p1,p2,p3,ncol=1)

  # if(printfigs==T){ ggsave(filename = file.path(fig_dir,paste(nodenums[i],nodes[i],"AnnPlot",scens,".png")), width = gage_widths[1],height = gage_heights[1])}


  } #end node loop

dev.off() #mega plot

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5. Plot Mass Balance 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# # match Inputs used in TriRvw_Master.R
# if (F){
#   oFigs <- fig_dir
#   Model.Step.Name <- Figs <- scens #plot title and results/folder name #[plot type] identifying name .pdf
#   startyr <- 2000 #filter out all years > this year
#   endyr <- 2020
#   width=9# 10 #9
#   height=6 #6.67 #6
#   customcolorltpt <- F
#   lt_scale <- rep(1, 4)
#   pt_scale <- rep(19, 4)
#   mycolors <- c("#D55E00" , "#F0E442", "#009E73" , "#407ec9") #TRY 2 - color blind red, yellow, red (stop light), blue
#   

scens <- list(
  "V6.9002" = "9002",
  "V5.TriRvw" = "V5.TriRvw,CRSS.V5.3.0.203.Jan2022.2023TriRvw.10.2022SaltIC.2000start.mdl,20221115NFS"
)

#   #SaltMassBal
#   source("code/Custom_MassBalAnn.R")  
# }
# 
