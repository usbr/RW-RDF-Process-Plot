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
CRSSDIR <- "C:/Users/cfelletter/Documents/crss.trirvw2020"
results_dir <- file.path(CRSSDIR,"results") 
# # where rdf results folder is kept

#easier to make folder from output in the results dir than to move it 
scen_dir <- file.path(CRSSDIR,"results") #file.path(CRSSDIR,"Scenario")
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


nodes <- c('glen','cameo','gunn','dolor','cisco','grwy','gdale','yampa','duch',
           'white','grut','sanraf','arch','bluf',"powellin")#,'lees','grcan','virgin','hoover',
           # 'parker','imper')
nodenums <- c(1,2,6:8,10:12,14:19,20)#,20,23:25,28,29)
flownm <- paste0(nodenums,rep("_flow_",length(nodes)),nodes)
massnm <- paste0(nodenums,rep("_mass_",length(nodes)),nodes)
concnm <- paste0(nodenums,rep("_conc_",length(nodes)),nodes)

#combine the data from 2 runs of Verification_Salt.R
df_obs$Scenario = "Obs" 
df_annual_2020$Scenario = "CRSSv4_2020TriRvw"
df_annual$Scenario = "CRSSv5"

unique(df_annual_2020$Variable) %in% unique(df_annual$Variable)

#### save the files before I mess it up #### 
saveRDS(df_annual,file = "df_annual_2022")
saveRDS(df_annual_2020,file = "df_annual_2020TriRvw")
# saveRDS(df,file = "df_origCRSSv5baseline_vs_2020TriRvw")
# readRDS(file = "df_origCRSSv5baseline_vs_2020TriRvw")


head(df_annual)
head(df_annual_2020)

df1 = df_annual %>% 
  filter(DataType == "Sim")

df2 = df_annual_2020 %>% 
  filter(DataType == "Sim")

# which(unique(df2$Variable) %in% unique(df1$Variable))

df <- rbind.data.frame(df1,df2,df_obs)
head(df)
unique(df$Variable)
saveRDS(df,file = "df_compare_saltruns")

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
#   endyr <- 2019
#   width=9# 10 #9
#   height=6 #6.67 #6
#   customcolorltpt <- F
#   lt_scale <- rep(1, 4)
#   pt_scale <- rep(19, 4)
#   mycolors <- c("#D55E00" , "#F0E442", "#009E73" , "#407ec9") #TRY 2 - color blind red, yellow, red (stop light), blue
#   
#   #SaltMassBal
#   source("code/Custom_MassBalAnn.R")  
# }
# 
