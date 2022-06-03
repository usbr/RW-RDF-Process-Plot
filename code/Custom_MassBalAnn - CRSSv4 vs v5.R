##############################################################################
#This script creates SaltMassBal figures 
##############################################################################

Model.Step.Name <- Figs <- "Salt_VerificationRuns_Compare_v4_v5"
scens <- list(
  # "2020TriRvw_Scen3_PulRvm3118" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200409",
  "CRSSv4" = "CRSSv4_2020TriRvw_SaltVerification",
  "CRSSv5" = "CRSSJan2022_2000runwSalt"
)

## Plot Parameters ##
if(T){
  width=9# 10 #9
  height=6 #6.67 #6
  
  startyr <- 2020 #filter out all years > this year
  endyr <- 2040 #60
  
  customcolorltpt <- F
  
  if(!file.exists(file.path(scen_dir, scens[1])) 
     | !file.exists(file.path(scen_dir, scens[2])))
    stop('Scenarios folder(s) do not exist or scen_dir is set up incorrectly. 
         Please ensure Scenarios is set correctly.')
  
  oFigs <- file.path(CRSSDIR,'results') 
  if (!file.exists(oFigs)) {
    message(paste('Creating folder:', oFigs))
    dir.create(oFigs)
  }
  
  oFigs <- file.path(oFigs,Model.Step.Name) 
  if (!file.exists(oFigs)) {
    message(paste('Creating folder:', Model.Step.Name))
    dir.create(oFigs)
  }
  
  message('Figures and tables will be saved to: ', oFigs)
  
  if(customcolorltpt == F && length(scens) == 2){ #1 color, first scen dashed, second solid 
    lt_scale <- rep(c(2, 1), 1)
    pt_scale <- rep(c(1, 19), 1)
    mycolors <- c("#ba9e66","#407ec9") #reclamation sand (old/baseline rslts), rec blue )
    
  } else if (customcolorltpt == F && length(scens) <= 4){ #4 colors, solid
    lt_scale <- rep(1, 4)
    pt_scale <- rep(19, 4)
    mycolors <- c("#D55E00" , "#F0E442", "#009E73" , "#407ec9") #TRY 2 - color blind red, yellow, red (stop light), blue
    
  } else if (customcolorltpt == F && length(scens) > 4) {
    stop("customcolorltpt not setup or too many Scens")
  } 
}

rw_agg_file <- "SaltMassBal_CRSSv4_2022.csv"    #"SaltMassBal_Nov19Control.csv" #Use this with CRSS 2020 TriRvw projection model
# rw_agg_file <- "SaltMassBal_CRSSv5_noExportExtra.csv" # use this for CRSS v4 Salt Verification
rdf_slot_names(read_rdf(iFile = file.path("C:/Users/cfelletter/Documents/crss.trirvw2020/results",scens[1],"SaltMassBalance.rdf")))
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) 

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens[1],
  agg = rwa1,
  scen_dir = "C:/Users/cfelletter/Documents/crss.trirvw2020/results"
)

########### CRSS v5 files #########
 ############ get it working ##############
rw_agg_file <- "SaltMassBal_CRSSv5.csv" #slot names updated for v5 redo. see SaltVerification.control
# rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[2],"SaltMassBalance.rdf")))
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) 

######### HACK because I didn't output UB Salt Mass Balance.ExportSaltMassExtra
# rwa1 = rwa1[c(1:17,19:21),]


#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res2 <- rw_scen_aggregate(
  scens[2],
  agg = rwa1,
  scen_dir = "C:/Users/cfelletter/Documents/crss.2023TRIRVW/results"
)

unique(scen_res$Variable) %in% unique(scen_res2$Variable)
# unique(scen_res$Variable) #check variable names 

##combine
scen_res = rbind.data.frame(scen_res,scen_res2)


## Divide Values by 1,000,000 to present data in Million of Tons/Year

scen_res$Value=(scen_res$Value)/1000000

#add scenario names to line, point and color scales
names(lt_scale) <- unique(scen_res$Scenario)
names(pt_scale) <- unique(scen_res$Scenario)
names(mycolors) <- unique(scen_res$Scenario)

#you don't get a scenario name assigned if you just have one, need it later for plotting 
if(length(scens) == 1){
  scen_res$Scenario = rep(scens,times=length(scen_res$Value))
} else {
  # # Adding factors so ggplot does not alphebetize legend
  scen_res$Scenario = factor(scen_res$Scenario, levels=names(scens))
  #this comes back as <NA> when only have 1 scen
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## create a pdf  
pdf(file.path(oFigs,paste0("SaltMassBalGrph_",Figs,".pdf")), width=9, height=6)

### Means ###
if(T){
variable = "UpperBasinBalance"
y_lab = "Salt Mass (million tons/yr)"
title = "Upper Basin Salt Mass Balance"
ylims <- c(-1,1)


# df <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
test <- which(scen_res$Variable == variable)
df = scen_res[test,] %>%
  #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 
p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  #ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name) , y = y_lab, x = "Year")+
  geom_line(aes(x=factor(Year), y=MinOut, color=Scenario, group=Scenario),linetype = "dotted",size = 1) +  #bound with min max
  geom_line(aes(x=factor(Year), y=MaxOut, color=Scenario, group=Scenario),linetype = "dotted",size = 1)  +
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

print(p)

# # Add BOR Logo
# # annotation_custom(im_rast, ymin = yaxmin, ymax = yaxmin + 12, xmin = 1999, xmax = 2006) #Jessie's way must spec position on ea
# source("code/add_logo.R") #alan's way, bottom right corner
# add_logo_horiz(p)


ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df_ub,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

# variable = "LowerBasinBalance"
# y_lab = "Salt Mass (million tons/yr)"
# title = "Lower Basin Salt Mass Balance"
# ylims <- c(-1,1)
# 
# 
# # df <- scen_res %>%
# #   dplyr::filter(Variable == variable) %>%
# test <- which(scen_res$Variable == variable)
# df = scen_res[test,] %>%
#   #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 
# p <- df %>%
#   ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
#   geom_line() +
#   geom_point() +
#   #ylim(ylims) +
#   scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
#   labs(title = paste(title,Model.Step.Name) , y = y_lab, x = "Year")+
#   geom_line(aes(x=factor(Year), y=MinOut, color=Scenario, group=Scenario),linetype = "dotted",size = 1) +  #bound with min max
#   geom_line(aes(x=factor(Year), y=MaxOut, color=Scenario, group=Scenario),linetype = "dotted",size = 1)  +
#   theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
# print(p)
# 
# ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)
# 
# write.csv(df_ub,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "UB_Natural_Inflow"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)


# df_ub <- scen_res %>%
#   dplyr::filter(Variable == variable) #%>%
test <- which(scen_res$Variable == variable)
df_ub = scen_res[test,] %>%
  #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 
p <- df_ub %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  #ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name) , y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df_ub,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

# variable = "LB_Natural_Inflow"
# y_lab = "Salt Mass (million tons/yr)"
# title = variable
# ylims <- c(0,4)
# 
# # df <- scen_res %>%
# #   dplyr::filter(Variable == variable) %>%
# test <- which(scen_res$Variable == variable)
# df = scen_res[test,] %>%
#   #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 
# 
# variable2 = "CoRivPariaToLittleCO.Outflow Salt Mass"
# # df <- scen_res %>%
# #   dplyr::filter(Variable == variable) %>%
# test <- which(scen_res$Variable == variable2)
# df2 = scen_res[test,] %>%
#   #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise(Value = mean(Value))
# 
# ##Subtracting out the UB portion of LB Natural Salt Load
# df_lb = df
# 
# df_lb$Mean <- df$Mean - df2$Value #subtract off UB, used to be call $Value
# 
# p <- df_lb %>%
#   ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
#   geom_line() +
#   geom_point() +
#   #ylim(ylims) +
#   scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
#   labs(title = paste(title,Model.Step.Name) , y = y_lab, x = "Year")+
#   theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
# print(p)
# 
# ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)
# 
# write.csv(df_lb,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "UB_Agricultural_Inflow"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

# df <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
test <- which(scen_res$Variable == variable)
df = scen_res[test,] %>%
  #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  #ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = "UB Agricultural Salt Loading", y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "UB_AgSaltLoading"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

# df <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
test <- which(scen_res$Variable == variable)
df = scen_res[test,] %>%
  #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  #ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "UB_AgSaltLoadingExtra"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

# df <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
test <- which(scen_res$Variable == variable)
df = scen_res[test,] %>%
  #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  #ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

# variable = "LB_Agricultural_Inflow"
# y_lab = "Salt Mass (million tons/yr)"
# title = variable
# ylims <- c(0,7)
# 
# # df <- scen_res %>%
# #   dplyr::filter(Variable == variable) %>%
# test <- which(scen_res$Variable == variable)
# df = scen_res[test,] %>%
#   #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 
# 
# p <- df %>%
#   ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
#   geom_line() +
#   geom_point() +
#   #ylim(ylims) +
#   scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
#   labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
#   theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
# print(p)

#-------------------------------------------------------------------------------------

variable = "UB_Exports"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

# df <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
test <- which(scen_res$Variable == variable)
df = scen_res[test,] %>%
  # #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  #ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "UB_ExportSaltMass"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

# df <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
test <- which(scen_res$Variable == variable)
df = scen_res[test,] %>%
  #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  #ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "UB_ExportSaltMassExtra"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

# df <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
test <- which(scen_res$Variable == variable)
df = scen_res[test,] %>%
  #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  #ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

# variable = "LB_Exports"
# y_lab = "Salt Mass (million tons/yr)"
# title = variable
# ylims <- c(0,7)
# 
# # df <- scen_res %>%
# #   dplyr::filter(Variable == variable) %>%
# test <- which(scen_res$Variable == variable)
# df = scen_res[test,] %>%
#   #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 
# 
# p <- df %>%
#   ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
#   geom_line() +
#   geom_point() +
#   #ylim(ylims) +
#   scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
#   labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
#   theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
# print(p)

#-------------------------------------------------------------------------------------

variable = "UB_WQIPS"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

# df <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
test <- which(scen_res$Variable == variable)
df = scen_res[test,] %>%
  #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 
p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  #ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = "UB Water Quality Improvement Projects", y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

# #custom scale for presentation
# ylims <- c(0,3)
# p <- df %>%
#   ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
#   geom_line() +
#   geom_point() +
#   #ylim(ylims) +
#   scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
#   labs(title = "UB Water Quality Improvement Projects", y = y_lab, x = "Year")+
#   theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
# print(p)
# 
# ggsave(filename = file.path(oFigs,paste0(variable,"customlimits.png")), width= width, height= height)


#-------------------------------------------------------------------------------------

# variable = "LB_WQIPS"
# y_lab = "Salt Mass (million tons/yr)"
# title = variable
# ylims <- c(0,7)
# 
# # df <- scen_res %>%
# #   dplyr::filter(Variable == variable) %>%
# test <- which(scen_res$Variable == variable)
# df = scen_res[test,] %>%
#   #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 
# 
# p <- df %>%
#   ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
#   geom_line() +
#   geom_point() +
#   #ylim(ylims) +
#   scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
#   labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
#   theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
# print(p)

#-------------------------------------------------------------------------------------

variable = "UB_ChangeInReachSaltMass"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(-3.5,3.5)

# df <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
test <- which(scen_res$Variable == variable)
df = scen_res[test,] %>%
  #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  #ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

#-------------------------------------------------------------------------------------

# variable = "LB_ChangeInReachSaltMass"
# y_lab = "Salt Mass (million tons/yr)"
# title = variable
# ylims <- c(-3.5,3.5)
# 
# # df <- scen_res %>%
# #   dplyr::filter(Variable == variable) %>%
# test <- which(scen_res$Variable == variable)
# df = scen_res[test,] %>%
#   #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 
# 
# p <- df %>%
#   ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
#   geom_line() +
#   geom_point() +
#   #ylim(ylims) +
#   scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
#   labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
#   theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
# print(p)
# 
# ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

#-------------------------------------------------------------------------------------

variable = "UB_ReservoirSaltMass"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(-3.5,3.5)

# df <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
test <- which(scen_res$Variable == variable)
df = scen_res[test,] %>%
  #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  #ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = "UB Reservoir Salt Mass", y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

#-------------------------------------------------------------------------------------

# variable = "LB_ReservoirSaltMass"
# y_lab = "Salt Mass (million tons/yr)"
# title = variable
# ylims <- c(-3.5,3.5)
# 
# # df <- scen_res %>%
# #   dplyr::filter(Variable == variable) %>%
# test <- which(scen_res$Variable == variable)
# df = scen_res[test,] %>%
#   #dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 
# 
# p <- df %>%
#   ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
#   geom_line() +
#   geom_point() +
#   #ylim(ylims) +
#   scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
#   labs(title = "LB Reservoir Salt Mass", y = y_lab, x = "Year")+
#   theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
# print(p)
# 
# ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

dev.off()

}
