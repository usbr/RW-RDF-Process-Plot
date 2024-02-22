##############################################################################
#This script creates SaltMassBal figures 
##############################################################################

# # Disable Scientifc Notation 
# options(scipen=999)

if (file.exists(oFigs)){
  file_dir <- oFigs ### old TriRvw code relies on oFigs, replace by file_dir
} else if (file.exists(results_dir)){
  file_dir <- file.path(results_dir,scens[1])
} else {
  stop('need either oFigs or results_dir defined')
}

#### File Checks #####
if (!file.exists(file_dir)) {
  message(paste('Creating folder:', file_dir,'aka file_dir'))
  dir.create(file_dir)
  stop('if created folder need to move results rdfs into this dir and then proceed with code') #if created folder need to move results rdfs into this dir and then proceed with code
}

fig_dir <-  file.path(file_dir,"png_figures")
data_dir <-  file.path(file_dir,"csv_data")

if (!file.exists(fig_dir) | !file.exists(data_dir)) {
  dir.create(fig_dir)
  dir.create(data_dir)
}


#agg file specifying which slots
rw_agg_file <- "SaltMassBal_CRSSv5.csv" #slot names updated for v5 redo. see SaltVerification.control
# rw_agg_file <- "SaltMassBal_Nov19Control.csv" #UB Salt Mass Balance.ExportSaltMassExtra does not exist in Nov2019 CRSS.OFFC - Need to rebuild

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# list.files(file.path(scen_dir,scens[2]))
# 
rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],"SaltMassBalance.rdf")))

#read agg file specifying which slots
# # NEW files are annual slots so use AsIs
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) 

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)

# unique(scen_res$Variable) #check variable names 

## Divide Values by 1,000,000 to present data in Million of Tons/Year
scen_res$Value=(scen_res$Value)/1000000
y_lab = "Salt Mass (million tons/yr)"

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
if(T){

## create a pdf  
pdf(file.path(file_dir,paste0("SaltMassBalGrph_",Figs,".pdf")), width=9, height=6)

### Means ###
variable = "UpperBasinBalance"
title = "Upper Basin Salt Mass Balance"
ylims <- c(-1,1)


df_ub <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),                    
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9)) 
p <- df_ub %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
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


ggsave(filename = file.path(fig_dir,paste0(variable,".png")), width= width, height= height)

write.csv(df_ub,file = paste0(data_dir,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "UB_Natural_Inflow"
title = variable
ylims <- c(-1,6) #c(0,7)


df_ub <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9)) 
p <- df_ub %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name) , y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(fig_dir,paste0(variable,".png")), width= width, height= height)

write.csv(df_ub,file = paste0(data_dir,'/','Stats_',variable,'.csv'))

# #Work on this ####
# zz_all <- df_ub 
# NumCrit <- NA
# title = "UB Natural Salt In" 
# subtitle = NA
ylims <- c(NA,NA) #c(0,7)
# source("code/Cloud_plot_woHist.R")


#-------------------------------------------------------------------------------------

variable = "UB_Agricultural_Inflow"
title = "Total UB Agricultural Inflow"#variable
ylims <- c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),                    
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = "UB Agricultural Salt Loading", y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(fig_dir,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(data_dir,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "UB_AgSaltLoading"
title = "PL UB Agricultural Inflow"#variable
ylims <- c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),                    
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(fig_dir,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(data_dir,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "UB_AgSaltLoadingExtra"
title = "Growth UB Agricultural Inflow"#variable
ylims <- c(-1,6) #c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),                    
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(fig_dir,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(data_dir,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "UB_Exports"
title = "Total UB Exports"#variable
ylims <- c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),                    
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(fig_dir,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(data_dir,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "UB_ExportSaltMass"
title = "Standard UB Exports"#variable
ylims <- c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),                    
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(fig_dir,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(data_dir,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "UB_ExportSaltMassExtra"

title = "Extra (acting like) UB Exports"#variable
ylims <- c(0,7)
df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),                    
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(fig_dir,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(data_dir,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "UB_WQIPS"
title = variable
ylims <- c(0,7)
df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),                    
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9)) 
p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = "UB Water Quality Improvement Projects", y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(fig_dir,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(data_dir,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "UB_ChangeInReachSaltMass"

title = variable
ylims <- c(-3.5,3.5)
df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),                    
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(fig_dir,paste0(variable,".png")), width= width, height= height)

#-------------------------------------------------------------------------------------

variable = "UB_ReservoirSaltMass"

title = variable
ylims <- c(-3.5,3.5)
df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),                    
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = "UB Reservoir Salt Mass", y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(fig_dir,paste0(variable,".png")), width= width, height= height)

#-------------------------------------------------------------------------------------
#### Lower Basin ####
#-------------------------------------------------------------------------------------

variable = "LowerBasinBalance"
title = "Lower Basin Salt Mass Balance"
ylims <- c(-1,1)

df_ub <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),                    
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9)) 
p <- df_ub %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name) , y = y_lab, x = "Year")+
  geom_line(aes(x=factor(Year), y=MinOut, color=Scenario, group=Scenario),linetype = "dotted",size = 1) +  #bound with min max
  geom_line(aes(x=factor(Year), y=MaxOut, color=Scenario, group=Scenario),linetype = "dotted",size = 1)  +
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(fig_dir,paste0(variable,".png")), width= width, height= height)

write.csv(df_ub,file = paste0(data_dir,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

### something is causing this to fail around this point, I can't isolate it to this or that function
# variable = "LB_Natural_Inflow"
# title = variable
# ylims <- c(0,15) #c(0,4)
# 
# df <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr)# %>% #filter year
# 
# variable2 = "CoRivPariaToLittleCO.Outflow Salt Mass"
# df2 <- scen_res %>%
#   dplyr::filter(Variable == variable2) %>%
#   dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) #%>% #filter year
# 
# ##Subtracting out the UB portion of LB Natural Salt Load
# df_lb = df
# 
# df_lb$Value <- df$Value - df2$Value #don't just use the mean
# 
# variable = "LB_NaturalSalt_InflowOnly"
# df_lb <- df_lb %>%
#   mutate(Variable = variable)
# 
# scen_res = rbind.data.frame(scen_res,df_lb)
# 
# df_lb <- df_lb %>%
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),
#                    'Min' = quantile(Value,.1),'Max' = quantile(Value,.9))
# 
# ### something is causing this to fail around this point, I can't isolate it to this or that function
# 
# p <- df_lb %>%
#   ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
#   geom_line() +
#   geom_point() +
#   ylim(ylims) +
#   scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
#   labs(title = paste(title,Model.Step.Name) , y = y_lab, x = "Year")+
#   theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
# print(p)
# 
# ggsave(filename = file.path(fig_dir,paste0(variable,".png")), width= width, height= height)
# 
# write.csv(df_lb,file = paste0(data_dir,'/','Stats_',variable,'.csv'))
### something is causing this to fail around this point, I can't isolate it to this or that function

##-------------------------------------------------------------------------------------

variable = "LB_Agricultural_Inflow"
title = variable
ylims <- c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9))
#
p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "LB_Exports"
title = variable
ylims <- c(0,7)
df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),                    
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "LB_WQIPS"

title = variable
ylims <- c(0,7)
df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),                    
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "LB_ChangeInReachSaltMass"

title = variable
ylims <- c(-7.5,7.5) #c(-3.5,3.5)
df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),                    
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(fig_dir,paste0(variable,".png")), width= width, height= height)

#-------------------------------------------------------------------------------------

variable = "LB_ReservoirSaltMass"

title = variable
ylims <- c(-7.5,7.5) #c(-3.5,3.5)
df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),                    
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = "LB Reservoir Salt Mass", y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(fig_dir,paste0(variable,".png")), width= width, height= height)

dev.off()

}
