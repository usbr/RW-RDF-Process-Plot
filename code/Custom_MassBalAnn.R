##############################################################################
#This script creates SaltMassBal figures 
##############################################################################

# # Disable Scientifc Notation 
# options(scipen=999)

#agg file specifying which slots
rw_agg_file <- "SaltMassBal.csv" #20190402: Add back in UB Salt Mass Balance.AgSaltLoading, UB Salt Mass Balance.AgSaltLoadingExtra , UB Salt Mass Balance.ExportSaltMass, UB Salt Mass Balance.ExportSaltMassExtra

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# list.files(file.path(scen_dir,scens[2]))
# 
# rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[2],"SaltMassBalance.rdf")))

#read agg file specifying which slots
# # NEW files are annual slots so use AsIs
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) 

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)

unique(scen_res$Variable) #check variable names 

## Divide Values by 1,000,000 to present data in Million of Tons/Year

scen_res$Value=(scen_res$Value)/1000000

#add scenario names to line, point and color scales
names(lt_scale) <- unique(scen_res$Scenario)
names(pt_scale) <- unique(scen_res$Scenario)
names(mycolors) <- unique(scen_res$Scenario)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## create a pdf  
pdf(file.path(oFigs,paste0("SaltMassBalGrph",Figs,".pdf")), width=9, height=6)


### Means ###

variable = "UpperBasinBalance"
y_lab = "Salt Mass (million tons/yr)"
title = "Upper Basin Salt Mass Balance"
ylims <- c(-1,1)


df_ub <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 
p <- df_ub %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name) , y = y_lab, x = "Year")+
  geom_line(aes(x=factor(Year), y=MinOut, color=Scenario, group=Scenario),linetype = "dotted") +  #bound with min max
  geom_line(aes(x=factor(Year), y=MaxOut, color=Scenario, group=Scenario),linetype = "dotted")  +
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

print(p)

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df_ub,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "LowerBasinBalance"
y_lab = "Salt Mass (million tons/yr)"
title = "Lower Basin Salt Mass Balance"
ylims <- c(-1,1)


df_ub <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 
p <- df_ub %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name) , y = y_lab, x = "Year")+
  geom_line(aes(x=factor(Year), y=MinOut, color=Scenario, group=Scenario),linetype = "dotted") +  #bound with min max
  geom_line(aes(x=factor(Year), y=MaxOut, color=Scenario, group=Scenario),linetype = "dotted")  +
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df_ub,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "UB_Natural_Inflow"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)


df_ub <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 
p <- df_ub %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name) , y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df_ub,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "LB_Natural_Inflow"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,4)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

variable2 = "CoRivPariaToLittleCO.Outflow Salt Mass"
df2 <- scen_res %>%
  dplyr::filter(Variable == variable2) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value))

##Subtracting out the UB portion of LB Natural Salt Load
df_lb = df

df_lb$Mean <- df$Mean - df2$Value #subtract off UB, used to be call $Value

p <- df_lb %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste("LB Natural Salt Loading",Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df_lb,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "UB_Agricultural_Inflow"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
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

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

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

variable = "UB_AgSaltLoadingExtra"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

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

variable = "LB_Agricultural_Inflow"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

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

variable = "UB_Exports"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

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

variable = "UB_ExportSaltMass"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

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

variable = "UB_ExportSaltMassExtra"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

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
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

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

variable = "UB_WQIPS"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 
p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
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
#   ylim(ylims) +
#   scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
#   labs(title = "UB Water Quality Improvement Projects", y = y_lab, x = "Year")+
#   theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
# print(p)
# 
# ggsave(filename = file.path(oFigs,paste0(variable,"customlimits.png")), width= width, height= height)


#-------------------------------------------------------------------------------------

variable = "LB_WQIPS"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

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

variable = "UB_ChangeInReachSaltMass"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(-3.5,3.5)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

#-------------------------------------------------------------------------------------

variable = "LB_ChangeInReachSaltMass"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(-3.5,3.5)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

#-------------------------------------------------------------------------------------

variable = "UB_ReservoirSaltMass"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(-3.5,3.5)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = "UB Reservoir Salt Mass", y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

#-------------------------------------------------------------------------------------

variable = "LB_ReservoirSaltMass"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(-3.5,3.5)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = "LB Reservoir Salt Mass", y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

dev.off()

