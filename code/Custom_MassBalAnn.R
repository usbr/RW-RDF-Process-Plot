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

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## create a pdf  
pdf(file.path(oFigs,paste0("SaltMassBalGrph",Figs,".pdf")), width=9, height=6)

# ++++++++++++++++++++++++++DoloresExp_OutSaltMass+++++++++++++++++++++++++++++++++++++

### Means ###

variable = "UB_Natural_Inflow"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)


df_ub <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 
p <- df_ub %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  labs(title = paste("UB Natural Salt Loading",Model.Step.Name) , y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "LB_Natural_Inflow"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,4)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

variable2 = "CoRivPariaToLittleCO.Outflow Salt Mass"
df2 <- scen_res %>%
  dplyr::filter(Variable == variable2) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value))

##Subtracting out the UB portion of LB Natural Salt Load
df_lb = df

df_lb$Value <- df$Value - df2$Value #subtract off UB

p <- df_lb %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  labs(title = paste("LB Natural Salt Loading",Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

# write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "UB_Agricultural_Inflow"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "UB_AgSaltLoading"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
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
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
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
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
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
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
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
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
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
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
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
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
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
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))
#-------------------------------------------------------------------------------------

variable = "LB_WQIPS"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,7)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
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
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "LB_ChangeInReachSaltMass"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(-3.5,3.5)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "UB_ReservoirSaltMass"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(-3.5,3.5)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "LB_ReservoirSaltMass"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(-3.5,3.5)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)


dev.off()

