##############################################################################
#This script creates monthly boxplots of Outflow and PE to compare two MTOM runs
##############################################################################

#agg file specifying which slots

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# list.files(file.path(scen_dir,scens[2]))
# 
# rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[2],"WQANN.rdf")))

rw_agg_file <- "WQAnn.csv" #doesn't include outflow

#read agg file specifying which slots
# # NEW files are annual slots so use AsIs
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE))
# # # Old files from 2017 review are monthly so use EOCY 

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)

# unique(scen_res$Variable) #check variable names 

## New ##
## Make a dataframe for Outflow and Outflow Salt Mass at each of the Numeric Criteria Points and Lees
#agg file specifying which slots

# #####################################################################################################################################################################
# ## For old TriRvw 2017 Runs to plot you Outflow and Outflow Salt Mass you have to grab these slots from Salt.rdf since the output.control changed throughtout time#
# ##################################################################################################################################################################
rw_agg_file2 <- "Outflow&OutflowSaltMass.csv"
rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],"KeySlots.rdf")))
rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],"Salt.rdf")))
rwa2 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file2), stringsAsFactors = FALSE))
# # # Old files from 2017 review are monthly so use EOCY

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res2 <- rw_scen_aggregate(
  scens,
  agg = rwa2,
  scen_dir = scen_dir
)

unique(scen_res2$Variable) #check variable names 
## Divide Values by 1,000,000 to present data in Million of Tons/Year or MAF
scen_res2$Value=(scen_res2$Value)/1000000
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## create a pdf  
pdf(file.path(oFigs,paste0("WQAnn",Figs,".pdf")), width=9, height=6)

# ++++++++++++++++++++++++++Below Powell+++++++++++++++++++++++++++++++++++++

### Means ###

variable = "AnnlSlntyLsFrry_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River at Lees Ferry" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(350,550)


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
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year",subtitle = subtitle)+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "Powell.Outflow"
y_lab = "Outflow (million acre-ft/year)"
title = "Lake Powell Average Annual Outflow" 
# subtitle = "Average Annual Concentration Comparision"
ylims <- c(0,15)


df <- scen_res2 %>%
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

# write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))
#-------------------------------------------------------------------------------------

variable = "Powell.Outflow Salt Mass"
y_lab = "Outflow Salt Mass (million tons/year)"
title = "Lake Powell Average Annual Outflow Salt Mass" 
# subtitle = "Average Annual Concentration Comparision"
ylims <- c(0,10)


df <- scen_res2 %>%
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

# ++++++++++++++++++++++++++Below Mead+++++++++++++++++++++++++++++++++++++

NumCrit <- data.frame(yintercept=723)
variable = "AnnlSlntyHvr_FWAAC"
# variable = "AnnlSlntyPrkr_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River below Hoover Dam" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(550,750)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 
p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) +
  ylim(ylims) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year",subtitle = subtitle)+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))
#-------------------------------------------------------------------------------------

variable = "Mead.Outflow"
y_lab = "Outflow (million acre-ft/year)"
title = "Lake Mead Average Annual Outflow" 
# subtitle = "Average Annual Concentration Comparision"
ylims <- c(0,15)


df <- scen_res2 %>%
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

# write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))
#-------------------------------------------------------------------------------------

variable = "Mead.Outflow Salt Mass"
y_lab = "Outflow Salt Mass (million tons/year)"
title = "Lake Mead Average Annual Outflow Salt Mass" 
# subtitle = "Average Annual Concentration Comparision"
ylims <- c(0,10)


df <- scen_res2 %>%
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
#------------------------------Below Parker-------------------------------------------------------

NumCrit <- data.frame(yintercept=747)
variable = "AnnlSlntyPrkr_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River below Parker Dam" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(550,750)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 
p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) +
  ylim(ylims) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year",subtitle = subtitle)+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))
#-------------------------------------------------------------------------------------

variable = "Havasu.Outflow"
y_lab = "Outflow (million acre-ft/year)"
title = "Lake Havasu Average Annual Outflow" 
# subtitle = "Average Annual Concentration Comparision"
ylims <- c(0,10)


df <- scen_res2 %>%
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

# write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))
#-------------------------------------------------------------------------------------

variable = "Havasu.Outflow Salt Mass"
y_lab = "Outflow Salt Mass (million tons/year)"
title = "Lake Havasu Average Annual Outflow Salt Mass" 
# subtitle = "Average Annual Concentration Comparision"
ylims <- c(0,10)


df <- scen_res2 %>%
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
#-------------------------------At Imperial------------------------------------------------------

NumCrit <- data.frame(yintercept=879)
variable = "AnnlSlntyImprl_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River above Imperial Dam" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(700,900)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 
p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) +
  ylim(ylims) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year",subtitle = subtitle)+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))
#-------------------------------------------------------------------------------------

variable = "AboveImperialDamColoradoR.Outflow"
y_lab = "Outflow (million acre-ft/year)"
title = "Imperial Dam Average Annual Outflow" 
# subtitle = "Average Annual Concentration Comparision"
ylims <- c(0,10)


df <- scen_res2 %>%
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

# write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))
#-------------------------------------------------------------------------------------

variable = "AboveImperialDamColoradoR.Outflow Salt Mass"
y_lab = "Outflow Salt Mass (million tons/year)"
title = "Imperial Dam Average Annual Outflow Salt Mass" 
# subtitle = "Average Annual Concentration Comparision"
ylims <- c(0,10)


df <- scen_res2 %>%
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
### Exceedence ###
#-------------------------------------------------------------------------------------
variable = "Exc_AnnlSlntyHvr_FWAAC"
y_lab = "Probability of Exceedance"
title = "Colorado River below Hoover Dam" 
subtitle = "Probability of Exceeding (>=) 723 mg/l"
ylims <- c(0,0.5)

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
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year" , subtitle = subtitle)+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

write.csv(df,file = paste0(oFigs,'/','Exc_',variable,'.csv'))

#-------------------------------------------------------------------------------------
variable = "Exc_AnnlSlntyPrkr_FWAAC"
y_lab = "Probability of Exceedance"
title = "Colorado River below Parker Dam" 
subtitle = "Probability of Exceeding (>=) 747 mg/l"
ylims <- c(0,0.5)

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
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year" , subtitle = subtitle)+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

write.csv(df,file = paste0(oFigs,'/','Exc_',variable,'.csv'))

#-------------------------------------------------------------------------------------
variable = "Exc_AnnlSlntyImprl_FWAAC"
y_lab = "Probability of Exceedance"
title = "Colorado River above Imperial Dam" 
subtitle = "Probability of Exceeding (>=) 879 mg/l"

ylims <- c(0,0.5)
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
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year" , subtitle = subtitle)+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

write.csv(df,file = paste0(oFigs,'/','Exc_',variable,'.csv'))

dev.off()

