##############################################################################
#This script creates FlowMassBal figures 
##############################################################################

# # Disable Scientifc Notation 
# options(scipen=999)

#agg file specifying which slots
rw_agg_file <- "FlowMassBal.csv" #20190402: Add back in UB Salt Mass Balance.AgSaltLoading, UB Salt Mass Balance.AgSaltLoadingExtra , UB Salt Mass Balance.ExportSaltMass, UB Salt Mass Balance.ExportSaltMassExtra

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# list.files(file.path(scen_dir,scens[2]))
# 
rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],"MassBalance.rdf")))
#ISSUE: mass balance doesn't know which is inflow for which basin! they have same object.slot name

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
pdf(file.path(oFigs,paste0("FlowMassBalGrph",Figs,".pdf")), width=9, height=6)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Means ###

variable = "UBFlow_Inflow"
y_lab = "Flow (MAF/yr)"
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
  # ylim(ylims) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "UBFlow_Outflow"
y_lab = "Flow (MAF/yr)"
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
  # ylim(ylims) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "UBFlow_OtherLosses"
y_lab = "Flow (MAF/yr)"
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
  # ylim(ylims) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "UBFlow_ChangeInReservoirStorage"
y_lab = "Flow (MAF/yr)"
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
  # ylim(ylims) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

#We don't output LB inflow or outflow from MasBal summary slot 

#-------------------------------------------------------------------------------------

variable = "LBFlow_ChangeInReservoirStorage"
y_lab = "Flow (MAF/yr)"
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
  # ylim(ylims) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)


dev.off()

