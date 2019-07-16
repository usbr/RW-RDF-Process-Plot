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
pdf(file.path(oFigs,paste0("Export_SaltMassBalGrph_Custom",Figs,".pdf")), width=9, height=6)

# ++++++++++++++++++++++++++DoloresExp_OutSaltMass+++++++++++++++++++++++++++++++++++++


#-------------------------------------------------------------------------------------

variable = "UB_Exports"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,0.25)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year", subtitle = "All Export: Div Salt Conc (I) & Rtrn Pickup Conc (I)")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))


#-------------------------------------------------------------------------------------

variable = "UB_ExportSaltMass"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,0.25)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year",
       subtitle = "Div Salt Conc (I):Duchesne,RoaringFork,Glenwood,LilSnake,Price,SJChama",
       caption = "Div Salt Conc (I) with no Rtrn. Doesn't inc Dolores since interbasin.")+  
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)


## Includes: DuchesneExp_DivSaltMass + RoaringForkExp_DivSaltMass + GlenwoodExp_DivSaltMass + LilSnakeExp_DivSaltMass
## + PriceExp_DivSaltMass + SJChamaExp_DivSaltMass 

##Missing: #DoloresExp_DivSaltMass - 130 mg/l interbasin + FontExp_DivSaltMass 0 mg/l 


#-------------------------------------------------------------------------------------

variable = "UB_ExportSaltMassExtra"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,0.25)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year",
       subtitle = "Rtrn Pickup Conc (I):MIGreendaleOuray,LilSnakeMisc,UTSynfuels,SanRafEnergy,NIPP",
       caption = "Div at Inflow Salt Conc then Rtrn at Pickup Conc (I), 0 for all except NIPP (1390). ") +  
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

# Rtrn Pickup Conc (I): 0 mg/l MIGreendaleOuray,LilSnakeMisc,UTSynfuels,SanRafEnergy,NIPP 1390mg/l

#-------------------------------------------------------------------------------------

variable = "LB_Exports"
y_lab = "Salt Mass (million tons/yr)"
title = variable
ylims <- c(0,2)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) +
  geom_line() +
  geom_point() +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = paste(title,Model.Step.Name), y = y_lab, x = "Year",
       subtitle = "Inc:CAP,MWD,MohaveSteam,PersPerfRights,Needles,NRA")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

write.csv(df,file = paste0(oFigs,'/','Mean_',variable,'.csv'))

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)




#-------------------------------------------------------------------------------------


dev.off()

