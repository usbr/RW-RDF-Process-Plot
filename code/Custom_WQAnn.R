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
# rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],"KeySlots.rdf")))
# rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],"Salt.rdf")))
rwa2 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file2), stringsAsFactors = FALSE))
# # # Old files from 2017 review are monthly so use EOCY

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res2 <- rw_scen_aggregate(
  scens,
  agg = rwa2,
  scen_dir = scen_dir
)

# unique(scen_res2$Variable) #check variable names 
## Divide Values by 1,000,000 to present data in Million of Tons/Year or MAF
scen_res2$Value=(scen_res2$Value)/1000000

#add scenario names to line, point and color scales
names(lt_scale) <- unique(scen_res$Scenario)
names(pt_scale) <- unique(scen_res$Scenario)
names(mycolors) <- unique(scen_res$Scenario)

# # Adding factors so ggplot does not alphebetize legend
scen_res$Scenario = factor(scen_res$Scenario, levels=names(scens))
scen_res2$Scenario = factor(scen_res2$Scenario, levels=names(scens))


# The blue gradient background is "graph trash" 
# # make custom axis shading, don't use for now doesn't look good with plotte pallette
# g <- rasterGrob(scales::alpha(blues9, 0.1), width=unit(1,"npc"), height = unit(1,"npc"), 
#                 interpolate = TRUE) #alpha is the transperency  

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if (T) {
  

## create a pdf  
pdf(file.path(oFigs,paste0("WQAnn_",Figs,".pdf")), width= width, height= height)

# ++++++++++++++++++++++++++Above Powell+++++++++++++++++++++++++++++++++++++

### Means ###
##### CISCO #####
#--Flow--#
variable = "8_flow_cisco" # "16_flow_grut" "19_flow_bluf"
y_lab = "Outflow (million acre-ft/year)"
title = "Cisco Average Annual Flow" 
df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

pf <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#--Mass--#
variable = "8_mass_cisco" #"19_mass_bluf" "16_mass_grut"
y_lab = "Outflow Salt Mass (million tons/year)"
title = "Cisco Average Annual Outflow Salt Mass"  
df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

pm <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "")+ 
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#--Conc--#
variable = "CiscoColorado_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Cisco FWAAC" 
subtitle = "Average Annual Concentration Comparison"
# ylims <- c(400,600)


df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

# View(df)

pc <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  # ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))


grid.arrange(pf,pm,pc,ncol=1)
ggsave(filename = file.path(oFigs,paste0("Cisco_3Panel.png")), width= width, height= height)


#-------------------------------------------------------------------------------------
##### GRUT #####
variable = "GreenRiverUTGreen_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Green River UT FWAAC" 
subtitle = "Average Annual Concentration Comparison"

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

pc <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  # ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#--Flow--#
variable = "16_flow_grut" # "16_flow_grut" "19_flow_bluf"
y_lab = "Outflow (million acre-ft/year)"
title = "GRUT Average Annual Flow" 
df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

pf <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "")+ 
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#--Mass--#
variable = "16_mass_grut" #"19_mass_bluf" "16_mass_grut"
y_lab = "Outflow Salt Mass (million tons/year)"
title = "GRUT Average Annual Outflow Salt Mass" 
df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

pm <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "")+ 
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))


grid.arrange(pf,pm,pc,ncol=1)
ggsave(filename = file.path(oFigs,paste0("GRUT_3Panel.png")), width= width, height= height)

#-------------------------------------------------------------------------------------
##### Bluff #####
#--Flow--#
variable = "19_flow_bluf" # "16_flow_grut" "19_flow_bluf"
y_lab = "Outflow (million acre-ft/year)"
title = "Bluff Average Annual Flow" 
df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

pf <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#--Mass--#
variable = "19_mass_bluf" #"19_mass_bluf" "16_mass_grut"
y_lab = "Outflow Salt Mass (million tons/year)"
title = "Bluff Average Annual Outflow Salt Mass" 
df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

pm <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#--Conc--#
variable = "Bluff_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Bluff FWAAC" 
subtitle = "Average Annual Concentration Comparison"

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

pc <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  # ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

grid.arrange(pf,pm,pc,ncol=1)
ggsave(filename = file.path(oFigs,paste0("Bluff_3Panel.png")), width= width, height= height)

#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
##### Powell Inflow #####

variable = "AnnlSlntyPwllInflw_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Powell Inflow FWAAC" 
subtitle = "Average Annual Concentration Comparison"
# ylims <- c(400,600)


df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

# View(df)

pc <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  # ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "Powell.Inflow"
y_lab = "Inflow (million acre-ft/year)"
title = "Lake Powell Average Annual Inflow" 
# subtitle = "Average Annual Concentration Comparison"

df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 


pf <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  # ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
# #print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))
#-------------------------------------------------------------------------------------

variable = "Powell.Inflow Salt Mass"
y_lab = "Inflow Salt Mass (million tons/year)"
title = "Lake Powell Average Annual Inflow Salt Mass" 
# subtitle = "Average Annual Concentration Comparison"

df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value))

pm <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
# #print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

grid.arrange(pf,pm,pc,ncol=1)
ggsave(filename = file.path(oFigs,paste0("PowIn_3Panel.png")), width= width, height= height)


#-------------------------------------------------------------------------------------

variable = "AnnlSlntyLsFrry_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River at Lees Ferry" 
subtitle = "Average Annual Concentration Comparison"
ylims <- c(400,600)


df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

pc <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  # ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "Powell.Outflow"
y_lab = "Outflow (million acre-ft/year)"
title = "Lake Powell Average Annual Outflow" 
# subtitle = "Average Annual Concentration Comparison"
# ylims <- c(7,10)

df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

pf <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +

  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  # ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))
#-------------------------------------------------------------------------------------

variable = "Powell.Outflow Salt Mass"
y_lab = "Outflow Salt Mass (million tons/year)"
title = "Lake Powell Average Annual Outflow Salt Mass" 
# subtitle = "Average Annual Concentration Comparison"
ylims <- c(0,10)


df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value))

pm <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +

  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

grid.arrange(pf,pm,pc,ncol=1)
ggsave(filename = file.path(oFigs,paste0("LF_3Panel.png")), width= width, height= height)

# ++++++++++++++++++++++++++Below Mead+++++++++++++++++++++++++++++++++++++

NumCrit <- data.frame(yintercept=723)
variable = "AnnlSlntyHvr_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River below Hoover Dam" 
subtitle = "Average Annual Concentration Comparison"
# subtitle = "Median Trace Annual Concentration Comparison"
ylims <- c(545,750)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 
# dplyr::summarise(Value = median(Value)) 

pc <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  # ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) +
  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)
# #ggsave(filename = file.path(oFigs,paste0(variable,"_Median.png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))
# write.csv(df,file = paste0(oFigs,'/','Median_',variable,'.csv'))

#-------------------------------------------------------------------------------------

variable = "Mead.Outflow"
y_lab = "Outflow (million acre-ft/year)"
title = "Lake Mead Average Annual Outflow" 
# subtitle = "Average Annual Concentration Comparison"
ylims <- c(7,10)

df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

pf <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))
#-------------------------------------------------------------------------------------

variable = "Mead.Outflow Salt Mass"
y_lab = "Outflow Salt Mass (million tons/year)"
title = "Lake Mead Average Annual Outflow Salt Mass" 
# subtitle = "Average Annual Concentration Comparison"
ylims <- c(0,10)


df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 


pm <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +

  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))

grid.arrange(pf,pm,pc,ncol=1)
ggsave(filename = file.path(oFigs,paste0("Mead_3Panel.png")), width= width, height= height)

#------------------------------Below Parker-------------------------------------------------------

NumCrit <- data.frame(yintercept=747)
variable = "AnnlSlntyPrkr_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River below Parker Dam" 
subtitle = "Average Annual Concentration Comparison"
ylims <- c(550,750)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 


pc <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +

  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  # ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) +
  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))
#-------------------------------------------------------------------------------------

variable = "Havasu.Outflow"
y_lab = "Outflow (million acre-ft/year)"
title = "Lake Havasu Average Annual Outflow" 
# subtitle = "Average Annual Concentration Comparison"
ylims <- c(0,10)


df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 


pf <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +

  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

# write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))
#-------------------------------------------------------------------------------------

variable = "Havasu.Outflow Salt Mass"
y_lab = "Outflow Salt Mass (million tons/year)"
title = "Lake Havasu Average Annual Outflow Salt Mass" 
# subtitle = "Average Annual Concentration Comparison"
ylims <- c(0,10)


df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 


pm <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +

  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

grid.arrange(pf,pm,pc,ncol=1)
ggsave(filename = file.path(oFigs,paste0("Parker_3Panel.png")), width= width, height= height)

#-------------------------------At Imperial------------------------------------------------------

NumCrit <- data.frame(yintercept=879)
variable = "AnnlSlntyImprl_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River above Imperial Dam" 
subtitle = "Average Annual Concentration Comparison"
ylims <- c(675,900)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 


pc <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +

  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  # ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) +
  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))
#-------------------------------------------------------------------------------------

variable = "AboveImperialDamColoradoR.Outflow"
y_lab = "Outflow (million acre-ft/year)"
title = "Imperial Dam Average Annual Outflow" 
# subtitle = "Average Annual Concentration Comparison"
ylims <- c(0,10)


df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 


pf <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +

  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

# write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))
#-------------------------------------------------------------------------------------

variable = "AboveImperialDamColoradoR.Outflow Salt Mass"
y_lab = "Outflow Salt Mass (million tons/year)"
title = "Imperial Dam Average Annual Outflow Salt Mass" 
# subtitle = "Average Annual Concentration Comparison"
ylims <- c(0,10)


df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 


pm <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +

  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)


grid.arrange(pf,pm,pc,ncol=1)
ggsave(filename = file.path(oFigs,paste0("Imperial_3Panel.png")), width= width, height= height)

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
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 


p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +

  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/',variable,'.csv'))

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
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 


p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +

  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/',variable,'.csv'))

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
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 


p <- df %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#print(p)

#ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df,file = paste0(oFigs,'/',variable,'.csv'))

#------------------------------------------------------------

dev.off()


}





