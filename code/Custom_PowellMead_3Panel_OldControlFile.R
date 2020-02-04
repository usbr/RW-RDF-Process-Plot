##############################################################################
#This script creates Flow, Mass and Concentration 3 Panel Figures to compare runs

##DEVELOPMENT UNDERWAY## 
##############INFLOW Average Annual SLOT NEEDS Moved into DEV CRSS Model############ 

##############################################################################

library(gridExtra)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# list.files(file.path(scen_dir,scens[2]))
# 
# rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[2],"WQANN.rdf")))


# Parameters for cloud plot customization (line thicknesses, text size, etc.)
# Have been pulled out for convenience
#Text
TitleSize =8
AxisText = 7
LegendLabText = 7

AxisLab = 7
LabSize = 7
LegendText = 7

ylims_pow_inoutflow <- c(7,11)
ylims_pow_inoutmass <- c(5,7)
ylims_mead_inoutflow <- c(7,11)
ylims_mead_inoutmass <- c(7,9)




rw_agg_file <- "PowellMeadInOut_OldControlFile.csv" #doesn't include outflow
#need to add WQAnn.rdf	AnnualSalinity.PowellInflow_FWAAC	eocy to model to use new


#read agg file specifying which slots
# # NEW files are annual slots so use AsIs
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE))
# # # Old files from 2017 review are monthly so use EOCY 

## DEBUG: check to make sure you have latest TriRvw output.control. All slots should be true for Salt.rdf except res.In,Stor,Out
# file.exists(file.path(scen_dir,scens[1],"Salt.rdf"))
# rdf_slot_names(rdf = read.rdf(iFile=file.path(scen_dir,scens[1],"Salt.rdf")))
# cbind(rwa1$variable, rwa1$file ,rwa1$variable %in% rdf_slot_names(rdf = read.rdf(iFile=file.path(scen_dir,scens[1],"Salt.rdf"))))
# rdf_slot_names(rdf = read.rdf(iFile=file.path(scen_dir,scens[1],"WQAnn.rdf")))


#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)

# unique(scen_res$Variable) #check variable names

# # Adding factors so ggplot does not alphebetize legend
scen_res$Scenario = factor(scen_res$Scenario, levels=names(scens))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## create a pdf  
pdf(file.path(oFigs,paste0("PowMeadInOut_",Figs,".pdf")), width= width, height= height)

# ++++++++++++++++++++++++++Below Powell+++++++++++++++++++++++++++++++++++++
# Powell Flow 
#-------------------------------------------------------------------------------------

# variables = c("Powell.Inflow","Powell.Storage","Powell.Outflow"
variable = "Powell.Inflow"

df1 <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

df1$Value <- df1$Value/1000000

variable = "Powell.Storage"

df2 <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

df2$Value <- df2$Value/1000000

variable = "Powell.Outflow"
df3 <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

df3$Value <- df3$Value/1000000

y_lab = "Inflow (million acre-ft/year)"
title = "Lake Powell Average Annual Inflow" 

p1 <- df1 %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(plot.title = element_text(size = TitleSize),         axis.text.x = element_text(size = AxisLab),         axis.text.y = element_text (size =AxisLab),         axis.title = element_text(size=AxisText),          legend.text = element_text(size=LegendText),         legend.title = element_text(size=LegendLabText, face="bold"),         legend.box.margin = margin(0,0,0,0)) 
# print(p1)

y_lab = "Storage (million acre-ft)"
title = "Lake Powell Average Annual Storage" 

p2 <- df2 %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  # ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(plot.title = element_text(size = TitleSize),         axis.text.x = element_text(size = AxisLab),         axis.text.y = element_text (size =AxisLab),         axis.title = element_text(size=AxisText),          legend.text = element_text(size=LegendText),         legend.title = element_text(size=LegendLabText, face="bold"),         legend.box.margin = margin(0,0,0,0)) 
# print(p2)

y_lab = "Outflow (million acre-ft/year)"
title = "Lake Powell Average Annual Outflow" 

p3 <- df3 %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(plot.title = element_text(size = TitleSize),         axis.text.x = element_text(size = AxisLab),         axis.text.y = element_text (size =AxisLab),         axis.title = element_text(size=AxisText),          legend.text = element_text(size=LegendText),         legend.title = element_text(size=LegendLabText, face="bold"),         legend.box.margin = margin(0,0,0,0)) 
# print(p3)

grid.arrange(p1,p2,p3,ncol=1)

ggsave(filename = file.path(oFigs,paste0("Powell_Flow_3Panel.png")), width= width, height= height)

#-------------------------------------------------------------------------------------
# Powell Mass
#-------------------------------------------------------------------------------------


variable = "Powell.Inflow Salt Mass"

df1 <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

df1$Value <- df1$Value/1000000

# variable = "Powell.Reservoir Salt Mass"
# 
# df2 <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise(Value = mean(Value))
# 
# df2$Value <- df2$Value/1000000

variable = "Powell.Outflow Salt Mass"
df3 <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

df3$Value <- df3$Value/1000000

y_lab = "Inflow Salt Mass (million tons/year)"
title = "Lake Powell Average Annual Inflow Salt Mass" 

ylims_pow_inoutmass <- c(5,7)

p1 <- df1 %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims_pow_inoutmass) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(plot.title = element_text(size = TitleSize),         axis.text.x = element_text(size = AxisLab),         axis.text.y = element_text (size =AxisLab),         axis.title = element_text(size=AxisText),          legend.text = element_text(size=LegendText),         legend.title = element_text(size=LegendLabText, face="bold"),         legend.box.margin = margin(0,0,0,0)) 
# print(p1)

# y_lab = "Reservoir Salt Mass (million tons/year)"
# title = "Lake Powell Average Annual Salt Mass" 
#  
# p2 <- df2 %>%
#   ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
# #   
#   scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
#   geom_line() +
#   geom_point() +
#   # ylim(ylims) +
#   scale_linetype_manual(values = lt_scale) +
#   scale_shape_manual(values = pt_scale) +
#   scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
#   labs(title = title, y = y_lab, x = "")+ #remove model step name from title
#   theme(plot.title = element_text(size = TitleSize),         axis.text.x = element_text(size = AxisLab),         axis.text.y = element_text (size =AxisLab),         axis.title = element_text(size=AxisText),          legend.text = element_text(size=LegendText),         legend.title = element_text(size=LegendLabText, face="bold"),         legend.box.margin = margin(0,0,0,0)) 
# # print(p2)

y_lab = "Outflow Salt Mass (million tons/year)"
title = "Lake Powell Average Annual Outflow Salt Mass" 

p3 <- df3 %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims_pow_inoutmass) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(plot.title = element_text(size = TitleSize),         axis.text.x = element_text(size = AxisLab),         axis.text.y = element_text (size =AxisLab),         axis.title = element_text(size=AxisText),          legend.text = element_text(size=LegendText),         legend.title = element_text(size=LegendLabText, face="bold"),         legend.box.margin = margin(0,0,0,0)) 
# print(p3)

grid.arrange(p1,p3,ncol=1)
# grid.arrange(p1,p3,ncol=1) # if don't have latest control file won't have res.salt mass

ggsave(filename = file.path(oFigs,paste0("Powell_Mass_3Panel.png")), width= width, height= height)


#-------------------------------------------------------------------------------------
# Powell Concentration 
#-------------------------------------------------------------------------------------

##############Ensure Inflow Slots have been imported into latest CRSS############ 

# variable = "AnnlSlnty_In_Powell_FWAAC"
# 
# df1 <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise(Value = mean(Value))

### FOR WELL MIXED RES CONC = OUTFLOW CONC - do 2 panel for now
# variable = "Powell.Reservoir Salt Concentration"
# 
# df2 <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise(Value = mean(Value)) 
# 
# df2$Value <- df2$Value/1000000

variable = "AnnlSlntyLsFrry_FWAAC"
df3 <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value))


# y_lab = "Inflow Salt Concentration (million tons/year)"
# title = "Lake Powell Average Annual Inflow Salt Concentration" 
# 
# p1 <- df1 %>%
#   ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
#   
#   scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
#   geom_line() +
#   geom_point() + 
#   # ylim(ylims) +
#   scale_linetype_manual(values = lt_scale) +
#   scale_shape_manual(values = pt_scale) +
#   scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
#   labs(title = title, y = y_lab, x = "")+ #remove model step name from title
#   theme(plot.title = element_text(size = TitleSize),         axis.text.x = element_text(size = AxisLab),         axis.text.y = element_text (size =AxisLab),         axis.title = element_text(size=AxisText),          legend.text = element_text(size=LegendText),         legend.title = element_text(size=LegendLabText, face="bold"),         legend.box.margin = margin(0,0,0,0)) 
# # print(p1)

# y_lab = "Storage (million acre-ft)"
# title = "Lake Powell Average Annual Storage" 
# # ylims <- c(0,20)
# 
# p2 <- df2 %>%
#   ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
#   
#   scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
#   geom_line() +
#   geom_point() + 
#   # ylim(ylims) +
#   scale_linetype_manual(values = lt_scale) +
#   scale_shape_manual(values = pt_scale) +
#   scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
#   labs(title = title, y = y_lab, x = "")+ #remove model step name from title
#   theme(plot.title = element_text(size = TitleSize),         axis.text.x = element_text(size = AxisLab),         axis.text.y = element_text (size =AxisLab),         axis.title = element_text(size=AxisText),          legend.text = element_text(size=LegendText),         legend.title = element_text(size=LegendLabText, face="bold"),         legend.box.margin = margin(0,0,0,0)) 
# # print(p2)

y_lab = "Outflow Salt Concentration (million tons/year)"
title = "Lake Powell Average Annual Reservoir/Outflow Salt Concentration" 

p3 <- df3 %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  # ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(plot.title = element_text(size = TitleSize),         axis.text.x = element_text(size = AxisLab),         axis.text.y = element_text (size =AxisLab),         axis.title = element_text(size=AxisText),          legend.text = element_text(size=LegendText),         legend.title = element_text(size=LegendLabText, face="bold"),         legend.box.margin = margin(0,0,0,0)) 
# print(p3)

grid.arrange(p3,ncol=1) #no need for Res Conc while all are well mixed


ggsave(filename = file.path(oFigs,paste0("Powell_Conc_2Panel.png")), width= width, height= height)

################################################################################
#############################  Mead  ##########################################
################################################################################

# Mead Flow 
#-------------------------------------------------------------------------------------

# variables = c("Mead.Inflow","Mead.Storage","Mead.Outflow"
variable = "Mead.Inflow"

df1 <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

df1$Value <- df1$Value/1000000

variable = "Mead.Storage"

df2 <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

df2$Value <- df2$Value/1000000

variable = "Mead.Outflow"
df3 <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

df3$Value <- df3$Value/1000000

y_lab = "Inflow (million acre-ft/year)"
title = "Lake Mead Average Annual Inflow" 

p1 <- df1 %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims_mead_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(plot.title = element_text(size = TitleSize),         axis.text.x = element_text(size = AxisLab),         axis.text.y = element_text (size =AxisLab),         axis.title = element_text(size=AxisText),          legend.text = element_text(size=LegendText),         legend.title = element_text(size=LegendLabText, face="bold"),         legend.box.margin = margin(0,0,0,0)) 
# print(p1)

y_lab = "Storage (million acre-ft)"
title = "Lake Mead Average Annual Storage" 
# ylims <- c(0,20)

p2 <- df2 %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  # ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(plot.title = element_text(size = TitleSize),         axis.text.x = element_text(size = AxisLab),         axis.text.y = element_text (size =AxisLab),         axis.title = element_text(size=AxisText),          legend.text = element_text(size=LegendText),         legend.title = element_text(size=LegendLabText, face="bold"),         legend.box.margin = margin(0,0,0,0)) 
# print(p2)

y_lab = "Outflow (million acre-ft/year)"
title = "Lake Mead Average Annual Outflow" 

p3 <- df3 %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims_mead_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(plot.title = element_text(size = TitleSize),         axis.text.x = element_text(size = AxisLab),         axis.text.y = element_text (size =AxisLab),         axis.title = element_text(size=AxisText),          legend.text = element_text(size=LegendText),         legend.title = element_text(size=LegendLabText, face="bold"),         legend.box.margin = margin(0,0,0,0)) 
# print(p3)

grid.arrange(p1,p2,p3,ncol=1)

ggsave(filename = file.path(oFigs,paste0("Mead_Flow_3Panel.png")), width= width, height= height)

#-------------------------------------------------------------------------------------
# Mead Mass
#-------------------------------------------------------------------------------------


variable = "Mead.Inflow Salt Mass"

df1 <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

df1$Value <- df1$Value/1000000

# variable = "Mead.Reservoir Salt Mass"
# 
# df2 <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise(Value = mean(Value))
# 
# df2$Value <- df2$Value/1000000

variable = "Mead.Outflow Salt Mass"
df3 <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value)) 

df3$Value <- df3$Value/1000000

y_lab = "Inflow Salt Mass (million tons/year)"
title = "Lake Mead Average Annual Inflow Salt Mass" 


p1 <- df1 %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims_mead_inoutmass) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(plot.title = element_text(size = TitleSize),         axis.text.x = element_text(size = AxisLab),         axis.text.y = element_text (size =AxisLab),         axis.title = element_text(size=AxisText),          legend.text = element_text(size=LegendText),         legend.title = element_text(size=LegendLabText, face="bold"),         legend.box.margin = margin(0,0,0,0)) 
# print(p1)

# y_lab = "Reservoir Salt Mass (million tons/year)"
# title = "Lake Mead Average Annual Salt Mass" 
# # 
# p2 <- df2 %>%
#   ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
#   #   
#   scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
#   geom_line() +
#   geom_point() +
#   # ylim(ylims) +
#   scale_linetype_manual(values = lt_scale) +
#   scale_shape_manual(values = pt_scale) +
#   scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
#   labs(title = title, y = y_lab, x = "")+ #remove model step name from title
#   theme(plot.title = element_text(size = TitleSize),         axis.text.x = element_text(size = AxisLab),         axis.text.y = element_text (size =AxisLab),         axis.title = element_text(size=AxisText),          legend.text = element_text(size=LegendText),         legend.title = element_text(size=LegendLabText, face="bold"),         legend.box.margin = margin(0,0,0,0)) 
# # print(p2)

y_lab = "Outflow Salt Mass (million tons/year)"
title = "Lake Mead Average Annual Outflow Salt Mass" 

p3 <- df3 %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims_mead_inoutmass) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(plot.title = element_text(size = TitleSize),         axis.text.x = element_text(size = AxisLab),         axis.text.y = element_text (size =AxisLab),         axis.title = element_text(size=AxisText),          legend.text = element_text(size=LegendText),         legend.title = element_text(size=LegendLabText, face="bold"),         legend.box.margin = margin(0,0,0,0)) 
# print(p3)

grid.arrange(p1,p3,ncol=1)
# grid.arrange(p1,p3,ncol=1) # if don't have latest control file won't have res.salt mass

ggsave(filename = file.path(oFigs,paste0("Mead_Mass_3Panel.png")), width= width, height= height)


#-------------------------------------------------------------------------------------
# Mead Concentration 
#-------------------------------------------------------------------------------------

# variable = "AnnlSlnty_In_Hvr_FWAAC"
# 
# df1 <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise(Value = mean(Value)) 

### FOR WELL MIXED RES CONC = OUTFLOW CONC - do 2 panel for now
# variable = "Mead.Reservoir Salt Concentration"
# 
# df2 <- scen_res %>%
#   dplyr::filter(Variable == variable) %>%
#   dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
#   dplyr::group_by(Scenario, Year) %>%
#   dplyr::summarise(Value = mean(Value)) 
# 
# df2$Value <- df2$Value/1000000

variable = "AnnlSlntyMead_FWAAC"
df3 <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = mean(Value))


# y_lab = "Inflow Salt Concentration (million tons/year)"
# title = "Lake Mead Average Annual Inflow Salt Concentration" 
# 
# p1 <- df1 %>%
#   ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
#   
#   scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
#   geom_line() +
#   geom_point() + 
#   # ylim(ylims) +
#   scale_linetype_manual(values = lt_scale) +
#   scale_shape_manual(values = pt_scale) +
#   scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
#   labs(title = title, y = y_lab, x = "")+ #remove model step name from title
#   theme(plot.title = element_text(size = TitleSize),         axis.text.x = element_text(size = AxisLab),         axis.text.y = element_text (size =AxisLab),         axis.title = element_text(size=AxisText),          legend.text = element_text(size=LegendText),         legend.title = element_text(size=LegendLabText, face="bold"),         legend.box.margin = margin(0,0,0,0)) 
# # print(p1)

# y_lab = "Storage (million acre-ft)"
# title = "Lake Mead Average Annual Storage" 
# # ylims <- c(0,20)
# 
# p2 <- df2 %>%
#   ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
#   
#   scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
#   geom_line() +
#   geom_point() + 
#   # ylim(ylims) +
#   scale_linetype_manual(values = lt_scale) +
#   scale_shape_manual(values = pt_scale) +
#   scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
#   labs(title = title, y = y_lab, x = "")+ #remove model step name from title
#   theme(plot.title = element_text(size = TitleSize),         axis.text.x = element_text(size = AxisLab),         axis.text.y = element_text (size =AxisLab),         axis.title = element_text(size=AxisText),          legend.text = element_text(size=LegendText),         legend.title = element_text(size=LegendLabText, face="bold"),         legend.box.margin = margin(0,0,0,0)) 
# # print(p2)

y_lab = "Outflow Salt Concentration (million tons/year)"
title = "Lake Mead Average Annual Reservoir/Outflow Salt Concentration" 

p3 <- df3 %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  # ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
  theme(plot.title = element_text(size = TitleSize),
        axis.text.x = element_text(size = AxisLab),
        axis.text.y = element_text (size =AxisLab),
        axis.title = element_text(size=AxisText), 
        legend.text = element_text(size=LegendText),
        legend.title = element_text(size=LegendLabText, face="bold"),
        legend.box.margin = margin(0,0,0,0)) 
# print(p3)

grid.arrange(p3,ncol=1) #no need for Res Conc while all are well mixed


ggsave(filename = file.path(oFigs,paste0("Mead_Conc_2Panel.png")), width= width, height= height)


dev.off()
