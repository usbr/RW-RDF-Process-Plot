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

lt_scale <- rep(1, 3)
pt_scale <- rep(1, 3)
mycolors <- c("#1F78B4","#33A02C","#E31A1C")

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

# unique(scen_res2$Variable) #check variable names 
## Divide Values by 1,000,000 to present data in Million of Tons/Year or MAF
scen_res2$Value=(scen_res2$Value)/1000000

#add scenario names to line, point and color scales
names(lt_scale) <- unique(scen_res$Scenario)
names(pt_scale) <- unique(scen_res$Scenario)
names(mycolors) <- unique(scen_res$Scenario)


# The blue gradient background is "graph trash" 
# # make custom axis shading, don't use for now doesn't look good with plotte pallette
# g <- rasterGrob(scales::alpha(blues9, 0.1), width=unit(1,"npc"), height = unit(1,"npc"), 
#                 interpolate = TRUE) #alpha is the transperency  

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## create a pdf  
pdf(file.path(oFigs,paste0("WQAnn_Boxplots_",Figs,".pdf")), width= width, height= height)
pdf(file.path(oFigs,paste0("WQAnn_MeanMedian_",Figs,".pdf")), width= width, height= height)


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
  dplyr::group_by(Scenario, Year) #%>%

# p <- df %>%
#   ggplot(aes(x = factor(Year), y = Value, color = Scenario)) +
#   stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
#   ylim(ylims) +
#   scale_color_manual(values = mycolors) +
#   labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
#   theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
# print(p)

dfmean <- df %>%
  dplyr::summarise(Value = mean(Value)) 

dfmed <- df %>%
  dplyr::summarise(Value = median(Value))

p <- dfmean %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  geom_line(data = dfmed, mapping = aes(x = factor(Year), y = Value, color = Scenario, group = Scenario), linetype = "dashed") +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle, caption = "Mean = Solid, Median = Dashed")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "Powell.Outflow"
y_lab = "Outflow (million acre-ft/year)"
title = "Lake Powell Average Annual Outflow" 
# subtitle = "Average Annual Concentration Comparision"
ylims <- c(0,15)


df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) #%>%

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  # ylim(ylims) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

df <- df %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "Powell.Outflow Salt Mass"
y_lab = "Outflow Salt Mass (million tons/year)"
title = "Lake Powell Average Annual Outflow Salt Mass" 
# subtitle = "Average Annual Concentration Comparision"
ylims <- c(0,10)


df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) #%>%

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  ylim(ylims) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

df <- df %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

# ++++++++++++++++++++++++++Below Mead+++++++++++++++++++++++++++++++++++++

NumCrit <- data.frame(yintercept=723)
variable = "AnnlSlntyHvr_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River below Hoover Dam" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(545,750)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) #%>%

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  ylim(ylims) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

dfmean <- df %>%
  dplyr::summarise(Value = mean(Value)) 

dfmed <- df %>%
  dplyr::summarise(Value = median(Value))

p <- dfmean %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  geom_line(data = dfmed, mapping = aes(x = factor(Year), y = Value, color = Scenario, group = Scenario), linetype = "dashed") +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle, caption = "Mean = Solid, Median = Dashed")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "Mead.Outflow"
y_lab = "Outflow (million acre-ft/year)"
title = "Lake Mead Average Annual Outflow" 
# subtitle = "Average Annual Concentration Comparision"
ylims <- c(0,15)


df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) #%>%

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  ylim(ylims) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

df <- df %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "Mead.Outflow Salt Mass"
y_lab = "Outflow Salt Mass (million tons/year)"
title = "Lake Mead Average Annual Outflow Salt Mass" 
# subtitle = "Average Annual Concentration Comparision"
ylims <- c(0,10)


df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) #%>%

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  ylim(ylims) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

df <- df %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
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
  dplyr::group_by(Scenario, Year) #%>%

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  ylim(ylims) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

dfmean <- df %>%
  dplyr::summarise(Value = mean(Value)) 

dfmed <- df %>%
  dplyr::summarise(Value = median(Value))

p <- dfmean %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  geom_line(data = dfmed, mapping = aes(x = factor(Year), y = Value, color = Scenario, group = Scenario), linetype = "dashed") +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle, caption = "Mean = Solid, Median = Dashed")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "Havasu.Outflow"
y_lab = "Outflow (million acre-ft/year)"
title = "Lake Havasu Average Annual Outflow" 
# subtitle = "Average Annual Concentration Comparision"
ylims <- c(0,10)


df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) #%>%

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  ylim(ylims) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

df <- df %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "Havasu.Outflow Salt Mass"
y_lab = "Outflow Salt Mass (million tons/year)"
title = "Lake Havasu Average Annual Outflow Salt Mass" 
# subtitle = "Average Annual Concentration Comparision"
ylims <- c(0,10)


df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) #%>%

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  ylim(ylims) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

df <- df %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------At Imperial------------------------------------------------------

NumCrit <- data.frame(yintercept=879)
variable = "AnnlSlntyImprl_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River above Imperial Dam" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(675,900)

df <- scen_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) #%>%

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  ylim(ylims) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

dfmean <- df %>%
  dplyr::summarise(Value = mean(Value)) 

dfmed <- df %>%
  dplyr::summarise(Value = median(Value))

p <- dfmean %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  geom_line(data = dfmed, mapping = aes(x = factor(Year), y = Value, color = Scenario, group = Scenario), linetype = "dashed") +
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle, caption = "Mean = Solid, Median = Dashed")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "AboveImperialDamColoradoR.Outflow"
y_lab = "Outflow (million acre-ft/year)"
title = "Imperial Dam Average Annual Outflow" 
# subtitle = "Average Annual Concentration Comparision"
ylims <- c(0,10)


df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) #%>%

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  ylim(ylims) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

df <- df %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

#-------------------------------------------------------------------------------------

variable = "AboveImperialDamColoradoR.Outflow Salt Mass"
y_lab = "Outflow Salt Mass (million tons/year)"
title = "Imperial Dam Average Annual Outflow Salt Mass" 
# subtitle = "Average Annual Concentration Comparision"
ylims <- c(0,10)


df <- scen_res2 %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year) #%>%

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  ylim(ylims) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

df <- df %>%
  dplyr::summarise(Value = mean(Value)) 

p <- df %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
  
  scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
  geom_line() +
  geom_point() + 
  ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle)+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)



dev.off()


