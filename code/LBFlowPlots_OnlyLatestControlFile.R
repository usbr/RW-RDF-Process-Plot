##############################################################################
#This script creates LB FlowMassBal figure
# Only works with latest control file as of 1/13/2023 
##############################################################################

# rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],"NF.rdf")))


# set up 
slots = c(
  "ImperialNF.Inflow","LittlefieldNF.Inflow","DavisNF.Inflow","AlamoNF.Inflow",
  "ParkerNF.Inflow","HooverNF.Inflow","CameronNF.Inflow","GrandCanyonNF.Inflow"         
)
rdfs = c(rep('NF.rdf', length(slots)))


slots = c(
  "AboveImperialDamColoradoR:GainsOnColoRAboveImperialDam.Local Inflow",
  # "LittlefieldNF.Inflow",
  "CoRivMeadToMohave:GainsAboveDavis.Local Inflow",
  "CoRivMohaveToHavasu:BillWilliamsRiver.Local Inflow",
  "CoRivMohaveToHavasu:GainsAboveParker.Local Inflow",
  "CoRivVirginToMead:GainsAboveHoover.Local Inflow",
  # "CameronNF.Inflow",
  "CoRivLittleCOToVirgin:GainsAboveGC.Local Inflow"         
)
rdfs = c(rep('Rch.rdf', length(slots)))

variables = gsub(" ","",slots)

# slots/agg to read
rwa1 <- rwd_agg(data.frame(
  file = rdfs,
  slot = slots, 
  period = rep("cy", length(slots)),
  summary = rep("sum", length(slots)),
  eval = rep(NA, length(slots)),
  t_s = rep(NA, length(slots)),
  variable = slots,
  stringsAsFactors = FALSE
))


#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)

# unique(scen_res$Variable) #check variable names 

## Divide Values by 1,000,000 to present data in Million of Tons/Year

scen_res$Value=(scen_res$Value)/1000000

i=1

pdf(file.path(oFigs,paste0("OldOutputNFflowgains_",Figs,".pdf")), width= width, height= height)


# for (i in 1:length(slots)) {
#   variable = variables[i]
#   
#   scen_res %>% 
#     dplyr::filter(Variable == variable) %>% #don't need to do this
#     dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
#     dplyr::group_by(Scenario, Year,Variable) %>%
#     dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
#                      'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
#                      'MinOut' = min(Value),'MaxOut' = max(Value)) %>%
#     ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + 
#     theme_light() +
#     geom_line() +
#     geom_point() +
#     # ylim(ylims) +
#     scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
#     labs(title = variable, y = y_lab, x = "Year")+
#     theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
#   print(p)
#   
#   ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)
# }
# 
# dev.off()


for(i in 1:length(slots)){
  df_x <- scen_res %>% dplyr::filter(Variable == slots[i]) 
  
  if(i==1){
    df_lbnf <- df_x
  } else {
    df_lbnf$Value = df_lbnf$Value + df_x$Value
  }
  
}

variable = "LBNatInflow"

df_lbnf <- df_lbnf %>% 
  mutate(Variable = variable) %>% #don't need to do this
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year,Variable) %>%
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 

p <- df_lbnf %>%
  ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + 
  theme_light() +
  geom_line() +
  geom_point() +
  # ylim(ylims) +
  scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
  labs(title = "LB Natural Inflow", y = y_lab, x = "Year")+
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(p)

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

write.csv(df_lbnf,file = paste0(oFigs,'/','Mean_',variable,'.csv'))

dev.off()





library('readxl') #read_xlsx()
library('cowplot') #get_legend()

### Cloud Plot 

MinMaxLines<-F # T is want dotted line as min max of any given trace 

# Parameters for cloud plot customization (line thicknesses, text size, etc.)
#Text
TitleSize = 13
AxisText = 11
LegendLabText = 9.5

AxisLab = 9
LabSize = 2.9
LegendText = 8
# 
#Lines
IGStartLine = .8
OpsLines = 1
Medians = 1
GridMaj = .25
GridMin = .25
# 
# #Y axis limits
# yaxmin = floor(min(zz$Min)/50)*50
# yaxmax = ceiling(max(zz$Max)/50)*50
# 
# #Other
LegendWidth = 1
LegendHeight = 2.5

# Set tick marks for x and y axis
myXLabs <- seq(1990,3000,5)
######FUTURE DELVEOPMENT ####### 
# myYLabs <- seq(-500,1200,50) #not being used in Cloud_Plot_woHist.R
# myYLabs_minor

yrs <- startyr:endyr #simplify 

cloudScen <- names(scens)
cloudLabs <- names(scens)

#use mycolors defined by Master rather than old Cloud colors 
plotColors <-   mycolors  # #black, my colors, grey  
# names(plotColors) <- colorNames ### Don't need this 

### Read Data ###

zz_all <- df_lbnf # %>%

# # Adding factors so ggplot does not alphebetize legend
zz_all$Scenario = factor(zz_all$Scenario, levels=names(scens))

## create a pdf  
pdf(file.path(oFigs,paste0("LBFlowMassBal_Clouds_",Figs,".pdf")), width= width, height= height)
# MinMaxLines=",MinMaxLines, ### could add this if want to indicate if lines are added 

NumCrit <- HistMin <- NA
variable = variable
y_lab = "Salt Mass (million tons/yr)"
title = "LB Natural Inflow"
subtitle = NA
ylims <- c(NA,NA)

source("code/Cloud_plot_woHist.R")

ggsave(filename = file.path(oFigs,paste0("Cloud_",variable,".png")), width= width, height= height)

dev.off()




### Individual NF gains 
zz_all <- scen_res %>% 
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year,Variable) %>%
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 

# # Adding factors so ggplot does not alphebetize legend
zz_all$Scenario = factor(zz_all$Scenario, levels=names(scens))


slots = c(
  "ImperialNF.Inflow",
  "ParkerNF.Inflow","DavisNF.Inflow","HooverNF.Inflow"        
)

pdf(file.path(oFigs,paste0("LB_NF_flowIn_Clouds_",Figs,".pdf")), width= width, height= height)


for (i in 1:length(slots)) {
  variable = slots[i]
  
  NumCrit <- HistMin <- NA
  y_lab = "Flow Gain (MAF/yr)"
  title = variable
  subtitle = NA
  ylims <- c(NA,NA)
  
  source("code/Cloud_plot_woHist.R")
  
  ggsave(filename = file.path(oFigs,paste0("Cloud_",variable,".png")), width= width, height= height)
}

dev.off()

#### Inflow Salt Mass

slots = c(
  "ImperialNF.Inflow Salt Mass",
  "ParkerNF.Inflow Salt Mass","DavisNF.Inflow Salt Mass","HooverNF.Inflow Salt Mass"        
)
rdfs = c(rep('salt.rdf', length(slots)))

# slots/agg to read
rwa1 <- rwd_agg(data.frame(
  file = rdfs,
  slot = slots, 
  period = rep("cy", length(slots)),
  summary = rep("sum", length(slots)),
  eval = rep(NA, length(slots)),
  t_s = rep(NA, length(slots)),
  variable = slots,
  stringsAsFactors = FALSE
))


#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)

scen_res$Value=(scen_res$Value)/1000000

### individual NF gains 
zz_all <- scen_res %>% 
  dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Year,Variable) %>%
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 

# # Adding factors so ggplot does not alphebetize legend
zz_all$Scenario = factor(zz_all$Scenario, levels=names(scens))

pdf(file.path(oFigs,paste0("LB_NF_MassIn_Clouds_",Figs,".pdf")), width= width, height= height)


for (i in 1:length(slots)) {
  variable = slots[i]
  
  NumCrit <- HistMin <- NA
  y_lab = "Salt Mass Gain (million tons/yr)"
  title = variable
  subtitle = NA
  ylims <- c(NA,NA)
  
  source("code/Cloud_plot_woHist.R")
  
  ggsave(filename = file.path(oFigs,paste0("Cloud_",variable,".png")), width= width, height= height)
}

dev.off()

