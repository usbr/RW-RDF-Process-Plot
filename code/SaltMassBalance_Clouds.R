##############################################################################
#This script creates clouds 
##############################################################################

library('readxl') #read_xlsx()
library('cowplot') #get_legend()

######READ IN and make scen_res USING MasBalAnn.R#######

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
# myYLabs <- seq(-500,1200,50) #not being used in Cloud_Plot_woHist.R

yrs <- startyr:endyr #simplify 

cloudScen <- names(scens)
cloudLabs <- names(scens)

#use mycolors defined by Master rather than old Cloud colors 
plotColors <-   mycolors  # #black, my colors, grey  
# names(plotColors) <- colorNames ### Don't need this 

### Read Data ###

zz_all <- scen_res %>%
  # dplyr::filter(Year %in% yrs, Variable %in% c("UB_Natural_Inflow",  #for some reason this was causing it to error, don't need it
  #                                              "LB_Natural_Inflow",
  #                                              "LB_NaturalSalt_InflowOnly")) #%>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 

# # Adding factors so ggplot does not alphebetize legend
zz_all$Scenario = factor(zz_all$Scenario, levels=names(scens))

## create a pdf  
pdf(file.path(oFigs,paste0("SaltMassBal_Clouds_",Figs,".pdf")), width= width, height= height)
# MinMaxLines=",MinMaxLines, ### could add this if want to indicate if lines are added 

NumCrit <- HistMin <- NA
variable = "UB_Natural_Inflow"
y_lab = "Salt Mass (million tons/yr)"
title = "UB Natural Inflow Salt Mass"
subtitle = NA
ylims <- c(NA,NA)

source("code/Cloud_plot_woHist.R")

NumCrit <- HistMin <- NA
variable = "LB_Natural_Inflow"
y_lab = "Salt Mass (million tons/yr)"
title = "LB Total Natural Inflow Salt Mass"
subtitle = NA
ylims <- c(NA,NA)

source("code/Cloud_plot_woHist.R")

### remove UB salt inflows ###

NumCrit <- HistMin <- NA
variable = "LB_NaturalSalt_InflowOnly"
y_lab = "Salt Mass (million tons/yr)"
title = "LB Only Natural Inflow Salt Mass"
subtitle = NA
ylims <- c(NA,NA)

source("code/Cloud_plot_woHist.R")

# # don't need the below any more since now is handled by Custom_MassBalAnn.R
# df_temp <- zz_all %>% filter(Variable == variable)
# write.csv(df_temp,file = paste0(data_dir,'/','Stats_',variable,'.csv'))


dev.off()


