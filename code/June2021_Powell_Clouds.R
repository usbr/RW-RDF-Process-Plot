##############################################################################
#This script creates powell in,out and PE clouds 
##############################################################################
library('readxl') #read_xlsx()
library('cowplot') #get_legend()
library('scales') #comma in y axis label 

figstats <- file.path(ofigs,"Stats") 
if (!file.exists(figstats)) {
  message(paste('Creating folder:', figstats))
  dir.create(figstats)
}
message('Stats will be saved to: ', figstats)

widths <- 11
heights <- 6 #fits under title text in pptx 

MinMaxLines<-F # T is want dotted line as min max of any given trace 

plotColors <- c("#407ec9" , "#6b8f00", "#9a3324" ) #Reclamation blue, green, red
plotColors <- c( "#6b8f00", "#9a3324" ) #green, red
colorNames <- c(names(scens))
exc_month <-12

#Text
TitleSize = 13
AxisText = 11
LegendLabText = 9.5

AxisLab = 9
LabSize = 2.9
LegendText = 8
 
#Lines
IGStartLine = .8
OpsLines = 1
Medians = 1
GridMaj = .25
GridMin = .25

# #Y axis limits
# yaxmin = floor(min(zz$Min)/50)*50
# yaxmax = ceiling(max(zz$Max)/50)*50
# 
# #Other
LegendWidth = 1
LegendHeight = 2.5

# # Set tick marks for x and y axis
# myXLabs <- seq(1990,3000,2)
# myYLabs <- seq(-500,1200,50)

yrs <- startyr:endyr #simplify 


### Read Data ###
zz_inout <- scen_res %>%
  dplyr::filter(Variable %in% c('FlamingGorge.Outflow', 'BlueMesa.Outflow','Navajo.Outflow','Powell.Inflow')) %>%
  dplyr::group_by(Variable,ScenarioGroup,Scenario,TraceNumber,Year) %>%
  dplyr::summarise(Value = sum(Value)) %>% #inflow and outflow this needs to be sum(Value)
  mutate(Value = Value/1000) #convert to MAF after we convert to AF
head(zz_inout)
unique(zz_inout$Variable)

zz_pe <- scen_res %>%
  dplyr::filter(Variable %in% c('FlamingGorge.PE', 'BlueMesa.PE','Navajo.PE','Powell.PE')) %>% #'Powell.Storage' didn't process 
  dplyr::filter(MonthNum %in% exc_month)   # EOCY 
zz_pe <- zz_pe[,names(zz_inout)]
head(zz_pe)

zz_stor <- scen_res %>%
  dplyr::filter(Variable %in% c('FlamingGorge.Storage', 'BlueMesa.Storage','Navajo.Storage')) %>% #'Powell.Storage' didn't process 
  mutate(Value = Value/1000) %>% #convert to MAF after we convert to AF
  dplyr::filter(MonthNum %in% exc_month) # EOCY 
head(zz_stor)
zz_stor <- zz_stor[,names(zz_inout)]
# summary(zz_stor)

zz_all <- rbind.data.frame(zz_inout,zz_pe,zz_stor) %>%
 # compute the 10/50/90 and aggregate by start month
dplyr::group_by(ScenarioGroup, Variable, Year) %>% #don't use scenario here 
dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                 'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                 'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 

zz_all$Scenario = zz_all$ScenarioGroup
unique(zz_all$Variable)
# summary(zz_all)

# # Adding factors so ggplot does not alphebetize legend
# zz_all$Scenario = factor(zz_all$Scenario, levels=colorNames)

## create a pdf  
pdf(file.path(ofigs,paste0(Hydro,"_Clouds.pdf")), width = widths[1],height = heights[1]) #width= width, height= height)

### Means ###

#-------------------------------------------------------------------------------------
# ++++++++++++++++++++++++++Plot+++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------------
NumCrit <- NA #3490
powtiers <- F
variable = "Powell.Inflow"
y_lab = "Annual Inflow (kaf/yr)"
title = "Lake Powell" 
source("code/Cloud_plot_woHistv2.R")

NumCrit <- NA #3490
powtiers <- T
variable = "Powell.PE"
y_lab = "EOCY Elevation (ft)"
title = "Lake Powell" 
source("code/Cloud_plot_woHistv2.R")

# powtiers <- F
# variable = "Powell.Storage" ## you didn't grab this data you silly goose 
# y_lab = "EOCY Storage (kaf)"
# title = "Lake Powell" 
# source("code/Cloud_plot_woHistv2.R")

NumCrit <- NA
powtiers <- F
variable = "FlamingGorge.Storage"
y_lab = "EOCY Storage (kaf)"
title = "Flaming Gorge"
# ylims <- c(7,12)
source("code/Cloud_plot_woHistv2.R")

variable = "FlamingGorge.PE"
y_lab = "EOCY Elevation (ft)"
subtitle = ""
source("code/Cloud_plot_woHistv2.R")

variable = "FlamingGorge.Outflow"
y_lab = "Annual Release (kaf/yr)"
subtitle = ""
source("code/Cloud_plot_woHistv2.R")

NumCrit <- NA
powtiers <- F
variable = "BlueMesa.Storage"
y_lab = "EOCY Storage (kaf)"
title = "Blue Mesa" 
subtitle = ""
# ylims <- c(7,12)
source("code/Cloud_plot_woHistv2.R")

variable = "BlueMesa.PE"
y_lab = "EOCY Elevation (ft)"
title = "Blue Mesa" 
subtitle = ""
source("code/Cloud_plot_woHistv2.R")

variable = "BlueMesa.Outflow"
y_lab = "Annual Release (kaf/yr)"
title = "Blue Mesa" 
subtitle = ""
source("code/Cloud_plot_woHistv2.R")

NumCrit <- NA
powtiers <- F
variable = "Navajo.Storage"
y_lab = "EOCY Storage (kaf)"
title = "Navajo" 
subtitle = ""
# ylims <- c(7,12)
source("code/Cloud_plot_woHistv2.R")

variable = "Navajo.PE"
y_lab = "EOCY Elevation (ft)"
title = "Navajo" 
subtitle = ""
source("code/Cloud_plot_woHistv2.R")

variable = "Navajo.Outflow"
y_lab = "Annual Release (kaf/yr)"
title = "Navajo" 
subtitle = ""
source("code/Cloud_plot_woHistv2.R")

dev.off()

## additional plots









dev.off()

