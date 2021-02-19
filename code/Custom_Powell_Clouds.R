##############################################################################
#This script creates concentration clouds 
##############################################################################

#agg file specifying which slots

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# list.files(file.path(scen_dir,scens[2]))
# 
# rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[2],"WQANN.rdf")))

library('readxl') #read_xlsx()
library('cowplot') #get_legend()

######READ IN and make scen_res USING other process code#######

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++Below Powell+++++++++++++++++++++++++++++++++++++
#SEE main_TriRvw_hack.R and custom_cloud_function.R in Process CRSS Res if want to get fancier 
### Plotting Parameters ###

MinMaxLines<-F # T is want dotted line as min max of any given trace 

colorNames <- c(names(scens))
# colorNames <- c("Historical Elevation","Full Hydrology","Early Pluvial Removed Hydrology","Stress Test Hydrology")  

#####UPDATE THIS EVERY TIME #### OR UPDATE LATER IN DOCUMENT USING 

# Parameters for cloud plot customization (line thicknesses, text size, etc.)
# Have been pulled out for convenience
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

# # Set tick marks for x and y axis
# myXLabs <- seq(1990,3000,5)
# myYLabs <- seq(-500,1200,50)

yrs <- startyr:endyr #simplify 

#use mycolors defined by Master rather than old Cloud colors 
# plotColors <-   c("#000000", mycolors,"#8B8682")  # #black, my colors, grey  
plotColors <-   c(mycolors)   


if (exists("keepscens")) {
  cloudScen <- keepscens
  cloudLabs <- keepscens
  names(plotColors) <- names(keepscens) # colorNames #### does this work? 
} else if (exists("scens")){
  cloudScen <- names(scens)
  cloudLabs <- names(scens)
  names(plotColors) <- names(scens) # colorNames #### does this work? 
} else {stop("missing keepscens or scens")}

### Read Data ###

# unique(scen_res$Scenario)
# unique(scen_res$Variable)

### Plot List for Coordinated Res Operations ###
# Powell CY Inflow
# Powell WY Inflow
# Powell EOCY PE (set release tier) - Process CRSS Res does this already 
# Powell release tier bar plot @sbaker2423
# Powell annual outflow (WY)
# Mead annual inflow (WY)
# Mead annual inflow CY
# Mead PE (CY for Mead for setting operating condition)
# Mead shortage/surplus bar plot @sbaker2423
# Mead release (CY)

scen_res <- scen_res %>% #filter out scens you don't want to keep for plots
  dplyr::filter(Scenario %in% keepscens)

zz_inout <- scen_res %>%
  dplyr::filter(Year %in% yrs, Variable %in% c("Powell.InflowCY","Powell.InflowWY",
                                               "Powell.OutflowCY","Powell.OutflowWY",
                                               "Mead.InflowWY","Mead.OutflowCY")) %>%
  mutate(Value = Value/1000000) #convert to MAF after we convert to AF  
zz_pe <- scen_res %>%
  dplyr::filter(Year %in% yrs, Variable %in% c("Powell.Pool Elevation", "Mead.Pool Elevation" #both EOCY - old names 
                                               # "Powell.EOCYPoolElevation", "Mead.EOCYPoolElevation" #new rw_agg slot name
                                               )) 
zz_all <- rbind.data.frame(zz_inout,zz_pe) %>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 

# # debug
# head(zz_all)
# unique(zz_all$Variable)
# unique(zz_all$Year)

# #  Pulling historical SLOAD data
# hist <- read_xlsx(file.path(getwd(),'data/HistSLOAD.xlsx'))
# 
# # Formatting data frame to match zz_all
# hist$Scenario <- 'Historical SLOAD'
# hist$Mean <-hist$Med <- hist$Min <- hist$Max <- hist$MinOut <- hist$MaxOut <- hist$Value
# hist <- within(hist, rm(Value))
# hist <- hist[c("Scenario","Year","Variable","Mean","Med","Min","Max","MinOut","MaxOut")]
# 
# # Appending historical data
# zz_all <- bind_rows(hist,zz_all)


# 
# # Adding factors so ggplot does not alphebetize legend
zz_all$Scenario = factor(zz_all$Scenario, levels=colorNames)

# # Generating labels for the lines in ggplot
# histLab = "Historical SLOAD"
# names(histLab) = "Historical SLOAD"
# histLab = append(histLab, cloudLabs)

# # Read in Reclamation logo png - CF: NOT ENABLED
# im <- load.image('logo/660LT-TK-flush.png')
# im_rast <- grid::rasterGrob(im, interpolate = T)

## create a pdf  
pdf(file.path(oFigs,paste0("PowellClouds_",Figs,".pdf")), width = widths[1],height = heights[1]) #width= width, height= height)

### Means ###

#-------------------------------------------------------------------------------------
# ++++++++++++++++++++++++++Lees Ferry+++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------------




NumCrit <- NA
variable = "Powell.InflowCY"
y_lab = "Inflow (MAF/yr)"
title = "Lake Powell CY Inflow" 
subtitle = ""
# ylims <- c(7,12)

source("code/Cloud_plot_woHist.R")

source("code/Cloud_plot_woHist.R")

NumCrit <- NA
variable = "Powell.OutflowCY"
y_lab = "Outflow (MAF/yr)"
title = "Lake Powell CY Outflow" 
subtitle = ""
# ylims <- c(7,12)

source("code/Cloud_plot_woHist.R")

NumCrit <- 3525
variable = "Powell.Pool Elevation"
# variable = "Powell.EOCYPoolElevation" #new rw_agg name
y_lab = "Elevation (ft)"
title = "Lake Powell EOCY Pool Elevation" 
subtitle = ""
# ylims <- c(7,12)

source("code/Cloud_plot_woHist.R")


zz_all <- zz_all %>%
  dplyr::filter(Year %in% 2022:2040) #don't have complete WY for 2021

NumCrit <- NA
variable = "Powell.InflowWY"
y_lab = "Inflow (MAF/yr)"
title = "Lake Powell WY Inflow" 
subtitle = ""
# ylims <- c(7,12)

source("code/Cloud_plot_woHist.R")

NumCrit <- NA
variable = "Powell.OutflowWY"
y_lab = "Outflow (MAF/yr)"
title = "Lake Powell WY Outflow" 
subtitle = NA
# ylims <- c(7,12)

source("code/Cloud_plot_woHist.R")

#-------------------------------------------------------------------------------------
# ++++++++++++++++++++++++++Below Mead+++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#------------------------------Below Parker-------------------------------------------------------
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#-------------------------------At Imperial------------------------------------------------------
#-------------------------------------------------------------------------------------




