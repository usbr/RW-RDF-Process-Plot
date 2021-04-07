



MinMaxLines<-F # T is want dotted line as min max of any given trace 

colorNames <- c(names(scens))
# colorNames <- c("Historical Elevation","Full Hydrology","Early Pluvial Removed Hydrology","Stress Test Hydrology")  

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

## create a pdf  
pdf(file.path(ofigs,paste0("FGClouds_",Figs,".pdf")), width = widths[1],height = heights[1]) #width= width, height= height)


### Read Data ###

#### WY ##########


zz_all <- annual_cy %>%
 
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 

# # debug
# head(zz_all)
unique(zz_all$Variable)
# unique(zz_all$Year)
 
# # Adding factors so ggplot does not alphebetize legend
zz_all$Scenario = factor(zz_all$Scenario, levels=colorNames)


NumCrit <- NA
variable = "FlamingGorge.Outflow"
y_lab = "Outflow (MAF/yr)"
title = "Flaming Gorge CY Outflow" 
subtitle = ""

source("code/Cloud_plot_woHist.R")

NumCrit <- NA
variable = "GreenNearJensen.Gage Inflow"
y_lab = "Outflow (MAF/yr)"
title = "Jensen CY Outflow" 
subtitle = ""

source("code/Cloud_plot_woHist.R")



#### WY ##########

zz_all <- annual_wy %>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, WY,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 
names(zz_all)[2] <- "Year" #change WY to Year so generic code works with it 
names(zz_all)

# # Adding factors so ggplot does not alphebetize legend
zz_all$Scenario = factor(zz_all$Scenario, levels=colorNames)

NumCrit <- NA
variable = "FlamingGorge.Outflow" #this slot was in CFS so didn't work to use rw_agg. had to build CY from monthly
y_lab = "Outflow (MAF/yr)"
title = "Flaming Gorge WY Outflow"
subtitle = ""

source("code/Cloud_plot_woHist.R")

NumCrit <- NA
variable = "GreenNearJensen.Gage Inflow" #this slot was in CFS so didn't work to use rw_agg. had to build CY from monthly
y_lab = "Outflow (MAF/yr)"
title = "Jensen WY Outflow"
subtitle = ""

source("code/Cloud_plot_woHist.R")




dev.off()

