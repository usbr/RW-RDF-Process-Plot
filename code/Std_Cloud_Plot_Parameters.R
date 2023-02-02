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
