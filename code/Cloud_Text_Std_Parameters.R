library('readxl') #read_xlsx()
library('cowplot') #get_legend()

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