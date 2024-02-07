##############################################################################
#This script creates powell in,out and PE clouds 
##############################################################################
# library('readxl') #read_xlsx()
library('cowplot') #get_legend()

##############################################################################
### required inputs ###
# scen_res processed by RW dataplyr 
# scens, mycolors, keepscens, Figs 
### optional inputs ###
# yrs, select_variables, 

### refigure
##############################################################################

if (exists("ofigs") && !exists("fig_dir")) {
  fig_dir <- ofigs 
} 

MinMaxLines<-F # T is want dotted line as min max of any given trace 

colorNames <- c(names(scens))
# colorNames <- c("Historical Elevation","Full Hydrology","Early Pluvial Removed Hydrology","Stress Test Hydrology")  

#use mycolors defined by Master rather than old Cloud colors 
# plotColors <-   c("#000000", mycolors,"#8B8682")  # #black, my colors, grey  
plotColors <-   c(mycolors)   

### default var values for cloud plots ### 
if(T){
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
myXLabs <- seq(1990,3000,2)
# myYLabs <- seq(-500,1200,50)

yrs <- startyr:endyr #simplify 

MinMaxLines <- F

}

### data wrangling  ###

if (exists("keepscens")) {
  cloudScen <- keepscens
  cloudLabs <- keepscens
  names(plotColors) <- names(keepscens) # colorNames #### does this work? 
  scen_res <- scen_res %>% #filter out scens you don't want to keep for plots
    dplyr::filter(Scenario %in% keepscens)
} else if (exists("scens")){
  cloudScen <- names(scens)
  cloudLabs <- names(scens)
  names(plotColors) <- names(scens) # colorNames #### does this work? 
} else {stop("missing keepscens or scens")}

# zz_inout <- scen_res %>%
#   dplyr::filter(Year %in% yrs, Variable %in% c("Powell.InflowCY","Powell.InflowWY",
#                                                "Powell.OutflowCY","Powell.OutflowWY",
#                                                "Mead.InflowWY","Mead.OutflowCY")) %>%
#   mutate(Value = Value/1000000) #convert to MAF after we convert to AF  
# zz_pe <- scen_res %>%
#   dplyr::filter(Year %in% yrs, Variable %in% c("Powell.EOCYPoolElevation", "Mead.EOCYPoolElevation" #new rw_agg slot name
#   )) 



variables <- unique(scen_res$Variable)

### monthly to annual for PE and Outflows handled differently ###
# zz_inout <- scen_res %>%
#   dplyr::filter(Variable %in% c("Navajo.Outflow", "Bluff_AF","Arch_250","Bluff_Less_500")) %>%
#   dplyr::group_by(Scenario,Variable,TraceNumber,Year) %>% #by leaving Variable in I keep the name in the resulting df
#   dplyr::summarise(Value = sum(Value)) #sum to annual CY
# 
# zz_pe <- scen_res %>%
#   dplyr::filter(MonthNum == 9, Variable %in% c("Navajo.PoolElevation")) %>%
#   mutate(Variable = "Navajo.WYPoolElevation")
# 
# zz_pe <- zz_pe[,c("Scenario","Variable","TraceNumber","Year","Value")]
# 
# zz_all <-rbind.data.frame(zz_inout,zz_pe)


### monthly to annual all sites ###
if((zz_all$Month %in% month.name)[1]){
  zz_all <- zz_all %>%
    dplyr::group_by(Scenario,Variable,TraceNumber,Year) %>% #by leaving Variable in I keep the name in the resulting df
    dplyr::summarise(Value = sum(Value)) #sum to annual CY
}

if (exists("yrs")) {
  zz_all <- scen_res %>%
    dplyr::filter(Year %in% yrs)
} else {
  zz_all <- scen_res} 

if (exists("select_variables")) {
  zz_all <- zz_all %>%
    dplyr::filter(Variable %in% select_variables)
} 




zz_all <- zz_all %>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 


variables <- unique(zz_all$Variable)


# # debug
# head(zz_all)
# unique(zz_all$Year)

# # Adding factors so ggplot does not alphebetize legend
zz_all$Scenario = factor(zz_all$Scenario, levels=colorNames)

#if running code for second time
custom_titles <- variables
custom_ylabs <- rep("",length(variables))

#if running code for first time 
if (exists("custom_ylabs")) {
  custom_ylabs <- custom_ylabs
} else {
  # custom_ylabs <- variables} 
  custom_ylabs <- rep("",length(variables)) }


if (exists("custom_titles")) {
  custom_titles <- custom_titles
} else {
  custom_titles <- variables } 

NumCrit <- NA
# powtiers <- F #currently disabled in code Cloud_plot_woHist.R
MinMaxLines <- T

#-------------------------------------------------------------------------------------
# ++++++++++++++++++++++++++Cloud Plots+++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------------

## create a pdf  
# pdf(file.path(ofigs,paste0("Clouds_",Figs,".pdf")), width = widths[1],height = heights[1]) #width= width, height= height)
pdf(file.path(ofigs,paste0("Clouds_",Figs,"Clouds_",variables[1],".pdf")), width = widths[1],height = heights[1]) #width= width, height= height)



for (i in 1:length(variables)) {
  variable = variables[i]
  y_lab = custom_ylabs[i]
  title = custom_titles[i]
  subtitle = ""
  # ylims <- c(7,12)
  
  source("code/Cloud_plot_woHist.R")
  # source("code/Cloud_plot_woHistv2.R")
  
}


dev.off()

