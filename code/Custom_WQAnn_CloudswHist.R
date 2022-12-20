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

######READ IN and make scen_res USING REGULAR WQAnn or PE code#######

# rw_agg_file <- "WQAnn.csv" #doesn't include outflow
# 
# #read agg file specifying which slots
# # # NEW files are annual slots so use AsIs
# rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE))
# # # # Old files from 2017 review are monthly so use EOCY 
# 
# #rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
# scen_res <- rw_scen_aggregate(
#   scens,
#   agg = rwa1,
#   scen_dir = scen_dir
# )

# unique(scen_res$Variable) #check variable names 

## New ##
## Make a dataframe for Outflow and Outflow Salt Mass at each of the Numeric Criteria Points and Lees
#agg file specifying which slots

# #####################################################################################################################################################################
# ## For old TriRvw 2017 Runs to plot you Outflow and Outflow Salt Mass you have to grab these slots from Salt.rdf since the output.control changed throughtout time#
# ##################################################################################################################################################################

# #add scenario names to line, point and color scales
# names(lt_scale) <- unique(scen_res$Scenario)
# names(pt_scale) <- unique(scen_res$Scenario)
# names(mycolors) <- unique(scen_res$Scenario)


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# ++++++++++++++++++++++++++Below Powell+++++++++++++++++++++++++++++++++++++
#SEE main_TriRvw_hack.R and custom_cloud_function.R in Process CRSS Res if want to get fancier 


# library('cowplot') #get_legend()

### Plotting Parameters ###

MinMaxLines<-F # T is want dotted line as min max of any given trace 


colorNames <- c("Historical SLOAD",names(scens))  
# colorNames <- c("Historical Elevation","Full Hydrology","Early Pluvial Removed Hydrology","Stress Test Hydrology")  

#####UPDATE THIS EVERY TIME #### OR UPDATE LATER IN DOCUMENT USING 

#source("code/Std_Cloud_Plot_Parameters.R")

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

# Set tick marks for x and y axis
myXLabs <- seq(1990,3000,5)
myYLabs <- seq(-500,1200,50)

yrs <- startyr:endyr #simplify 

cloudScen <- names(scens)
cloudLabs <- names(scens)

### Read Data ###

### Read Data ###


zz_all <- scen_res %>%
  dplyr::filter(Year %in% yrs, Variable %in% c("AnnlSlntyLsFrry_FWAAC",
                                                "AnnlSlntyHvr_FWAAC",
                                                "AnnlSlntyPrkr_FWAAC",
                                               "AnnlSlntyPwllInflw_FWAAC",
                                                "AnnlSlntyImprl_FWAAC")) %>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 

# # debug
# head(zz_all)
# unique(zz_all$Variable)
# unique(zz_all$Year)

#  Pulling historical SLOAD data
hist <- read_xlsx(file.path(getwd(),'data/HistSLOAD.xlsx'))

# Formatting data frame to match zz_all
hist$Scenario <- 'Historical SLOAD'
hist$Mean <-hist$Med <- hist$Min <- hist$Max <- hist$MinOut <- hist$MaxOut <- hist$Value
hist <- within(hist, rm(Value))
hist <- hist[c("Scenario","Year","Variable","Mean","Med","Min","Max","MinOut","MaxOut")]

# # # # bring in prv TriRvw estimates of mean - Not yet 
# TR17Proj <- read_xlsx(file.path(getwd(),'data/TR17Proj.xlsx'))
# 
# # Formatting data frame to match zz_all
# TR17Proj$Scenario <- 'TR17 Mean Projected FWAAC'
# ######### !! ######### !!
# #Use 2020 no further controls scen2 from 2017 rvw to compare to 2020 rvw scen1
# ######### !! ######### !!
# TR17Proj$Mean <- TR17Proj$Med <- TR17Proj$Min <- TR17Proj$MaxOut <- TR17Proj$MinOut <- TR17Proj$Max <-  TR17Proj$Scen2
# TR17Proj <- within(TR17Proj, rm(Scen1,Scen2,Scen3,Scen4))
# TR17Proj <- TR17Proj[c("Scenario","Year","Variable","Mean","Med","Min","Max","MinOut","MaxOut")]
# # View(TR17Proj)

# Appending historical data
zz_all <- bind_rows(hist,zz_all)
# zz_all <- bind_rows(zz_all,TR17Proj)

# # Getting all scenarios passed to fxn - CF: Alan's addIC use scens instead

# Setting colors for graph- ensures historical data is black on plot
# colorNames <- unique(zz_all$Scenario)
#DCP colors (to match AZ Big Bang slides)"#54FF9F","#F4A460"
#Grey for Interim Guidelines Projections (if included) #8B8682. Add to end.
# plotColors <- c("#000000", "#00BFC4","#F8766D")

#use mycolors defined by Master rather than old Cloud colors 
plotColors <-   c("#000000", mycolors,"#8B8682")  # #black, my colors, grey  
plotColors <-   c("#000000", mycolors)  # #black, my colors, grey  


# 
# if(length(scens) == 4){
#   plotColors <-   c("#000000", "#00BFC4","#329b20","#ede453","#F8766D","#8B8682")  #"#e553fc") #black, green blue yellow red grey  (other = purple
# } else if(length(scens) == 3){
#   plotColors <-   c("#000000", "#00BFC4","#329b20","#ede453","#8B8682")  #old yellow fcf353
# } else if(length(scens) == 2){
#   plotColors <-   c("#000000", "#00BFC4","#329b20","#8B8682")  # green = 54ef39, purple "#d442f4"
# } else if(length(scens) == 1){
#   plotColors <-   c("#000000", "#00BFC4","#8B8682")  
# } else {
#   stop("not set up for that scenario number")
# }

names(plotColors) <- colorNames
# 
# # Adding factors so ggplot does not alphebetize legend
zz_all$Scenario = factor(zz_all$Scenario, levels=colorNames)

# Generating labels for the lines in ggplot
histLab = "Historical SLOAD"
# PrvTRLab = "2017 TriRvw Projection (No Adtl Bynd 2020)"
names(histLab) = "Historical SLOAD"
# names(PrvTRLab) = "2017 TriRvw Projection (No Adtl Bynd 2020)"
histLab = append(histLab, cloudLabs)
# histLab = append(histLab, PrvTRLab)


# # Read in Reclamation logo png - CF: NOT ENABLED
# im <- load.image('logo/660LT-TK-flush.png')
# im_rast <- grid::rasterGrob(im, interpolate = T)

## create a pdf  
pdf(file.path(oFigs,paste0("WQAnnClouds_MinMaxLines=",MinMaxLines,"_",Figs,".pdf")), width= width, height= height)

### Means ###

#-------------------------------------------------------------------------------------
# ++++++++++++++++++++++++++Powell In+++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------------
# zz_all_sv <- zz_all 
# 
# zz_all <- scen_res %>%
#   dplyr::filter(Year %in% yrs, Variable %in% c("AnnlSlntyPwllInflw_FWAAC")) %>%
#   # compute the 10/50/90 and aggregate by start month
#   dplyr::group_by(Scenario, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
#   dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
#                    'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
#                    'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 

NumCrit <- NA
variable = "AnnlSlntyPwllInflw_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Powell Inflow FWAAC"
subtitle = "Average Annual Concentration Comparison"
ylims <- c(NA,NA)
HistMean <- data.frame(yintercept=462) #1999-2020 Avg from 2020 NFSM run 

source("code/Cloud_plot_wHist.R")

# source("code/Cloud_plot_woHist.R")
# zz_all = zz_all_sv  

#-------------------------------------------------------------------------------------
# ++++++++++++++++++++++++++Lees Ferry+++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------------

NumCrit <- NA
variable = "AnnlSlntyLsFrry_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River at Lees Ferry" 
subtitle = "Average Annual Concentration Comparison"
ylims <- c(NA,NA)
# ylims <- c(400,600)
HistMean <- data.frame(yintercept=460) #1999-2021 Avg based on Aug2022 SLOAD

source("code/Cloud_plot_wHist.R")

#-------------------------------------------------------------------------------------
# ++++++++++++++++++++++++++Below Mead+++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------------


NumCrit <- data.frame(yintercept=723)
variable = "AnnlSlntyHvr_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River below Hoover Dam" 
subtitle = "Average Annual Concentration Comparison"
ylims <- c(NA,NA)
#ylims <- c(545,750)
HistMean <- data.frame(yintercept=584) #1999-2021 Avg based on Aug2022 SLOAD

source("code/Cloud_plot_wHist.R")

#-------------------------------------------------------------------------------------
#------------------------------Below Parker-------------------------------------------------------
#-------------------------------------------------------------------------------------


NumCrit <- data.frame(yintercept=747)
variable = "AnnlSlntyPrkr_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River below Parker Dam" 
subtitle = "Average Annual Concentration Comparison"
ylims <- c(NA,NA)
#ylims <- c(550,750)
HistMean <- data.frame(yintercept=600) #1999-2021 Avg based on Aug2022 SLOAD

source("code/Cloud_plot_wHist.R")

#-------------------------------------------------------------------------------------
#-------------------------------At Imperial------------------------------------------------------
#-------------------------------------------------------------------------------------

NumCrit <- data.frame(yintercept=879)
variable = "AnnlSlntyImprl_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River above Imperial Dam" 
subtitle = "Average Annual Concentration Comparison"
ylims <- c(NA,NA)
#ylims <- c(675,900)
HistMean <- data.frame(yintercept=692) #1999-2021 Avg based on Aug2022 SLOAD

source("code/Cloud_plot_wHist.R")

dev.off()


