##############################################################################
#This script creates Flow, Mass and Concentration 3 Panel Cloud Figures to compare runs
##############################################################################

library(gridExtra)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# list.files(file.path(scen_dir,scens[2]))
# 
# rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[2],"WQANN.rdf")))


# Parameters for cloud plot customization (line thicknesses, text size, etc.)
# Have been pulled out for convenience
#Text
TitleSize = 6# 8
AxisText = 4#7
LegendLabText = 4#7

AxisLab = 4#7
LabSize = 4#7
LegendText = 4#7

# ylims_pow_inoutflow <- c(7,11)
# ylims_pow_inoutmass <- c(5,7)
# ylims_mead_inoutflow <- c(7,11)
# ylims_mead_inoutmass <- c(7,9)

widths=width
heights=height


rw_agg_file <- "PowellMeadInOut.csv" #doesn't include outflow
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE))
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)

if(names(scens[1])=="2020TriRvw_Scen3" && 2020 %in% unique(scen_res$Year)){
#Shift TriRvw forward 3 years
trirvwrows <- which(scen_res$Scenario == names(scens[1]))
scen_res$Year[trirvwrows] = scen_res$Year[trirvwrows] + 3
unique(scen_res$Year)
}


# unique(scen_res$Variable) #check variable names

# # Adding factors so ggplot does not alphebetize legend
scen_res$Scenario = factor(scen_res$Scenario, levels=names(scens))

#add scenario names to line, point and color scales
names(lt_scale) <- unique(scen_res$Scenario)
names(pt_scale) <- unique(scen_res$Scenario)
names(mycolors) <- unique(scen_res$Scenario)

### Plotting Parameters ###
MinMaxLines<-F # T is want dotted line as min max of any given trace 

source("code/Cloud_Text_Std_Parameters.R")

plotColors <-   c(mycolors)#,"#8B8682")  # #black, my colors, grey  
colorNames <- names(scens)  
# colorNames <- names(scens)  

# Set tick marks for x and y axis
myXLabs <- seq(1990,3000,5)
myYLabs <- seq(2,12,1)

yrs <- startyr:endyr #simplify 

cloudScen <- names(scens)
cloudLabs <- names(scens)

unique(scen_res$Variable)
divide_1M <- unique(scen_res$Variable)[1:12]
zz_1M <- scen_res %>%
  dplyr::filter(Variable %in% divide_1M) %>%
  dplyr::mutate(Value = Value/1000000) 

zz_conc <- scen_res %>%
  dplyr::filter(!(Variable %in% divide_1M)) 

#not sure why I can't combine these but just do individually for now 
zz_1M <- zz_1M %>% #this doesn't work for some reason 
  dplyr::filter(Year %in% yrs) %>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot
# unique(zz_1M$Variable)


zz_conc <- zz_conc %>% #this doesn't work for some reason 
  dplyr::filter(Year %in% yrs) %>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot
# unique(zz_conc$Variable)





# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dev.off()

if(T){
## create a pdf  
pdf(file.path(oFigs,paste0("Cloud_PowMeadInOut_",Figs,".pdf")), width= width, height= height)
  
  # # need df from rw_agg_file <- "UB3GageFlowSaltMass.csv" 
  # # run SingleTrace_Salt_Explore.R to load this data 
  zz_all = UB_res_raw %>% 
    dplyr::filter(Year %in% yrs) %>%
    # compute the 10/50/90 and aggregate by start month
    dplyr::group_by(Scenario, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
    dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                     'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                     'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot
  
  NumCrit = HistMin = HistMax = NA 
  
  variable = "8_conc_cisco"
  y_lab = "Concentration (mg/l)"
  title = "Cisco FWAAC" 
  source("code/Cloud_plot_woHist.R")
  c1 <- gg
  
  variable = "16_conc_grut"
  y_lab = "Concentration (mg/l)"
  title = "GRUT FWAAC" 
  source("code/Cloud_plot_woHist.R")
  c2 <- gg
  
  variable = "19_conc_bluf"
  y_lab = "Concentration (mg/l)"
  title = "Bluff FWAAC" 
  source("code/Cloud_plot_woHist.R")
  c3 <- gg
  
  grid.arrange(c1,c2,c3,ncol=1)
  
#-------------------------------------------------------------------------------------
# Powell Inflow 
#-------------------------------------------------------------------------------------

zz_all = zz_1M #need zz_all for Cloud_plot_woHist.R
variable = "Powell.Inflow"
y_lab = "Inflow (MAF/yr)"
title = "Lake Powell Inflow" 
subtitle = ""
# ylims <- c(3490,3570)
NumCrit <- data.frame(yintercept=9.6) #mean of 2020 NFSM, 1991-2020 CY sum
HistMin <- data.frame(yintercept=3.9)
HistMax <- data.frame(yintercept=17.1)
source("code/Cloud_plot_woHist.R")
f1 <- gg

variable = "Powell.Inflow Salt Mass"
y_lab = "Mass (MTons/yr)"
title = "Lake Powell Inflow Salt Mass" 
NumCrit <- data.frame(yintercept=5.7) #mean of 2020 NFSM, 1991-2020 CY sum
HistMin <- data.frame(yintercept=2.6)
HistMax <- data.frame(yintercept=8.8)
source("code/Cloud_plot_woHist.R")
m1 <- gg

zz_all = zz_conc #need zz_all for Cloud_plot_woHist.R
variable = "AnnlSlnty_In_Powell_FWAAC"
y_lab = "Concentration (mg/l)"
title = "Lake Powell Inflow Salt Concentration" 
NumCrit <- data.frame(yintercept=456) #mean of 2020 NFSM, 1991-2020 FWAAC
HistMin <- data.frame(yintercept=342)
HistMax <- data.frame(yintercept=608)
source("code/Cloud_plot_woHist.R")
c1 <- gg

# #ggsave(filename = file.path(oFigs,paste0("Powell_Inflow_3Panel.png")), width= width, height= height)


#-------------------------------------------------------------------------------------
# Powell Storage 
#-------------------------------------------------------------------------------------

zz_all = zz_1M #need zz_all for Cloud_plot_woHist.R
variable = "Powell.Storage"
y_lab = "Storage (MAF)"
title = "Lake Powell EOCY Storage" 
subtitle = ""
# ylims <- c(3490,3570)
NumCrit <- data.frame(yintercept=13.8) #mean of 2020 NFSM, 1991-2020 CY sum
HistMin <- data.frame(yintercept=8.1)
HistMax <- data.frame(yintercept=20.7)
source("code/Cloud_plot_woHist.R")
f2 <- gg

# df1 <- scen_res %>%
#   dplyr::filter(Variable == variable) 

variable = "Powell.Reservoir Salt Mass"
y_lab = "Mass (MTons)"
title = "Lake Powell Reservoir Salt Mass" 
NumCrit <- data.frame(yintercept=9.3) #mean of 2020 NFSM, 1991-2020 CY sum
HistMin <- data.frame(yintercept=6.5)
HistMax <- data.frame(yintercept=13.2)
source("code/Cloud_plot_woHist.R")
m2 <- gg

# df2 <- scen_res %>%
#   dplyr::filter(!(Variable %in% variable)) 
# 
# df2$Value = df2$Value/df1$Value * 735.474184440583
# variable = "Reservoir Salt Concentration"
# df2$Variable = variable
# y_lab = "Concentration (mg/l)"
# title = "Calculated Reservoir Salt Concentration" 
# 
# zz_all = df2 %>% #this doesn't work for some reason 
#   dplyr::filter(Year %in% yrs) %>%
#   # compute the 10/50/90 and aggregate by start month
#   dplyr::group_by(Scenario, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
#   dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
#                    'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
#                    'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot
# 
# source("code/Cloud_plot_woHist.R")
# p3 <- gg


# #ggsave(filename = file.path(oFigs,paste0("Powell_Storage_3Panel.png")), width= width, height= height)


#-------------------------------------------------------------------------------------
# Powell Outflow 
#-------------------------------------------------------------------------------------

zz_all = zz_1M #need zz_all for Cloud_plot_woHist.R
variable = "Powell.Outflow"
y_lab = "Outflow (MAF/yr)"
title = "Lake Powell Outflow" 
subtitle = ""
# ylims <- c(3490,3570)
NumCrit <- data.frame(yintercept=9.3) #mean of 2020 NFSM, 1991-2020 CY sum
HistMin = NA
HistMax = NA
# HistMin <- data.frame(yintercept=7.9)
# HistMax <- data.frame(yintercept=15.3)
source("code/Cloud_plot_woHist.R")
f3 <- gg

variable = "Powell.Outflow Salt Mass"
y_lab = "Mass (MTons/yr)"
title = "Lake Powell Outflow Salt Mass" 
NumCrit <- data.frame(yintercept=5.9) #mean of 2020 NFSM, 1991-2020 CY sum
HistMin <- data.frame(yintercept=4.7)
HistMax <- data.frame(yintercept=8.8)
source("code/Cloud_plot_woHist.R")
m3 <- gg

zz_all = zz_conc #need zz_all for Cloud_plot_woHist.R
variable = "AnnlSlntyLsFrry_FWAAC"
y_lab = "Concentration (mg/l)"
title = "Lees Ferry Average Salt Concentration" 
NumCrit <- data.frame(yintercept=469) #mean of 2020 NFSM, 1991-2020 FWAAC
HistMin <- data.frame(yintercept=405)
HistMax <- data.frame(yintercept=580)
source("code/Cloud_plot_woHist.R")
c3 <- gg

# #ggsave(filename = file.path(oFigs,paste0("Powell_Outflow_3Panel.png")), width= width, height= height)

#arrange by in, out 
grid.arrange(f1,m1,c1,ncol=1)
grid.arrange(f2,m2,ncol=1)
grid.arrange(f3,m3,c3,ncol=1)

#arrange by type 
grid.arrange(f1,f2,f3,ncol=1)
grid.arrange(m1,m2,m3,ncol=1)
grid.arrange(c1,c3,ncol=1)


################################################################################
#############################  Mead  ##########################################
################################################################################

#-------------------------------------------------------------------------------------
# Mead Inflow 
#-------------------------------------------------------------------------------------
NumCrit = HistMin = HistMax = NA #dont add for Mead

zz_all = zz_1M #need zz_all for Cloud_plot_woHist.R
variable = "Mead.Inflow"
y_lab = "Inflow (MAF/yr)"
title = "Lake Mead Inflow" 
subtitle = ""
# ylims <- c(3490,3570)
#NumCrit <- data.frame(yintercept=9.6) #mean of 2020 NFSM, 1991-2020 CY sum
#HistMin <- data.frame(yintercept=3.9)
#HistMax <- data.frame(yintercept=17.1)
source("code/Cloud_plot_woHist.R")
f1 <- gg

variable = "Mead.Inflow Salt Mass"
y_lab = "Mass (MTons/yr)"
title = "Lake Mead Inflow Salt Mass" 
#NumCrit <- data.frame(yintercept=5.7) #mean of 2020 NFSM, 1991-2020 CY sum
#HistMin <- data.frame(yintercept=2.6)
#HistMax <- data.frame(yintercept=8.8)
source("code/Cloud_plot_woHist.R")
m1 <- gg

zz_all = zz_conc #need zz_all for Cloud_plot_woHist.R
variable = "AnnlSlnty_In_Hvr_FWAAC"
y_lab = "Concentration (mg/l)"
title = "Lake Mead Inflow Salt Concentration" 
#NumCrit <- data.frame(yintercept=456) #mean of 2020 NFSM, 1991-2020 FWAAC
#HistMin <- data.frame(yintercept=342)
#HistMax <- data.frame(yintercept=608)
source("code/Cloud_plot_woHist.R")
c1 <- gg

#ggsave(filename = file.path(oFigs,paste0("Mead_Inflow_3Panel.png")), width= width, height= height)


#-------------------------------------------------------------------------------------
# Mead Storage 
#-------------------------------------------------------------------------------------

zz_all = zz_1M #need zz_all for Cloud_plot_woHist.R
variable = "Mead.Storage"
y_lab = "Storage (MAF)"
title = "Lake Mead EOCY Storage" 
subtitle = ""
# ylims <- c(3490,3570)
#NumCrit <- data.frame(yintercept=13.8) #mean of 2020 NFSM, 1991-2020 CY sum
#HistMin <- data.frame(yintercept=8.1)
#HistMax <- data.frame(yintercept=20.7)
source("code/Cloud_plot_woHist.R")
f2 <- gg

df1 <- scen_res %>%
  dplyr::filter(Variable == variable) 

variable = "Mead.Reservoir Salt Mass"
y_lab = "Mass (MTons)"
title = "Lake Mead Reservoir Salt Mass" 
#NumCrit <- data.frame(yintercept=9.3) #mean of 2020 NFSM, 1991-2020 CY sum
#HistMin <- data.frame(yintercept=6.5)
#HistMax <- data.frame(yintercept=13.2)
source("code/Cloud_plot_woHist.R")
m2 <- gg

# df2 <- scen_res %>%
#   dplyr::filter(!(Variable %in% variable)) 
# 
# df2$Value = df2$Value/df1$Value * 735.474184440583
# variable = "Reservoir Salt Concentration"
# df2$Variable = variable
# y_lab = "Concentration (mg/l)"
# title = "Calculated Reservoir Salt Concentration" 
# 
# zz_all = df2 %>% #this doesn't work for some reason 
#   dplyr::filter(Year %in% yrs) %>%
#   # compute the 10/50/90 and aggregate by start month
#   dplyr::group_by(Scenario, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
#   dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
#                    'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
#                    'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot
# 
# source("code/Cloud_plot_woHist.R")
# p3 <- gg

#ggsave(filename = file.path(oFigs,paste0("Mead_Storage_3Panel.png")), width= width, height= height)


#-------------------------------------------------------------------------------------
# Mead Outflow 
#-------------------------------------------------------------------------------------

zz_all = zz_1M #need zz_all for Cloud_plot_woHist.R
variable = "Mead.Outflow"
y_lab = "Outflow (MAF/yr)"
title = "Lake Mead Outflow" 
subtitle = ""
# ylims <- c(3490,3570)
#NumCrit <- data.frame(yintercept=9.3) #mean of 2020 NFSM, 1991-2020 CY sum
#HistMin = NA
#HistMax = NA
# #HistMin <- data.frame(yintercept=7.9)
# #HistMax <- data.frame(yintercept=15.3)
source("code/Cloud_plot_woHist.R")
f3 <- gg

variable = "Mead.Outflow Salt Mass"
y_lab = "Mass (MTons/yr)"
title = "Lake Mead Outflow Salt Mass" 
#NumCrit <- data.frame(yintercept=5.9) #mean of 2020 NFSM, 1991-2020 CY sum
#HistMin <- data.frame(yintercept=4.7)
#HistMax <- data.frame(yintercept=8.8)
source("code/Cloud_plot_woHist.R")
m3 <- gg

zz_all = zz_conc #need zz_all for Cloud_plot_woHist.R
variable = "AnnlSlntyMead_FWAAC"
y_lab = "Concentration (mg/l)"
title = "Below Hoover Average Salt Concentration" 
#NumCrit <- data.frame(yintercept=469) #mean of 2020 NFSM, 1991-2020 FWAAC
#HistMin <- data.frame(yintercept=405)
#HistMax <- data.frame(yintercept=580)
source("code/Cloud_plot_woHist.R")
c3 <- gg

#ggsave(filename = file.path(oFigs,paste0("Mead_Outflow_3Panel.png")), width= width, height= height)


#arrange by in, out 
grid.arrange(f1,m1,c1,ncol=1)
grid.arrange(f2,m2,ncol=1)
grid.arrange(f3,m3,c3,ncol=1)

#arrange by type 
grid.arrange(f1,f2,f3,ncol=1)
grid.arrange(m1,m2,m3,ncol=1)
grid.arrange(c1,c3,ncol=1)


dev.off()
}
