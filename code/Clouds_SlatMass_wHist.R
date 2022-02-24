##############################################################################
#This script creates mass clouds 
##############################################################################


rw_agg_file <- "saltmass.csv" #doesn't include outflow
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE))

scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)

#add scenario names to line, point and color scales
names(lt_scale) <- unique(scen_res$Scenario)
names(pt_scale) <- unique(scen_res$Scenario)
names(mycolors) <- unique(scen_res$Scenario)

### Plotting Parameters ###
MinMaxLines<-F # T is want dotted line as min max of any given trace 

source("code/Cloud_Text_Std_Parameters.R")

colorNames <- c("SLOAD Mass",names(scens))  
# colorNames <- names(scens)  

# Set tick marks for x and y axis
myXLabs <- seq(1990,3000,5)
myYLabs <- seq(2,12,1)

yrs <- startyr:endyr #simplify 

cloudScen <- names(scens)
cloudLabs <- names(scens)

### Read Data ###

unique(scen_res$Variable)

zz_all <- scen_res %>%
  dplyr::filter(Year %in% yrs) %>%
  dplyr::mutate(Value = Value/1000000) %>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 

vars <- unique(zz_all$Variable)
# head(zz_all)

# #  Pulling historical SLOAD data
df_obs <- readxl::read_xlsx('data/HistFlowMassConcAnn.xlsx',col_names = T, ) 
df_obs = df_obs %>% 
  pivot_longer(cols=names(df_obs)[3:62],names_to = 'Variable',values_to = 'Value') %>% 
  dplyr::mutate(Value = Value/1000000) %>%
  filter(Variable %in% vars) 

# Formatting data frame to match zz_all
df_obs$Scenario <- 'SLOAD Mass'
df_obs$Mean <-df_obs$Med <- df_obs$Min <- df_obs$Max <- df_obs$MinOut <- df_obs$MaxOut <- df_obs$Value
df_obs <- within(df_obs, rm(Value))
# df_obs <- df_obs[c("Scenario","Year","Variable","Mean","Med","Min","Max","MinOut","MaxOut")]
# Appending df_obsorical data
zz_all <- bind_rows(df_obs,zz_all)

# Generating labels for the lines in ggplot
histLab = 'SLOAD Mass'
names(histLab) = histLab
histLab = append(histLab, cloudLabs)

#use mycolors defined by Master rather than old Cloud colors 
plotColors <-   c("#000000", mycolors)#,"#8B8682")  # #black, my colors, grey  
# plotColors <-   mycolors#,"#8B8682")  # #black, my colors, grey  


names(plotColors) <- colorNames
# 
# # Adding factors so ggplot does not alphebetize legend
zz_all$Scenario = factor(zz_all$Scenario, levels=colorNames)


# # Read in Reclamation logo png - CF: NOT ENABLED
# im <- load.image('logo/660LT-TK-flush.png')
# im_rast <- grid::rasterGrob(im, interpolate = T)

## create a pdf  
# pdf(file.path(oFigs,paste0("SaltMass_Clouds_MinMaxLines=",MinMaxLines,"_",Figs,".pdf")), width= width, height= height)
pdf(file.path(oFigs,paste0("SaltMass_Clouds_",Figs,".pdf")), width= width, height= height)

### Means ###

#-------------------------------------------------------------------------------------
# ++++++++++++++++++++++++++Lees Ferry+++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------------

variable = "20_mass_lees"
y_lab = "CY Outflow Salt Mass (1,000,000 tons)"
title = "Powell Outflow Salt Mass"
subtitle = ""
# ylims <- c(3490,3570)
NumCrit <- NA

# source("code/Cloud_plot_woHist.R")
source("code/Cloud_plot_wHist.R")


#-------------------------------------------------------------------------------------
# ++++++++++++++++++++++++++Below Mead+++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------------

variable = "25_mass_hoover"
y_lab = "CY Outflow Salt Mass (1,000,000 tons)"
title = "Mead Outflow Salt Mass"
subtitle = ""
# ylims <- c(3490,3570)
NumCrit <- NA

# source("code/Cloud_plot_woHist.R")
source("code/Cloud_plot_wHist.R")


variable = "28_mass_parker"
y_lab = "CY Outflow Salt Mass (1,000,000 tons)"
title = "Parker Outflow Salt Mass"
subtitle = ""
# ylims <- c(3490,3570)
NumCrit <- NA

source("code/Cloud_plot_wHist.R")

variable = "29_mass_imper"
y_lab = "CY Outflow Salt Mass (1,000,000 tons)"
title = "Above Imperial Salt Mass"
subtitle = ""
# ylims <- c(3490,3570)
NumCrit <- NA

source("code/Cloud_plot_wHist.R")


dev.off()


