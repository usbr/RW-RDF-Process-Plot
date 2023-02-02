##############################################################################
#This script creates monthly boxplots of Outflow and PE to compare two MTOM runs
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


rw_agg_file <-   "NFSinputAnn_oldControlfile_woImpLoad.csv" #"NFSinputAnn.csv" 
#### NEEED NEW SaltOutput.control then change this the RW Agg file to use imperialnf.inflow salt mass NOT LOCAL  


#read agg file specifying which slots
# # NEW files are annual slots so use AsIs
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE))
# # # Old files from 2017 review are monthly so use EOCY 

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)

## Divide Values by 1,000,000 to present data in Million of Tons/Year or MAF
scen_res$Value=(scen_res$Value)/1000000

#add scenario names to line, point and color scales
names(lt_scale) <- unique(scen_res$Scenario)
names(pt_scale) <- unique(scen_res$Scenario)
names(mycolors) <- unique(scen_res$Scenario)

# # Adding factors so ggplot does not alphebetize legend
scen_res$Scenario = factor(scen_res$Scenario, levels=names(scens))


source("code/Std_Cloud_Plot_Parameters.R")

######READ IN and make scen_res USING MasBalAnn.R#######



zz_all <- scen_res %>%
  dplyr::filter(Year %in% yrs) %>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 

zz_all$Scenario = factor(zz_all$Scenario, levels=names(scens))


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if (T) {
  

## create a pdf  
pdf(file.path(oFigs,paste0("NFSinput_Ann_",Figs,".pdf")), width= width, height= height)

NumCrit <- HistMin <- NA
subtitle = NA
ylims <- c(NA,NA)

variable = "WatsonNF.Flow"  
y_lab = "Inflow (MAF/yr)"
title = variable
source("code/Cloud_plot_woHist.R")

variable = "WatsonNF.Mass" 
y_lab = "Inflow Mass (Mtons/yr)"
title = variable
source("code/Cloud_plot_woHist.R")

variable = "RandlettNF.Mass" 
y_lab = "Inflow Mass (Mtons/yr)"
title = variable
source("code/Cloud_plot_woHist.R")

variable = "GreenRiverUTSanRafaelNF.Mass"  
y_lab = "Inflow Mass (Mtons/yr)"
title = variable
source("code/Cloud_plot_woHist.R")

variable = "LeesFerryNF.Mass" 
y_lab = "Inflow Mass (Mtons/yr)"
title = variable
source("code/Cloud_plot_woHist.R")
 
variable = "GrandCanyonNF.Mass" 
y_lab = "Inflow Mass (Mtons/yr)"
title = variable
source("code/Cloud_plot_woHist.R")

variable = "HooverNF.Flow" 
y_lab = "Inflow (MAF/yr)"
title = variable
source("code/Cloud_plot_woHist.R")

variable = "HooverNF.Mass" 
y_lab = "Inflow Mass (Mtons/yr)"
title = variable
source("code/Cloud_plot_woHist.R")

variable = "DavisNF.Flow" 
y_lab = "Inflow (MAF/yr)"
title = variable
source("code/Cloud_plot_woHist.R")

variable = "DavisNF.Mass" 
y_lab = "Inflow Mass (Mtons/yr)"
title = variable
source("code/Cloud_plot_woHist.R")

variable = "ParkerNF.Flow" 
y_lab = "Inflow (MAF/yr)"
title = variable
source("code/Cloud_plot_woHist.R")

variable = "ParkerNF.Mass" 
y_lab = "Inflow Mass (Mtons/yr)"
title = variable
source("code/Cloud_plot_woHist.R")

variable = "ImperialNF.Flow" 
y_lab = "Inflow (MAF/yr)"
title = variable
source("code/Cloud_plot_woHist.R")

# variable = "ImperialNF.Load" #### NEEED NEW SaltOutput.control to get this 
# y_lab = "Inflow Mass (Mtons/yr)"
# title = variable
# source("code/Cloud_plot_woHist.R")








dev.off()


}
