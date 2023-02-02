##############################################################################
#This script creates clouds 
##############################################################################

library('readxl') #read_xlsx()
library('cowplot') #get_legend()

######READ IN and make scen_res USING MasBalAnn.R#######
source("code/Std_Cloud_Plot_Parameters.R")

### Read Data ###

zz_all <- scen_res %>%
  dplyr::filter(Year %in% yrs, Variable %in% c("UBFlow_Inflow")) %>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 

# # Adding factors so ggplot does not alphebetize legend
zz_all$Scenario = factor(zz_all$Scenario, levels=names(scens))

## create a pdf  
pdf(file.path(oFigs,paste0("FlowMassBal_Clouds_",Figs,".pdf")), width= width, height= height)
# MinMaxLines=",MinMaxLines, ### could add this if want to indicate if lines are added 

NumCrit <- HistMin <- NA
variable = "UBFlow_Inflow"
y_lab = "Inflow (MAF/yr)"
title = "UB Natural Inflow"
subtitle = NA
ylims <- c(NA,NA)

source("code/Cloud_plot_woHist.R")

ggsave(filename = file.path(oFigs,paste0("Cloud_",variable,".png")), width= width, height= height)


dev.off()
