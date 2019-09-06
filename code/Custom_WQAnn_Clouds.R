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

library('readxl')

rw_agg_file <- "WQAnn.csv" #doesn't include outflow

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

# unique(scen_res$Variable) #check variable names 

## New ##
## Make a dataframe for Outflow and Outflow Salt Mass at each of the Numeric Criteria Points and Lees
#agg file specifying which slots

# #####################################################################################################################################################################
# ## For old TriRvw 2017 Runs to plot you Outflow and Outflow Salt Mass you have to grab these slots from Salt.rdf since the output.control changed throughtout time#
# ##################################################################################################################################################################

#add scenario names to line, point and color scales
names(lt_scale) <- unique(scen_res$Scenario)
names(pt_scale) <- unique(scen_res$Scenario)
names(mycolors) <- unique(scen_res$Scenario)


# The blue gradient background is "graph trash" 
# # make custom axis shading, don't use for now doesn't look good with plotte pallette
# g <- rasterGrob(scales::alpha(blues9, 0.1), width=unit(1,"npc"), height = unit(1,"npc"), 
#                 interpolate = TRUE) #alpha is the transperency  

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## create a pdf  
pdf(file.path(oFigs,paste0("WQAnnClouds_",Figs,".pdf")), width= width, height= height)

# ++++++++++++++++++++++++++Below Powell+++++++++++++++++++++++++++++++++++++
#SEE main_TriRvw_hack.R and custom_cloud_function.R in Process CRSS Res if want to get fancier 


# library('cowplot') #get_legend()

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
# LegendWidth = 1
# LegendHeight = 2.5

### Plotting Parameters ###


# names(scen_res)[4] <- "StartMonth" #I'm not using Scenario instead of matching alan's code which uses StartMonth

cloudScen <- names(scens)
cloudLabs <- names(scens)

# Setting colors for graph
colorNames <- unique(zz$Scenario)
# colors from TriRvw_Master
plotColors <- mycolors

### Read Data ###


zz <- scen_res %>%
  dplyr::filter(Year %in% yrs, Variable %in% c("AnnlSlntyLsFrry_FWAAC",
                                                "AnnlSlntyHvr_FWAAC",
                                                "AnnlSlntyPrkr_FWAAC",
                                                "AnnlSlntyImprl_FWAAC")) %>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9))

head(zz)
unique(zz$Variable)
unique(zz$Year)

#  Pulling historical SLOAD data
hist <- read_xlsx(file.path(getwd(),'data/HistSLOAD.xlsx'))

# Formatting data frame to match zz
hist$Scenario <- 'Historical Elevation'
hist$Mean <-hist$Med <- hist$Min <- hist$Max <- hist$Value
hist <- within(hist, rm(Value))
hist <- hist[c("Scenario","Year","Variable","Mean","Med","Min","Max")]

# # # could use this same procedur to bring in prv TriRvw estimates of mean 
# #  BUILD DATA SHEET IF WANT TO BRING in 2017 TriRvw Proj
# TR17Proj <- read_xlsx(file.path(getwd(),'data/TR17Proj.xlsx'))
# 
# # Formatting data frame to match zz
# TR17Proj$Scenario <- 'TR17Projorical Elevation'
# TR17Proj$Mean <- TR17Proj$Med <- TR17Proj$Min <- TR17Proj$Max <- TR17Proj$Value
# TR17Proj <- within(TR17Proj, rm(Value))
# TR17Proj <- TR17Proj[c("Scenario","Year","Variable","Mean","Med","Min","Max")]


# Appending historical data
zz2 <- bind_rows(hist,zz)
# zz <- bind_rows(zz,TR17Proj)


head(hist)




# Adding factors so ggplot does not alphebetize legend
scen_res$Scenario = factor(scen_res$Scenario, levels=colorNames)


### Means ###

yrs <- startyr:endyr #simplify 


variable = "AnnlSlntyLsFrry_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River at Lees Ferry" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(350,550)




zz <- scen_res %>%
  dplyr::filter(Year %in% yrs, Variable == variable) %>%
  

gg <- ggplot(zz, aes(x=Year, y=Med, color=Scenario, group=Scenario)) +  theme_light() #looks nice

# Generate plot of 10-90 clouds 
name <- str_wrap("10th, Mean, 90th percentile",20)
gga <- gg + geom_ribbon(data = zz,aes(ymin=Min, ymax=Max, fill = Scenario), 
                        alpha = 0.3, linetype = 2, size = 0.5*Medians) +
  scale_fill_manual(name, 
                    values = plotColors, guide = guide_legend(order=1),
                    labels = str_wrap(cloudLabs, 15)) + scale_color_manual(name,
                                                                           values = plotColors, guide = guide_legend(order=1),
                                                                           labels = str_wrap(cloudLabs, 15))  +
  theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
        legend.box.margin = margin(0,0,0,0)) +

  geom_line(data = zz,aes(y=Mean, x=Year, color = Scenario)) +
  # geom_line(data = zz,aes(y=Med, x=Year, color = Scenario, linetype = "dashed")) +
  # geom_line(data = zz,aes(y=Med, x=Year, color = Scenario, size=Medians)) + #add line size Medians 

  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle) #+ #remove model step name from title
  # theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(gga)

ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)


#-------------------------------------------------------------------------------------


# ++++++++++++++++++++++++++Below Mead+++++++++++++++++++++++++++++++++++++

NumCrit <- data.frame(yintercept=723)
variable = "AnnlSlntyHvr_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River below Hoover Dam" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(545,750)


zz <- scen_res %>%
  dplyr::filter(Year %in% yrs, Variable == variable) %>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Year,Variable) %>%
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9))

gg <- ggplot(zz, aes(x=Year, y=Med, color=Scenario, group=Scenario)) +  theme_light() #looks nice

# Generate plot of 10-90 clouds 
name <- str_wrap("10th, Mean, 90th percentile",20)
gga <- gg + geom_ribbon(data = zz,aes(ymin=Min, ymax=Max, fill = Scenario), 
                        alpha = 0.3, linetype = 2, size = 0.5*Medians) +
  scale_fill_manual(name, 
                    values = plotColors, guide = guide_legend(order=1),
                    labels = str_wrap(cloudLabs, 15)) + scale_color_manual(name,
                                                                           values = plotColors, guide = guide_legend(order=1),
                                                                           labels = str_wrap(cloudLabs, 15))  +
  theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
        legend.box.margin = margin(0,0,0,0)) +
  
  geom_line(data = zz,aes(y=Med, x=Year, color = Scenario)) + 
  
  geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) +
  
  # geom_line(data = zz,aes(y=Med, x=Year, color = Scenario, size=Medians))
  
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle) #+ #remove model step name from title
# theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(gga)


ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

#-------------------------------------------------------------------------------------


#------------------------------Below Parker-------------------------------------------------------

NumCrit <- data.frame(yintercept=747)
variable = "AnnlSlntyPrkr_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River below Parker Dam" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(550,750)

zz <- scen_res %>%
  dplyr::filter(Year %in% yrs, Variable == variable) %>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Year,Variable) %>%
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9))

gg <- ggplot(zz, aes(x=Year, y=Med, color=Scenario, group=Scenario)) +  theme_light() #looks nice

# Generate plot of 10-90 clouds 
name <- str_wrap("10th, Mean, 90th percentile",20)
gga <- gg + geom_ribbon(data = zz,aes(ymin=Min, ymax=Max, fill = Scenario), 
                        alpha = 0.3, linetype = 2, size = 0.5*Medians) +
  scale_fill_manual(name, 
                    values = plotColors, guide = guide_legend(order=1),
                    labels = str_wrap(cloudLabs, 15)) + scale_color_manual(name,
                                                                           values = plotColors, guide = guide_legend(order=1),
                                                                           labels = str_wrap(cloudLabs, 15))  +
  theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
        legend.box.margin = margin(0,0,0,0)) +
  
  geom_line(data = zz,aes(y=Med, x=Year, color = Scenario)) + 
  
  geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) +
  
  # geom_line(data = zz,aes(y=Med, x=Year, color = Scenario, size=Medians))
  
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle) #+ #remove model step name from title
# theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(gga)


ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)

#-------------------------------At Imperial------------------------------------------------------

NumCrit <- data.frame(yintercept=879)
variable = "AnnlSlntyImprl_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River above Imperial Dam" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(675,900)

zz <- scen_res %>%
  dplyr::filter(Year %in% yrs, Variable == variable) %>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Year,Variable) %>%
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9))

gg <- ggplot(zz, aes(x=Year, y=Med, color=Scenario, group=Scenario)) +  theme_light() #looks nice

# Generate plot of 10-90 clouds 
name <- str_wrap("10th, Mean, 90th percentile",20)
gga <- gg + geom_ribbon(data = zz,aes(ymin=Min, ymax=Max, fill = Scenario), 
                        alpha = 0.3, linetype = 2, size = 0.5*Medians) +
  scale_fill_manual(name, 
                    values = plotColors, guide = guide_legend(order=1),
                    labels = str_wrap(cloudLabs, 15)) + scale_color_manual(name,
                                                                           values = plotColors, guide = guide_legend(order=1),
                                                                           labels = str_wrap(cloudLabs, 15))  +
  theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
        legend.box.margin = margin(0,0,0,0)) +
  
  geom_line(data = zz,aes(y=Med, x=Year, color = Scenario)) + 
  
  geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) +
  
  # geom_line(data = zz,aes(y=Med, x=Year, color = Scenario, size=Medians))
  
  labs(title = title, y = y_lab, x = "Year",subtitle = subtitle) #+ #remove model step name from title
# theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
print(gga)


ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)


dev.off()


