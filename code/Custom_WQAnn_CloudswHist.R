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

yrs <- startyr:endyr #simplify 

cloudScen <- names(scens)
cloudLabs <- names(scens)

### Read Data ###


zz_all <- scen_res %>%
  dplyr::filter(Year %in% yrs, Variable %in% c("AnnlSlntyLsFrry_FWAAC",
                                                "AnnlSlntyHvr_FWAAC",
                                                "AnnlSlntyPrkr_FWAAC",
                                                "AnnlSlntyImprl_FWAAC")) %>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Year,Variable) %>% #by leaving Variable in I keep the name in the resulting df
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9))

# # debug
# head(zz_all)
# unique(zz_all$Variable)
# unique(zz_all$Year)

#  Pulling historical SLOAD data
hist <- read_xlsx(file.path(getwd(),'data/HistSLOAD.xlsx'))

# Formatting data frame to match zz_all
hist$Scenario <- 'Historical Elevation'
hist$Mean <-hist$Med <- hist$Min <- hist$Max <- hist$Value
hist <- within(hist, rm(Value))
hist <- hist[c("Scenario","Year","Variable","Mean","Med","Min","Max")]

# # # # bring in prv TriRvw estimates of mean - Not yet 
# TR17Proj <- read_xlsx(file.path(getwd(),'data/TR17Proj.xlsx'))
# 
# # Formatting data frame to match zz_all
# TR17Proj$Scenario <- 'TR17 Mean Projected FWAAC'
# ######### !! ######### !!
# #Use 2020 no further controls scen2 from 2017 rvw to compare to 2020 rvw scen1
# ######### !! ######### !!
# TR17Proj$Mean <- TR17Proj$Med <- TR17Proj$Min <- TR17Proj$Max <- TR17Proj$Scen2
# TR17Proj <- within(TR17Proj, rm(Scen1,Scen2,Scen3,Scen4))
# TR17Proj <- TR17Proj[c("Scenario","Year","Variable","Mean","Med","Min","Max")]
# # View(TR17Proj)

# Appending historical data
zz_all <- bind_rows(hist,zz_all)
# zz_all <- bind_rows(zz_all,TR17Proj)

# # Getting all scenarios passed to fxn - CF: Alan's addIC use scens instead

# Setting colors for graph- ensures historical data is black on plot
colorNames <- unique(zz_all$Scenario)
#DCP colors (to match AZ Big Bang slides)"#54FF9F","#F4A460"
#Grey for Interim Guidelines Projections (if included) #8B8682. Add to end.
# plotColors <- c("#000000", "#00BFC4","#F8766D")
if(length(scens) == 4){
  plotColors <-   c("#000000", "#00BFC4","#329b20","#ede453","#F8766D","#8B8682")  #"#e553fc") #black, green blue yellow red grey  (other = purple
} else if(length(scens) == 3){
  plotColors <-   c("#000000", "#00BFC4","#329b20","#ede453","#8B8682")  #old yellow fcf353
} else if(length(scens) == 2){
  plotColors <-   c("#000000", "#00BFC4","#329b20","#8B8682")  # green = 54ef39, purple "#d442f4"
} else if(length(scens) == 1){
  plotColors <-   c("#000000", "#00BFC4","#8B8682")  
} else {
  stop("not set up for that scenario number")
}

names(plotColors) <- colorNames

# Adding factors so ggplot does not alphebetize legend
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
pdf(file.path(oFigs,paste0("WQAnnClouds_",Figs,".pdf")), width= width, height= height)

### Means ###

variable = "AnnlSlntyLsFrry_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River at Lees Ferry" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(350,550)

zz <- zz_all %>%
  dplyr::filter(Variable == variable) 

gg <- ggplot(zz, aes(x=Year, y=Mean, color=Scenario, group=Scenario)) +  theme_light()  #this is just a blank grided plot

# Generate plot a to make ribbon legend
name <- str_wrap("10th, 90th percentile",20)
# gga <- gg + geom_ribbon(data = zz,aes(ymin=Min, ymax=Max, fill = Scenario), 
gga <- gg + geom_ribbon(data = subset(zz,Scenario %in% rev(names(scens))),aes(ymin=Min, ymax=Max, fill = Scenario),  #CF: need to subset so don't create for mean 
                        alpha = 0.3, linetype = 2, size = 0.5*Medians) +
  scale_fill_manual(name, 
                    values = plotColors, guide = guide_legend(order=1),
                    labels = str_wrap(cloudLabs[], 15)) + scale_color_manual(name,
                                                                           values = plotColors, guide = guide_legend(order=1),
                                                                           labels = str_wrap(cloudLabs, 15))  +
  theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
        legend.box.margin = margin(0,0,0,0)) 

  legenda <- get_legend(gga)

# # Generate plot b to take medians legend
# if(med == F){
  lengendtitle <- "Mean FWAAC"
# } else {
#   lengendtitle <- "Historical and Median Projected Pool Elevation"
# }

ggb <- gg + geom_line(size=Medians) + 
  scale_color_manual(name = str_wrap(lengendtitle,20),
                     values = plotColors, labels = str_wrap(histLab, 15)) +
  theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
        legend.box.margin = margin(0,0,0,0)) 
legendb <- get_legend(ggb)

# Make legend grob.  4 rows used to make legend close together and in the middle with respects to the vertical
gglegend <- plot_grid(NULL, legenda,legendb, NULL, align = 'hv', nrow=4) #nrows=4

# Generate plot
ggc <- gg + geom_vline(xintercept=2020, size = IGStartLine, color = '#707070') + 
  
  geom_ribbon(data = subset(zz,Scenario %in% rev(names(scens))),aes(ymin=Min, ymax=Max, fill = Scenario),  #CF: need to subset so don't create for mean 
                    alpha = 0.3, linetype = 2, size = 0.5*Medians) +
  geom_line(size=Medians) +
  scale_fill_manual(str_wrap("10th to 90th percentile of full range",20),
                    values = plotColors, guide = FALSE,
                    labels = str_wrap(cloudLabs, 15)) + 
  scale_color_manual(name = str_wrap(lengendtitle,20),
                     values = plotColors, guide = FALSE,
                     labels = str_wrap(histLab, 15)) +
  labs(y = y_lab, title = title, x = '',subtitle = subtitle) + 
  theme(plot.title = element_text(size = TitleSize),
        ## axis.text.x = element_text(size = AxisLab),
        axis.text.y = element_text (size =AxisLab),
        axis.title = element_text(size=AxisText) ) +
        # panel.grid.minor = element_line(size = GridMin),
        # panel.grid.major = element_line(size = GridMaj)) +
  
  # Adding lines for numeric criteria
  # geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) #+
  
  guides(fill=FALSE) #+
  
#   # Add BOR Logo
#   annotation_custom(im_rast, ymin = yaxmin, ymax = yaxmin + 12, xmin = 1999, xmax = 2006) 

#final plot configuration 
gg <- plot_grid(ggc, gglegend, rel_widths = c(2,.4))
gg

ggsave(filename = file.path(oFigs,paste0(variable,"_Cloud.png")), width= width, height= height)


#-------------------------------------------------------------------------------------


# ++++++++++++++++++++++++++Below Mead+++++++++++++++++++++++++++++++++++++

NumCrit <- data.frame(yintercept=723)
variable = "AnnlSlntyHvr_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River below Hoover Dam" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(545,750)


zz <- zz_all %>%
  dplyr::filter(Variable == variable) 

gg <- ggplot(zz, aes(x=Year, y=Mean, color=Scenario, group=Scenario)) +  theme_light()  #this is just a blank grided plot

# Generate plot a to make ribbon legend
name <- str_wrap("10th, 90th percentile",20)
# gga <- gg + geom_ribbon(data = zz,aes(ymin=Min, ymax=Max, fill = Scenario), 
gga <- gg + geom_ribbon(data = subset(zz,Scenario %in% rev(names(scens))),aes(ymin=Min, ymax=Max, fill = Scenario),  #CF: need to subset so don't create for mean 
                        alpha = 0.3, linetype = 2, size = 0.5*Medians) +
  scale_fill_manual(name, 
                    values = plotColors, guide = guide_legend(order=1),
                    labels = str_wrap(cloudLabs[], 15)) + scale_color_manual(name,
                                                                             values = plotColors, guide = guide_legend(order=1),
                                                                             labels = str_wrap(cloudLabs, 15))  +
  theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
        legend.box.margin = margin(0,0,0,0)) 

legenda <- get_legend(gga)

# # Generate plot b to take medians legend
# if(med == F){
lengendtitle <- "Mean FWAAC"
# } else {
#   lengendtitle <- "Historical and Median Projected Pool Elevation"
# }

ggb <- gg + geom_line(size=Medians) + 
  scale_color_manual(name = str_wrap(lengendtitle,20),
                     values = plotColors, labels = str_wrap(histLab, 15)) +
  theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
        legend.box.margin = margin(0,0,0,0)) 
legendb <- get_legend(ggb)

# Make legend grob.  4 rows used to make legend close together and in the middle with respects to the vertical
gglegend <- plot_grid(NULL, legenda,legendb, NULL, align = 'hv', nrow=4) #nrows=4

# Generate plot
ggc <- gg + geom_vline(xintercept=2020, size = IGStartLine, color = '#707070') + 
  
  geom_ribbon(data = subset(zz,Scenario %in% rev(names(scens))),aes(ymin=Min, ymax=Max, fill = Scenario),  #CF: need to subset so don't create for mean 
              alpha = 0.3, linetype = 2, size = 0.5*Medians) +
  geom_line(size=Medians) +
  scale_fill_manual(str_wrap("10th to 90th percentile of full range",20),
                    values = plotColors, guide = FALSE,
                    labels = str_wrap(cloudLabs, 15)) + 
  scale_color_manual(name = str_wrap(lengendtitle,20),
                     values = plotColors, guide = FALSE,
                     labels = str_wrap(histLab, 15)) +
  labs(y = y_lab, title = title, x = '',subtitle = subtitle) + 
  theme(plot.title = element_text(size = TitleSize),
        ## axis.text.x = element_text(size = AxisLab),
        axis.text.y = element_text (size =AxisLab),
        axis.title = element_text(size=AxisText) ) +
  # panel.grid.minor = element_line(size = GridMin),
  # panel.grid.major = element_line(size = GridMaj)) +
  
  # Adding lines for numeric criteria
  geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) +

  guides(fill=FALSE) #+

#   # Add BOR Logo
#   annotation_custom(im_rast, ymin = yaxmin, ymax = yaxmin + 12, xmin = 1999, xmax = 2006) 

#final plot configuration 
gg <- plot_grid(ggc, gglegend, rel_widths = c(2,.4))
gg

ggsave(filename = file.path(oFigs,paste0(variable,"_Cloud.png")), width= width, height= height)

#-------------------------------------------------------------------------------------


#------------------------------Below Parker-------------------------------------------------------

NumCrit <- data.frame(yintercept=747)
variable = "AnnlSlntyPrkr_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River below Parker Dam" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(550,750)

zz <- zz_all %>%
  dplyr::filter(Variable == variable) 

gg <- ggplot(zz, aes(x=Year, y=Mean, color=Scenario, group=Scenario)) +  theme_light()  #this is just a blank grided plot

# Generate plot a to make ribbon legend
name <- str_wrap("10th, 90th percentile",20)
# gga <- gg + geom_ribbon(data = zz,aes(ymin=Min, ymax=Max, fill = Scenario), 
gga <- gg + geom_ribbon(data = subset(zz,Scenario %in% rev(names(scens))),aes(ymin=Min, ymax=Max, fill = Scenario),  #CF: need to subset so don't create for mean 
                        alpha = 0.3, linetype = 2, size = 0.5*Medians) +
  scale_fill_manual(name, 
                    values = plotColors, guide = guide_legend(order=1),
                    labels = str_wrap(cloudLabs[], 15)) + scale_color_manual(name,
                                                                             values = plotColors, guide = guide_legend(order=1),
                                                                             labels = str_wrap(cloudLabs, 15))  +
  theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
        legend.box.margin = margin(0,0,0,0)) 

legenda <- get_legend(gga)

# # Generate plot b to take medians legend
# if(med == F){
lengendtitle <- "Mean FWAAC"
# } else {
#   lengendtitle <- "Historical and Median Projected Pool Elevation"
# }

ggb <- gg + geom_line(size=Medians) + 
  scale_color_manual(name = str_wrap(lengendtitle,20),
                     values = plotColors, labels = str_wrap(histLab, 15)) +
  theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
        legend.box.margin = margin(0,0,0,0)) 
legendb <- get_legend(ggb)

# Make legend grob.  4 rows used to make legend close together and in the middle with respects to the vertical
gglegend <- plot_grid(NULL, legenda,legendb, NULL, align = 'hv', nrow=4) #nrows=4

# Generate plot
ggc <- gg + geom_vline(xintercept=2020, size = IGStartLine, color = '#707070') + 
  
  geom_ribbon(data = subset(zz,Scenario %in% rev(names(scens))),aes(ymin=Min, ymax=Max, fill = Scenario),  #CF: need to subset so don't create for mean 
              alpha = 0.3, linetype = 2, size = 0.5*Medians) +
  geom_line(size=Medians) +
  scale_fill_manual(str_wrap("10th to 90th percentile of full range",20),
                    values = plotColors, guide = FALSE,
                    labels = str_wrap(cloudLabs, 15)) + 
  scale_color_manual(name = str_wrap(lengendtitle,20),
                     values = plotColors, guide = FALSE,
                     labels = str_wrap(histLab, 15)) +
  labs(y = y_lab, title = title, x = '',subtitle = subtitle) + 
  theme(plot.title = element_text(size = TitleSize),
        ## axis.text.x = element_text(size = AxisLab),
        axis.text.y = element_text (size =AxisLab),
        axis.title = element_text(size=AxisText) ) +
  # panel.grid.minor = element_line(size = GridMin),
  # panel.grid.major = element_line(size = GridMaj)) +
  
  # Adding lines for numeric criteria
  geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) +
  
  guides(fill=FALSE) #+

#   # Add BOR Logo
#   annotation_custom(im_rast, ymin = yaxmin, ymax = yaxmin + 12, xmin = 1999, xmax = 2006) 

#final plot configuration 
gg <- plot_grid(ggc, gglegend, rel_widths = c(2,.4))
gg

ggsave(filename = file.path(oFigs,paste0(variable,"_Cloud.png")), width= width, height= height)

#-------------------------------At Imperial------------------------------------------------------

NumCrit <- data.frame(yintercept=879)
variable = "AnnlSlntyImprl_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Colorado River above Imperial Dam" 
subtitle = "Average Annual Concentration Comparision"
ylims <- c(675,900)

zz <- zz_all %>%
  dplyr::filter(Variable == variable) 

gg <- ggplot(zz, aes(x=Year, y=Mean, color=Scenario, group=Scenario)) +  theme_light()  #this is just a blank grided plot

# Generate plot a to make ribbon legend
name <- str_wrap("10th, 90th percentile",20)
# gga <- gg + geom_ribbon(data = zz,aes(ymin=Min, ymax=Max, fill = Scenario), 
gga <- gg + geom_ribbon(data = subset(zz,Scenario %in% rev(names(scens))),aes(ymin=Min, ymax=Max, fill = Scenario),  #CF: need to subset so don't create for mean 
                        alpha = 0.3, linetype = 2, size = 0.5*Medians) +
  scale_fill_manual(name, 
                    values = plotColors, guide = guide_legend(order=1),
                    labels = str_wrap(cloudLabs[], 15)) + scale_color_manual(name,
                                                                             values = plotColors, guide = guide_legend(order=1),
                                                                             labels = str_wrap(cloudLabs, 15))  +
  theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
        legend.box.margin = margin(0,0,0,0)) 

legenda <- get_legend(gga)

# # Generate plot b to take medians legend
# if(med == F){
lengendtitle <- "Mean FWAAC"
# } else {
#   lengendtitle <- "Historical and Median Projected Pool Elevation"
# }

ggb <- gg + geom_line(size=Medians) + 
  scale_color_manual(name = str_wrap(lengendtitle,20),
                     values = plotColors, labels = str_wrap(histLab, 15)) +
  theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
        legend.box.margin = margin(0,0,0,0)) 
legendb <- get_legend(ggb)

# Make legend grob.  4 rows used to make legend close together and in the middle with respects to the vertical
gglegend <- plot_grid(NULL, legenda,legendb, NULL, align = 'hv', nrow=4) #nrows=4

# Generate plot
ggc <- gg + geom_vline(xintercept=2020, size = IGStartLine, color = '#707070') + 
  
  geom_ribbon(data = subset(zz,Scenario %in% rev(names(scens))),aes(ymin=Min, ymax=Max, fill = Scenario),  #CF: need to subset so don't create for mean 
              alpha = 0.3, linetype = 2, size = 0.5*Medians) +
  geom_line(size=Medians) +
  scale_fill_manual(str_wrap("10th to 90th percentile of full range",20),
                    values = plotColors, guide = FALSE,
                    labels = str_wrap(cloudLabs, 15)) + 
  scale_color_manual(name = str_wrap(lengendtitle,20),
                     values = plotColors, guide = FALSE,
                     labels = str_wrap(histLab, 15)) +
  labs(y = y_lab, title = title, x = '',subtitle = subtitle) + 
  theme(plot.title = element_text(size = TitleSize),
        ## axis.text.x = element_text(size = AxisLab),
        axis.text.y = element_text (size =AxisLab),
        axis.title = element_text(size=AxisText) ) +
  # panel.grid.minor = element_line(size = GridMin),
  # panel.grid.major = element_line(size = GridMaj)) +
  
  # Adding lines for numeric criteria
  geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) +
  
  guides(fill=FALSE) #+

#   # Add BOR Logo
#   annotation_custom(im_rast, ymin = yaxmin, ymax = yaxmin + 12, xmin = 1999, xmax = 2006) 

#final plot configuration 
gg <- plot_grid(ggc, gglegend, rel_widths = c(2,.4))
gg

ggsave(filename = file.path(oFigs,paste0(variable,"_Cloud.png")), width= width, height= height)


dev.off()


