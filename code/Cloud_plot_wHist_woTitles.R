#vars controling how many spaces before the string wrap occurs - not the overall width of the label those are in higher level script
legend_title_width <- 25 #20 # = old value didn't fit #width positive integer giving target line width in characters
legend_labels_width <- 25 #15 # = old value didn't fit

zz <- zz_all %>%
  dplyr::filter(Variable == variable) 

gg <- ggplot(zz, aes(x=Year, y=Mean, color=Scenario, group=Scenario)) +  theme_light()  #this is just a blank grided plot

# #DEBUG ##### PLOTING ISSUE 
# head(zz)
# unique(zz$Scenario)
# gg + geom_line(size=Medians)
# gg + geom_ribbon(data = subset(zz,Scenario %in% cloudScen),aes(ymin=Min, ymax=Max, fill = Scenario),  #CF: need to subset so don't create for mean
#                         alpha = 0.3, linetype = 2, size = 0.5*Medians)

# Generate plot a to make ribbon legend
name <- str_wrap("10th, 90th percentile",legend_title_width)
gga <- gg + geom_ribbon(data = subset(zz,Scenario %in% cloudScen),aes(ymin=Min, ymax=Max, fill = Scenario),  #CF: need to subset so don't create for mean 
                        alpha = 0.3, linetype = 2, size = 0.5*Medians) + 
  scale_fill_manual(name, 
                    values = mycolors, #2022 change: was plotcolors
                    guide = guide_legend(order=1),
                    labels = str_wrap(cloudLabs[], 15)) + scale_color_manual(name,
                                                                             values = mycolors, #2022 change: was plotcolors 
                                                                             guide = guide_legend(order=1),
                                                                             labels = str_wrap(cloudLabs, 15))  +
  theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
        legend.box.margin = margin(0,0,0,0)) 
# gga
legenda <- get_legend(gga)

# # Generate plot b to take medians legend
if (MinMaxLines == T ){ # T is want dotted line as min max of any given trace 
  lengendtitle <- "Min, Mean, Max" #
} else {
  # lengendtitle <- "Mean"
  lengendtitle <- "Scenario"
  
}

ggb <- gg + geom_line(size=Medians) + 
  scale_color_manual(name = str_wrap(lengendtitle,legend_title_width),
                     values = plotColors, labels = str_wrap(histLab, legend_labels_width)) +
  theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
        legend.box.margin = margin(0,0,0,0)) 
# ggb
legendb <- get_legend(ggb)

# Make legend grob.  4 rows used to make legend close together and in the middle with respects to the vertical
# gglegend <- plot_grid(NULL, legenda,legendb, NULL, align = 'hv', nrow=4) #nrows=4
gglegend <- plot_grid(legendb, align = 'hv', nrow=1) #only 1 legend


# Generate plot
ggc <- gg + 
  
  #geom_vline(xintercept=2020, size = IGStartLine, color = '#707070') + 
  
  geom_ribbon(data = subset(zz,Scenario %in% cloudScen),aes(ymin=Min, ymax=Max, fill = Scenario),  #CF: need to subset so don't create for mean 
              alpha = 0.3, linetype = 2, size = 0.5*Medians) +
  geom_line(size=Medians) +
  scale_fill_manual(str_wrap("10th to 90th percentile of full range",legend_title_width),
                    values = plotColors, guide = "none", #It is deprecated to specify `guide = FALSE` 
                    labels = str_wrap(cloudLabs, legend_labels_width)) + 
  scale_color_manual(name = str_wrap(lengendtitle,legend_title_width),
                     values = plotColors, guide = "none", #It is deprecated to specify `guide = FALSE` 
                     labels = str_wrap(histLab, legend_labels_width)) +
  labs(y = y_lab) + #no title or sub
  # labs(y = y_lab, title = title, x = '',subtitle = subtitle) + 
  
  
  scale_x_continuous(minor_breaks = 1990:3000, breaks = myXLabs,
                     labels = myXLabs, expand = c(0,0)) +
  scale_y_continuous(minor_breaks = seq(300,9000,25),
                     breaks = myYLabs, labels = comma) +
  
  theme(plot.title = element_text(size = TitleSize),
        ## axis.text.x = element_text(size = AxisLab),
        axis.text.y = element_text (size =AxisLab),
        axis.title = element_text(size=AxisText),
        panel.grid.minor = element_line(size = GridMin),
        panel.grid.major = element_line(size = GridMaj)) +
  
  if (!is.na(NumCrit)){
    #Adding lines for numeric criteria
    geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) #+
  }
  
  
  guides(fill="none") #+

if (MinMaxLines == T){
  ggc <- ggc + 
    geom_line(data = zz, aes(x=Year, y=MinOut, color=Scenario, group=Scenario),linetype = "dotted") +  
    geom_line(data = zz, aes(x=Year, y=MaxOut, color=Scenario, group=Scenario),linetype = "dotted")   
}

ggc

#final plot configuration 
# gg <- plot_grid(ggc, gglegend, rel_widths = c(2,.4)) #old relative size of the plot vs legend
gg <- plot_grid(ggc, gglegend, rel_widths = c(1.9,.5)) #this is how to adjust the relative size of the plot vs legend

print(gg)

  # # Read in Reclamation logo png
  # im <- load.image('code/BofR-horiz-cmyk.png')
  # im_rast <- grid::rasterGrob(im, interpolate = T)
  
#   # Add BOR Logo
#   annotation_custom(im_rast, ymin = yaxmin, ymax = yaxmin + 12, xmin = 1999, xmax = 2006) 
# 
# #final plot configuration 
# gg <- plot_grid(ggc, gglegend, rel_widths = c(2,.4))

# # Add BOR Logo
# # annotation_custom(im_rast, ymin = yaxmin, ymax = yaxmin + 12, xmin = 1999, xmax = 2006) #Jessie's way must spec position on ea
# source("code/add_logo.R") #alan's way, bottom right corner
# add_logo_horiz(gg)

ggsave(filename = file.path(oFigs,paste0(variable,"_Cloud_woTitles.png")), width= width, height= height)
