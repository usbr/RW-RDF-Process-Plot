zz <- zz_all %>%
  dplyr::filter(Variable == variable) 

# write.csv(zz,file.path(oFigs,paste0(title,"_stats.csv")))


# if (powtiers){
#   powelltiers <- read.csv(file.path(getwd(),"data", "PowellTiers.csv"),header = T)
# }

# gg <- ggplot(zz, aes(x=Year, y=Mean, color=Scenario, group=Scenario)) +  theme_light()  #this is just a blank grided plot
gg <- ggplot(zz, aes(x=Year, y=Med, color=Scenario, group=Scenario)) +  theme_light()  #this is just a blank grided plot

# #DEBUG ##### PLOTING ISSUE 
# head(zz)
# unique(zz$Scenario)
# gg + geom_line(size=Medians)
# gg + geom_ribbon(data = subset(zz,Scenario %in% cloudScen),aes(ymin=Min, ymax=Max, fill = Scenario),  #CF: need to subset so don't create for mean
#                         alpha = 0.3, linetype = 2, size = 0.5*Medians)

# Generate plot a to make ribbon legend
name <- str_wrap("10th, 90th percentile",20)
gga <- gg + geom_ribbon(data = subset(zz,Scenario %in% cloudScen),aes(ymin=Min, ymax=Max, fill = Scenario),  #CF: need to subset so don't create for mean 
                        alpha = 0.3, linetype = 2, size = 0.5*Medians) + 
  scale_fill_manual(name, 
                    values = plotColors, guide = guide_legend(order=1),
                    labels = str_wrap(cloudLabs[], 15)) + scale_color_manual(name,
                                                                             values = plotColors, guide = guide_legend(order=1),
                                                                             labels = str_wrap(cloudLabs, 15))  +
  theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
        legend.box.margin = margin(0,0,0,0)) 
# gga
legenda <- get_legend(gga)

# # Generate plot b to take medians legend
if (exists("MinMaxLines") && MinMaxLines == T){ # T is want dotted line as min max of any given trace 
  lengendtitle <- "Min, Mean, Max" #
} else {
  # lengendtitle <- "Mean"
  lengendtitle <- "Median"
}


ggb <- gg + geom_line(size=Medians) + 
  scale_color_manual(name = str_wrap(lengendtitle,20),
                     values = plotColors) + #, labels = str_wrap(histLab, 15)) +
  theme(legend.text = element_text(size=LegendText),legend.title = element_text(size=LegendLabText, face="bold"),
        legend.box.margin = margin(0,0,0,0)) 
# ggb
legendb <- get_legend(ggb)

# Make legend grob.  4 rows used to make legend close together and in the middle with respects to the vertical
gglegend <- plot_grid(NULL, legenda,legendb, NULL, align = 'hv', nrow=4) #nrows=4


# Generate plot
ggc <- gg + 
  
  #geom_vline(xintercept=2020, size = IGStartLine, color = '#707070') + 
  
  geom_ribbon(data = subset(zz,Scenario %in% cloudScen),aes(ymin=Min, ymax=Max, fill = Scenario),  #CF: need to subset so don't create for mean 
              alpha = 0.3, linetype = 2, size = 0.5*Medians) +
  geom_line(size=Medians) +
  scale_fill_manual(str_wrap("10th to 90th percentile of full range",20),
                    values = plotColors, guide = "none", #It is deprecated to specify `guide = FALSE` 
                    labels = str_wrap(cloudLabs, 15)) + 
  scale_color_manual(name = str_wrap(lengendtitle,20),
                     values = plotColors, guide = "none") +#, #It is deprecated to specify `guide = FALSE` 
                     # labels = str_wrap(histLab, 15)) +
  labs(y = y_lab, title = title, x = 'Year') +# +,subtitle = subtitle) + 
  
  scale_x_continuous(minor_breaks = 1990:3000, breaks = myXLabs,
                     labels = myXLabs, expand = c(0,0)) +
  # scale_y_continuous(minor_breaks = seq(300,9000,25),
  #                    breaks = myYLabs, labels = comma) +
  
  theme(plot.title = element_text(size = TitleSize),
        ## axis.text.x = element_text(size = AxisLab),
        axis.text.y = element_text (size =AxisLab),
        axis.title = element_text(size=AxisText),
        panel.grid.minor = element_line(size = GridMin),
        panel.grid.major = element_line(size = GridMaj)) #+
  
  # guides(fill = "none") #guides(fill=FALSE) is depreciated #+

  # if (powtiers){
  #   ggc <-       ggc + 
  #   geom_hline(aes(yintercept=UEB), data=powelltiers, color = "black", lty = 3,size = 1) +
  #   geom_abline(slope = 1.2542,intercept = 1124.4,color = "black", lty = 3,size = 1) + #this is linear reg for 2020-2040 line, R2 = .99
  #   # geom_line(aes(x=Year,y=EQ), data=powelltiers, color = "red", lty = 2) +
  #   geom_hline(aes(yintercept=MER), data=powelltiers, color = "black", lty = 3,size = 1) +
  #   annotate("text", x = 2022, y = 3600, label = "Upper Elevation Balancing Tier",size = 3,hjust = 0) +
  #     annotate("text", x = 2022, y = 3550, label = "Mid Elevation Release Tier",size = 3,hjust = 0) + 
  #   annotate("text", x = 2022, y = 3450, label = "Lower Elevation Balancing Tier",size = 3,hjust = 0)
  # }

if (exists("NumCrit") && !is.na(NumCrit)){
  ggc <- ggc +
  #Adding lines for numeric criteria
  geom_hline(aes(yintercept=yintercept), data=NumCrit, color = "red", lty = 2) #+
}

#use && and only evaluate the first statement before proceeding 
if (exists("HistMin") && !is.na(HistMin)){
  ggc <- ggc +
    #Adding lines for historic min and max 
    geom_hline(aes(yintercept=yintercept), data=HistMin, color = "red", lty = 3) +
    geom_hline(aes(yintercept=yintercept), data=HistMax, color = "red", lty = 3) #+
}


if (exists("MinMaxLines") && MinMaxLines == T){
  ggc <- ggc + 
    geom_line(data = zz, aes(x=Year, y=MinOut, color=Scenario, group=Scenario),linetype = "dotted") +  
    geom_line(data = zz, aes(x=Year, y=MaxOut, color=Scenario, group=Scenario),linetype = "dotted")   
}

ggc

#final plot configuration 
gg <- plot_grid(ggc, gglegend, rel_widths = c(2,.4))

print(gg) #do print it for now, just assign gg in and print in your main code

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

ggsave(filename = file.path(fig_dir,paste0(variable,".png")), width= width, height= height)
